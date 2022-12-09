### FUNCTIONS FOR VALIDATION SCRIPT ####
library("dplyr")
library("tidyr")
library("plyr")
library("data.table")
library(svDialogs)
##### Parameters ####

## Could add error check here ...
{
  country <- toupper(dlgInput("3 letter Country abreviation (ex: AGO) ",
                              "KEN/AGO/ZWE")$res)
  # initial_age <- as.integer(dlgInput("Start age of interest ",
  #                             0)$res)
  final_age<- as.integer(dlgInput("Ages 0 - ",
                                     4)$res)
  # sYear<- as.integer(dlgInput("Start year of interest ",
  #                                 2023)$res)
  eYear<- as.integer(dlgInput("From 2023 -  ",
                              2027)$res)
  effic<- as.double(dlgInput("Efficacy of coverage (ex: 0.8) ",
                              0.8)$res)
  #plots<- tolower(dlgInput("Graphs to be made. Seperate with a comma ","scatter,bar,line")$res)
}

#If we want to add list of ggplots to be made
#as.list(strsplit(x, ",")


years_to_evaluate<- as.character(c(2023:eYear))
age_to_evaluate<- c(0:as.numeric(final_age))

param <- list(country, years_to_evaluate, age_to_evaluate, effic)

paramN <- list("Country", "Years to Evaluate", "Ages to Evaluate", "Efficacy")

testP <- do.call(rbind, 
                 Map(data.frame, `Parameter of Interest`=paramN, Value=param))


curwd <- getwd()
##### Import Data ####

if ( .Platform$OS.type == "unix"){
  sep <- '/'
}else{
  #need for files
  sep <- '\\'
}

tF <- paste(curwd,sep,"Data",sep, sep = "")
keepcol <- c("cntry", "age", years_to_evaluate)
mocol <- c("metric", years_to_evaluate)
mupath <- gsub(" ","", paste(tF,"mu.csv"))
covpath <- gsub(" ","", paste(tF,"coverage.csv"))
incpath <- gsub(" ","", paste(tF,"incidence.csv"))
poppath <- gsub(" ","", paste(tF,"population.csv"))
mopath <- gsub(" ","", paste(tF,"Model_Output.csv"))

## Mortality
mu <- read.csv(
  mupath, 
  sep=",", header=T, check.names = FALSE)
mu <- mu[mu$cntry == country & mu$age %in% age_to_evaluate,keepcol]
##

## Coverage 
coverage<- read.csv(covpath, sep=",", header=T, check.names = FALSE)
coverage <- coverage[coverage$cntry == country & 
                       coverage$age %in% age_to_evaluate, keepcol]
##

## Incidence
incidence<- read.csv(incpath, sep=",", header=T, check.names = FALSE)
incidence <- incidence[incidence$cntry == country & 
                       incidence$age %in% age_to_evaluate, keepcol]

## Population
pop<- read.csv(poppath, sep=",", header=T, check.names = FALSE)
pop <- pop[pop$cntry == country & 
                         pop$age %in% age_to_evaluate, keepcol]
##

## Model_Output
model_output<- read.csv(mopath, 
                        sep=",", header=T, check.names = FALSE)
model_output<- model_output[model_output$cnty == country, mocol]

model_output <- setorder(model_output, cols = "metric")
##


#### Pivot Longer ####

TransL <- function(dt) {
  if(deparse(substitute(dt)) != "pop"){
    pivot_longer(
      dt, 
      cols = years_to_evaluate,
      names_to= "Year",
      values_to= deparse(substitute(dt))
    )
  }else{
    pivot_longer(
      dt, 
      cols = years_to_evaluate,
      names_to= "Year",
      values_to= "population"
    )
  }
}

mu_longer <- TransL(mu)
coverage_longer <- TransL(coverage)
pop_longer <- TransL(pop)
incidence_longer <- TransL(incidence)

measure_df <- join_all(list(mu_longer, coverage_longer, incidence_longer, pop_longer),
                       by= c('cntry','age','Year'), 
                       type="left")

measure_dt <- as.data.table(measure_df)

rm(coverage,coverage_longer,incidence,incidence_longer,measure_df,mu,mu_longer,
   param, paramN, pop, pop_longer)


#### Calculations ####

prepData <- function(x,y){
  x <- x[, `fully vaccinated people` := population * (coverage/100)]
  x <- x[, protected1YO := `fully vaccinated people` * effic]
  x <- x[, lag.mu := lag(mu)]
  x <- x[, lag.P1YO := lag(protected1YO)]
  x <- x[, protected2YO := (1- lag.mu)*lag.P1YO]
  x <- x[, lag.P2YO:= lag(protected2YO)]
  x <- x[, protected3YO := (1- lag.mu)*lag.P2YO]
  x <- x[, lag.P3YO:= lag(protected3YO)]
  x <- x[, protected4YO := (1- lag.mu)*lag.P3YO]
}

