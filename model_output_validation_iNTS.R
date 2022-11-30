
rm(list = ls())

# Model Output Validation -------------------------------------------------


library("dplyr")
library("tidyr")
library("plyr")
library("data.table")

setwd<- ("/Users/anitapereda/Documents/START/IPM/Model_Output_Validation")


# The following script was written by Ana Pereda, the goal is to validate vaccine
# valuation model output of fully vaccinated population, protected population, and
# cases averted for the first 3 years after PQ. 

# Valuation parameters ----------------------------------------------------
# insert valauation parameters
country <- "AGO"
years_to_evaluate<- as.character(c(2030:2032))
age_to_evaluate<- c(0:4)
initial_age<- 0
scenario<-"tpp_min"
market_outcome <- "Trivalent NTS-TCV, SoC"
effic<- 0.8
dur_prot <- 5



# Import dataframes -------------------------------------------------------


parameters_to_evaluate<- c( "Scenario", "Year","Cases_Averted", 
                            "Protected_Population", "Fully_Vaccinated_People")

## Import dataframes: mortality rate, pop, coverage, incidence and filter by set parameters

mu <- read.csv("/Users/anitapereda/Documents/START/IPM/Model_Output_Validation/mu.csv", sep=",", header=T, check.names = FALSE)%>%
  filter(cntry == country & age %in% age_to_evaluate) %>%
  select(cntry, age, all_of(years_to_evaluate))

country_cov<- list( cntry=rep(country, 3), "age"= c(1:3)) %>%
  data.frame(country_cov)

coverage<- read.csv("/Users/anitapereda/Documents/START/IPM/Model_Output_Validation/iNTS/VXcoverage.csv", sep=",", header=T, check.names = FALSE)%>%
  filter(cntry == country & del_platform == "routine") %>%
  select(cntry, all_of(years_to_evaluate)) %>%
  rbind(coverage, data_cov, fill= T)


incidence<- read.csv("/Users/anitapereda/Documents/START/IPM/Model_Output_Validation/iNTS/incidence.csv", sep=",", header=T, check.names = FALSE)%>%
  filter(cntry == country & age %in% age_to_evaluate) %>%
  select(cntry, age, all_of(years_to_evaluate))

pop<- read.csv("/Users/anitapereda/Documents/START/IPM/Model_Output_Validation/mening/population.csv", sep=",", header=T, check.names = FALSE)#%>%
  filter(cntry == country & age %in% age_to_evaluate) %>%
  select(cntry, age, all_of(years_to_evaluate))


model_output<- read.csv("/Users/anitapereda/Documents/START/IPM/Model_Output_Validation/iNTS/detailed_view_by_year_of_impact_2022_09_19_14_11_30.csv", sep=",", header=T, check.names = FALSE)


# Input valuation transformations -----------------------------------------


mu_longer<- pivot_longer(
  mu, 
  cols = years_to_evaluate,
  names_to= "Year",
  values_to= "mu"
)

coverage_longer<- pivot_longer(
  coverage, 
  cols = years_to_evaluate,
  names_to= "Year",
  values_to= "coverage"
)

pop_longer<- pivot_longer(
  pop, 
  cols = years_to_evaluate,
  names_to= "Year",
  values_to= "population"
)

incidence_longer<- pivot_longer(
  incidence, 
  cols = years_to_evaluate,
  names_to= "Year",
  values_to= "incidence"
)

measure_df <- join_all(list(mu_longer, coverage_longer, incidence_longer, pop_longer),
                       by= c('cntry','age','Year'), 
                       type="left")
# convert dataframe to datatable
measure_dt <- as.data.table(measure_df)


# Output valuation transformations ----------------------------------------

names(model_output) <- gsub(" ", "_", names(model_output))
model_output <- model_output%>%
  filter(Scenario == "tpp_min" & Year %in% years_to_evaluate & Country== country & Market_Outcome == market_outcome)%>%
  select(all_of(parameters_to_evaluate))

setDT(model_output)
model_output <- melt(
  model_output,
  id.vars=  "Year",
  measure.vars = c("Cases_Averted", "Protected_Population", "Fully_Vaccinated_People"),
  variable.name= "measure",
  value.name = "values") 

model_output <- dcast(
  model_output,
  measure ~ Year, 
  value.var = "values"
)


# Model calculations ------------------------------------------------------


measure_dt <- measure_dt[, full_vac := population * (coverage/100)]


measure_dt <- measure_dt[, protected1YO := full_vac * effic]

measure_dt <- measure_dt[, lag.mu := lag(mu)]
measure_dt <- measure_dt[, lag.P1YO := lag(protected1YO)]

measure_dt <- measure_dt[, protected2YO := (1- lag.mu)*lag.P1YO]

measure_dt <- measure_dt[, lag.P2YO:= lag(protected2YO)]


measure_dt <- measure_dt[, protected3YO := (1- lag.mu)*lag.P2YO]

## FVP and full protected years-- year summation

interest_values <- measure_dt[,list(Year, full_vac, protected1YO, protected2YO, protected3YO)]

#extract full vaccinated population

full_vaccinated <- interest_values$full_vac
interest_values<- interest_values[, 'full_vac' :=NULL]


interest_values<-  melt(
  interest_values,
  id.vars= "Year",
  measure.vars = c("protected1YO", "protected2YO", "protected3YO"),
  variable.name= "measure",
  value.name = "values"
)
#sum of protected population grouped by year 
protected_pop<- interest_values[, 
                                .(protectec_per_year= sum(values, na.rm= TRUE)),
                                by= Year]
protectec_per_year <- protected_pop$protectec_per_year

#organizes the values in a table similar to the excel
interest_values<- dcast(
  interest_values, 
  measure~ Year, 
  value.var ="values"
)




##---------- CASES AVERTED -----------------------------------------------------

## Calculations: The result is for three years by age; Protected 1YO/(pop 1 YO * inc 1YO)

measure_dt<- measure_dt[, ':='('cases_averted1YO' = (protected1YO/population) * incidence,
                               'cases_averted2YO' = (protected2YO/population)*incidence,
                               'cases_averted3YO' = (protected3YO/population)*incidence)]



# approach: datable calclulation; pivot table by cases averted each year and sum for total population

cases_averted <- measure_dt[age == 1 & Year %in% c(2028:2030), ][,list(Year, cases_averted1YO, cases_averted2YO , cases_averted3YO)]
cases_averted<-  melt(
  cases_averted,
  id.vars= "Year",
  measure.vars = c("cases_averted1YO", "cases_averted2YO" , "cases_averted3YO"),
  variable.name= "measure",
  value.name = "values"
)

cases_averted_pop<- cases_averted[, 
                                .(cases_averted_per_year= round(sum(values, na.rm= TRUE),0)),
                                by= Year]

cases_averted_per_year <- cases_averted_pop$cases_averted_per_year


full_vaccinated_dt<-data.table(data.frame("Year"=c(2028:2030),full_vaccinated))

calculations <- rbind(cases_averted_pop,protected_pop,full_vaccinated_dt, fill=TRUE) 
calculations<-  melt(
  calculations,
  id.vars= "Year",
  measure.vars = c('cases_averted_per_year','protectec_per_year','full_vaccinated'),
  variable.name= "measure",
  value.name = "values"
) 
calculations<- dcast(
  na.omit(calculations), 
  measure~ Year, 
  value.var = "values"
)


# Compare data with model output ------------------------------------------







