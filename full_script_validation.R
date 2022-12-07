## IPM MODEL VALIDATION 
library("dplyr")
library("tidyr")
library("plyr")
library("data.table")

setwd<- ("/Users/anitapereda/Documents/Aut2022/HMS_Final/Data/")
in_dir<- '/Users/anitapereda/Documents/Aut2022/HMS_Final/Data/'
#to conduct this validation set parameters for data extraction below: country, age, years
country <- "KEN"
age_range <- c(0:4)
year_range <- as.character(c(2023:2028))

#valuation specific parameters
years_to_evaluate<- as.character(c(2023:2025))
age_to_evaluate<- c(0:2)
effic<- 0.85
parameters_to_evaluate<- c("cntry", "metric", as.character(2023:2028))
#scenario <- "Full_ctry_scope_routine_and_full_ctry_scope_campaign"

## Import dataframes: mortality rate, pop, coverage, incidence and filter by set parameters

mu <- read.csv(paste0(in_dir,"mortality.csv"), sep=",", header=T, check.names = FALSE) %>%
  select(cntry, age, all_of(years_to_evaluate))%>% 
  filter(cntry == country & age %in% age_to_evaluate)

coverage<- read.csv(paste0(in_dir,"coverage.csv"), sep=",", header=T, check.names = FALSE)%>%
  select(cntry, age, all_of(years_to_evaluate)) %>%
  filter(cntry == country & age %in% age_to_evaluate)

incidence<- read.csv(paste0(in_dir,"incidence.csv"), sep=",", header=T, check.names = FALSE)%>%
  select(cntry, age, all_of(years_to_evaluate)) %>%
  filter(cntry == country & age %in% age_to_evaluate)
  

pop<- read.csv(paste0(in_dir, "population.csv"), sep=",", header=T, check.names = FALSE)%>%
  select(cntry, age, all_of(years_to_evaluate)) %>%
  filter(cntry == country & age %in% age_to_evaluate) 

model_output<- read.csv(paste0(in_dir, "Model_Output.csv"), sep=",", header=T, check.names = FALSE)

# Make longer datasets and join by country, age and year
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

 

# ---------------------- Calculations and create tests ---------------------------------------
## Each calculation is a row of the the function

## Calculation 1: FVP

# fully vaccinated operator
#fully_vacinated1YO<- pop[["2028"]][1 +1]*((coverage[["2028"]][1 +1])/100)

##using data table calculate all FVPs
measure_dt <- measure_dt[, Fully_Vaccinated_People := population * (coverage/100)]



## Calculation 2: Fully protected 1 YO
#operator
#protected_1YO<- round(fully_vacinated1YO*effic, 0)


#using datatable to calculate 1 YO protected by multiplying full vac and effic
measure_dt <- measure_dt[, protected1YO := Fully_Vaccinated_People * effic]

#Calculation 3: Fully protected 2 YO
## figuring out a loop to do the validation 
## 1- mortality rate of the previous year (same age) multiplied by protect 1 YO of that year. 2YO and 3YO are zero.
## Shift mortality my adding one year and join tables by: ????

#operator
#protected_2YO <- round((1- mu$"2028"[2-1+1])*protected_1YO, 0)

measure_dt <- measure_dt[, lag.mu := lag(mu)]
measure_dt <- measure_dt[, lag.P1YO := lag(protected1YO)]


measure_dt <- measure_dt[, protected2YO := (1- lag.mu)*lag.P1YO]

#Calculation 4: Fully protected 3 YO 

#1- mortality rate of the previous year (same age) multiplied by protect 2 YO of that year
#operator

#protected_3YO<- round((1- mu$'2029'[3-1+1])*protected_2YO,0)

measure_dt <- measure_dt[, lag.P2YO:= lag(protected2YO)]


measure_dt <- measure_dt[, protected3YO := (1- lag.mu)*lag.P2YO]

## FVP and full protected years-- year summation
#set age and year range should be user defined values 
interest_values <- measure_dt[age == 0, ][,list(Year, Fully_Vaccinated_People, protected1YO, protected2YO, protected3YO)]

#extract full vaccinated population

full_vaccinated <- interest_values %>%
  select(Fully_Vaccinated_People)

interest_values<- interest_values[, 'Fully_Vaccinated_People' :=NULL]


#sum of protected population grouped by year 
protected_pop <- interest_values[, Protected_Population := rowSums(.SD, na.rm = TRUE), .SDcols= 2:4]

protected_per_year <- protected_pop%>%
  select(5)

##---------- CASES AVERTED -----------------------------------------------------

## Calculations: The result is for three years by age; Protected 1YO/(pop 1 YO * inc 1YO)

measure_dt<- measure_dt[, ':='('cases_averted0YO' = (protected1YO/population) * incidence,
                               'cases_averted1YO' = (protected2YO/population)*incidence,
                               'cases_averted2YO' = (protected3YO/population)*incidence)]



# approach: datable calclulation; pivot table by cases averted each year and sum for total population

cases_averted <- measure_dt[age == 0 , ][,list(Year, cases_averted0YO, cases_averted1YO , cases_averted2YO)]

calculated_model_output<- cases_averted[, Cases_Averted := rowSums(.SD, na.rm = TRUE), .SDcols= 2:4] %>%
  select(1,5) %>%
  cbind(full_vaccinated, protected_per_year) %>%
  pivot_longer(
    cols= c(2:4),
    names_to= 'measure',
    values_to = 'values'
  )%>%
  pivot_wider(
    names_from= 'Year',
    values_from = 'values'
  )




#create final data table for comparison


#----------- COMPARE WITH DATA FROM MODEL OUTPUT -------------------------------
#create one data frame with results of four calculation ordered by year and sum

## load model output 


#select columns: country, year, "Case Averted, "Fully Vaccinated People" and "Protected Population"
#select rows by defined values: country, year
# pivot longer columns to measures, values to values
# dcast measure to year, value var to vals


# comparison: comparef() [library (arsenal)]

#-------------------------------------------------------------------------------







