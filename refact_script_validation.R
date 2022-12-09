## IPM MODEL VALIDATION 
library("dplyr")
library("tidyr")
library("plyr")
library("data.table")
library(arsenal)

source("functions.R")

print(testP)

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
interest_values <- measure_dt[age == 0, ][,
  list(Year, Fully_Vaccinated_People, protected1YO, protected2YO, protected3YO)]

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


calculated_model_output <- setorder(calculated_model_output, cols = "measure")


#create final data table for comparison


#----------- COMPARE WITH DATA FROM MODEL OUTPUT -------------------------------
#create one data frame with results of four calculation ordered by year and sum

## load model output 


#select columns: country, year, "Case Averted, "Fully Vaccinated People" and "Protected Population"
#select rows by defined values: country, year
# pivot longer columns to measures, values to values
# dcast measure to year, value var to vals


summary(comparedf(model_output,calculated_model_output))
# comparison: comparef() [library (arsenal)]

#-------------------------------------------------------------------------------







