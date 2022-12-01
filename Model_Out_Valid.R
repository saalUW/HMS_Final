
#rm(list = ls())

# Model Output Validation -------------------------------------------------
library("dplyr")
library("tidyr")
library("plyr")
library("data.table")

directory <- readline(prompt = "Path to parent directory ")
setwd(directory)


# The following script was written by Ana Pereda, the goal is to validate 
#vaccine valuation model output of fully vaccinated population, 
#protected population, and cases averted for the first 3 years after PQ. 

# Valuation parameters ----------------------------------------------------
source("Valuation_Param.R")
print("Current Test Parameters: ")
testP

# Import dataframes -------------------------------------------------------

source("Import_Data.R")
parameters_to_evaluate<- c( "Scenario", "Year","Cases_Averted", 
                            "Protected_Population", "Fully_Vaccinated_People")

## Import dataframes: mortality rate, pop, coverage, incidence 
#and filter by set parameters

# Input valuation transformations -----------------------------------------

source("TransformationL.R")
mu_longer <- TransL(mu)
coverage_longer <- TransL(coverage)
pop_longer <- TransL(pop)
incidence_longer <- TransL(incidence)

measure_df <- join_all(list(mu_longer, coverage_longer, incidence_longer, pop_longer),
                       by= c('cntry','age','Year'), 
                       type="left")

# convert dataframe to datatable
measure_dt <- as.data.table(measure_df)

# Output valuation transformations ----------------------------------------

names(model_output) <- gsub(" ", "_", names(model_output))
model_output <- model_output%>%
  filter(Scenario == "tpp_min" & Year %in% years_to_evaluate & 
           Country== country & Market_Outcome == market_outcome)%>%
  select(all_of(parameters_to_evaluate))

setDT(model_output)
model_output <- melt(
  model_output,
  id.vars=  "Year",
  measure.vars = c("Cases_Averted", "Protected_Population", 
                   "Fully_Vaccinated_People"),
  variable.name= "measure",
  value.name = "values") 

model_output <- dcast(
  model_output,
  measure ~ Year, 
  value.var = "values"
)

# Model calculations ------------------------------------------------------

source("Model_Calc.R")
 Model_Calc(measure_dt)

## FVP and full protected years-- year summation

interest_values <- measure_dt[,list(Year, full_vac, protected1YO, 
                                    protected2YO, protected3YO)]

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

## Calculations: The result is for three years by age; 
#Protected 1YO/(pop 1 YO * inc 1YO)

measure_dt<- measure_dt[, ':='('cases_averted1YO' = 
                                 (protected1YO/population) * incidence,
                               'cases_averted2YO' = 
                                 (protected2YO/population)*incidence,
                               'cases_averted3YO' = 
                                 (protected3YO/population)*incidence)]



# approach: datable calclulation; pivot table by cases averted each year 
#and sum for total population

cases_averted <- measure_dt[age == 1 & Year %in% c(2028:2030), ][,
                                                                 list(Year, cases_averted1YO, cases_averted2YO , cases_averted3YO)]
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

calculations <- rbind(cases_averted_pop,protected_pop,
                      full_vaccinated_dt, fill=TRUE) 
calculations<-  melt(
  calculations,
  id.vars= "Year",
  measure.vars = c('cases_averted_per_year','protectec_per_year',
                   'full_vaccinated'),
  variable.name= "measure",
  value.name = "values"
) 
calculations<- dcast(
  na.omit(calculations), 
  measure~ Year, 
  value.var = "values"
)


# Compare data with model output ------------------------------------------







