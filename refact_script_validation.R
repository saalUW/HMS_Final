## IPM MODEL VALIDATION 
library("dplyr")
library("tidyr")
library("plyr")
library("data.table")
library(arsenal)


source("functions.R")

print(testP)

# ---------------------- Calculations and create tests ------------------------
## Each calculation is a row of the the function

## could edit the list to make it be true to what is put in input

prepData(measure_dt)

# if(final_age == 1){
#   interest_values <- measure_dt[age == 0, ][,list(Year, Fully_Vaccinated_People,
#                                                   protected1YO)]
# }else if (final_age == 2){
#   interest_values <- measure_dt[age == 0, ][,list(Year, Fully_Vaccinated_People,
#                                                   protected1YO, protected2YO)]
# }else if (final_age == 3){
#   interest_values <- measure_dt[age == 0, ][,list(Year, Fully_Vaccinated_People,
#                                       protected1YO, protected2YO, protected3YO)]
# }else if (final_age == 4){
#   interest_values <- measure_dt[age == 0, ][,list(Year, Fully_Vaccinated_People,
#                         protected1YO, protected2YO, protected3YO, protected4YO)]
# }

#coi <- 
interest_values <- measure_dt[age == 0, ][,list(Year, `fully vaccinated people`,
                                                protected1YO, protected2YO, protected3YO)]



#extract full vaccinated population

full_vaccinated <- interest_values %>%
  select(`fully vaccinated people`)

interest_values<- interest_values[, 'fully vaccinated people' :=NULL]


#sum of protected population grouped by year 
protected_pop <- interest_values[, `protected population` 
                                 := rowSums(.SD, na.rm = TRUE), .SDcols= 2:4]

protected_per_year <- protected_pop%>%
  select(5)

##---------- CASES AVERTED -----------------------------------------------------

## Calculations: The result is for three years by age; Protected 1YO/(pop 1 YO * inc 1YO)

measure_dt<- measure_dt[, 
          ':='('cases_averted0YO' = (protected1YO/population) * incidence,
          'cases_averted1YO' = (protected2YO/population)*incidence,
          'cases_averted2YO' = (protected3YO/population)*incidence)]



# approach: datable calclulation; pivot table by cases averted each year 
#and sum for total population

cases_averted <- measure_dt[age == 0 , ][,
  list(Year, cases_averted0YO, cases_averted1YO , cases_averted2YO)]

calculated_model_output<- cases_averted[, 
  `cases averted` := rowSums(.SD, na.rm = TRUE), .SDcols= 2:4] %>%
  select(1,5) %>%
  cbind(full_vaccinated, protected_per_year) %>%
  pivot_longer(
    cols= c(2:4),
    names_to= 'metric',
    values_to = 'values'
  )%>%
  pivot_wider(
    names_from= 'Year',
    values_from = 'values'
  )


calculated_model_output <- setorder(calculated_model_output, cols = "metric")


#create final data table for comparison


#----------- COMPARE WITH DATA FROM MODEL OUTPUT -------------------------------
#create one data frame with results of four calculation ordered by year and sum

## load model output 


#select columns: country, year, "Case Averted, "Fully Vaccinated People" and "Protected Population"
#select rows by defined values: country, year
# pivot longer columns to measures, values to values
# dcast measure to year, value var to vals


cmp <- comparedf(model_output,calculated_model_output, by = "metric",
                  int.as.num = T )
summary(cmp)

diffs(cmp)
dif <- as.data.table(diffs(cmp))
colnames(dif)[which(names(dif) == "var.x")] <- "Year"
colnames(dif)[which(names(dif) == "metric")] <- "Metric"
colnames(dif)[which(names(dif) == "values.x")] <- "Model Value"
colnames(dif)[which(names(dif) == "values.y")] <- "Calculated Model Value"
dif <- dif[,list(Year, Metric,`Model Value`, `Calculated Model Value`)]


# comparison: comparef() [library (arsenal)]

#-------------------------------------------------------------------------------





