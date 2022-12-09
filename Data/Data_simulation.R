# Data simulation HMS 520

library (dplyr)
library (readr)
library(tidyr)

# DATA INPUTS -------------------------------------------------------------

#This should be the same than what we are going to be
#using for the code

cntry <- c("KEN", "AGO", "ZWE")
age<- c(0:4)
year <- c(2023:2028)

base_data<- expand.grid(cntry, age, year)
names(base_data) <- c("cntry", "age", "year")

# Converage incidence population and mortality ----------------------------

coverage<- 44
base_data$coverage <- ifelse(base_data$age == 0, coverage, 0)

# incidence: generate incidence for each country and age
set.seed(126)

set_parameter<- function(data, 
                          parameter = vector("integer", length = nrow(data)),
                          mean_distribution, sd_distribution){
  n= nrow(data)
  parameter =rnorm(n, mean=mean_distribution, sd= sd_distribution)
  return (parameter)
}

base_data$incidence<- set_parameter(base_data, incidence, 150,5.9)
base_data$population <- set_parameter(base_data, population, 1650900.54, 32000)
base_data$mortality<- set_parameter(base_data, mortality, 0.02, 0.001)
base_data$coverage <- ifelse(base_data$age == 0, coverage, 0)

# reshape data ------------------------------------------------------------

#Not running for me for some reason .... - Sameer
incidence <- base_data %>%
  select(c(1:3, "incidence")) %>%
  pivot_wider(names_from = year,
              values_from = incidence) %>%
  arrange(cntry)


population <- base_data %>%
  select(c(1:3),"population") %>%
  pivot_wider(names_from = year,
              values_from = population) %>%
  arrange(cntry)

mortality <- base_data %>%
  select(c(1:3), "mortality") %>%
  pivot_wider(names_from = year,
              values_from = mortality) %>%
  arrange(cntry)

coverage <- base_data %>%
  select(c(1:3), "coverage") %>%
  pivot_wider(names_from = year,
              values_from = coverage) %>%
  arrange(cntry)

#saving data
curwd <- getwd()
curwd <- gsub(" ","",paste(curwd,"/Data/"))

savecsv <- function(x){
  path <- gsub(" ","", paste(curwd, deparse(substitute(x))))
  write.csv(x, path)
}

savecsv(population)
savecsv(mortality)
savecsv(coverage)


write.csv(incidence, "/Users/anitapereda/Documents/Aut2022/HMS_Final/Data/incidence.csv")





