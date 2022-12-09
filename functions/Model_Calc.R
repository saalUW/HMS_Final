#Write a function that does all this to the dataset so
library("dplyr")
library("tidyr")
library("plyr")
library("data.table")

Model_Calc <- function(dt) {
  dt <- mutate(dt,
               full_vac = population * (coverage/100),
               protected1YO = full_vac * effic,
               lag.mu = lag(mu),
               lag.P1YO = lag(protected1YO),
               protected2YO = (1- lag.mu)*lag.P1YO,
               lag.P2YO = lag(protected2YO),
               protected3YO = (1- lag.mu)*lag.P2YO
               )
  #return(dt)
}
