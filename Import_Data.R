# If simulation writes files to the test_files, 
# we can simplify this by a lot
if ( .Platform$OS.type == "unix"){
  sep <- '/'
}else{
  #need for files
  sep <- '\\'
}

tF <- paste(sep,"test_files",sep, sep = "")

mu <- read.csv(
  paste(tF,"mu.csv"), 
  sep=",", header=T, check.names = FALSE)%>%
  filter(cntry == country & age %in% age_to_evaluate) %>%
  select(cntry, age, all_of(years_to_evaluate))

country_cov<- list( cntry=rep(country, 3), "age"= c(1:3)) %>%
  data.frame(country_cov)

coverage<- read.csv(
  paste(tF,"VXcoverage.csv"), 
  sep=",", header=T, check.names = FALSE)%>%
  filter(cntry == country & del_platform == "routine") %>%
  select(cntry, all_of(years_to_evaluate)) %>%
  rbind(coverage, data_cov, fill= T)

incidence<- read.csv(
  paste(tF,"incidence.csv"),
                     sep=",", header=T, check.names = FALSE)%>%
  filter(cntry == country & age %in% age_to_evaluate) %>%
  select(cntry, age, all_of(years_to_evaluate))

pop<- read.csv(
  paste(tF,"population.csv"), 
               sep=",", header=T, check.names = FALSE)#%>%
filter(cntry == country & age %in% age_to_evaluate) %>%
  select(cntry, age, all_of(years_to_evaluate))

model_output<- read.csv(
  paste(tF,"detailed_view_by_year_of_impact_2022_09_19_14_11_30.csv"),
                        sep=",", header=T, check.names = FALSE)
