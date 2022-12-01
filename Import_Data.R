# If simulation writes files to the test_files, 
# we can simplify this by a lot

mu <- read.csv(
  "/test_files/mu.csv", 
  sep=",", header=T, check.names = FALSE)%>%
  filter(cntry == country & age %in% age_to_evaluate) %>%
  select(cntry, age, all_of(years_to_evaluate))

country_cov<- list( cntry=rep(country, 3), "age"= c(1:3)) %>%
  data.frame(country_cov)

coverage<- read.csv(
  "/test_files/VXcoverage.csv", 
  sep=",", header=T, check.names = FALSE)%>%
  filter(cntry == country & del_platform == "routine") %>%
  select(cntry, all_of(years_to_evaluate)) %>%
  rbind(coverage, data_cov, fill= T)


incidence<- read.csv("/test_files/incidence.csv",
                     sep=",", header=T, check.names = FALSE)%>%
  filter(cntry == country & age %in% age_to_evaluate) %>%
  select(cntry, age, all_of(years_to_evaluate))

pop<- read.csv("/test_files/population.csv", 
               sep=",", header=T, check.names = FALSE)#%>%
filter(cntry == country & age %in% age_to_evaluate) %>%
  select(cntry, age, all_of(years_to_evaluate))


model_output<- read.csv("/test_files/detailed_view_by_year_of_impact_2022_09_19_14_11_30.csv",
                        sep=",", header=T, check.names = FALSE)
