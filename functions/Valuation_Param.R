# Valuation parameters ----------------------------------------------------
{
country <- toupper(readline(prompt = "3 letter Country abreviation (ex: AGO) "))
sYear <- as.integer(readline(prompt = "Start year of interest "))
eYear <- as.integer(readline(prompt = "End year of interest "))
initial_age <- as.integer(readline(prompt = "Start age of interest "))
final_age <- as.integer(readline(prompt = "End age of interest "))
scenario<- readline(prompt = "Scenario (tpp_min / ...) ")
market_outcome <- readline(prompt = "Scenario (Trivalent NTS-TCV, SoC/ ...) ")
effic<- as.double(readline(prompt = "Efficacy of coverage (ex: 0.8) "))
dur_prot <- as.double(readline(prompt = "Duration of Protection (years) "))
}
years_to_evaluate<- as.character(c(sYear:eYear))
age_to_evaluate<- c(as.numeric(initial_age):as.numeric(final_age))

param <- list(country, years_to_evaluate, age_to_evaluate,
              scenario, market_outcome, effic, dur_prot)

paramN <- list("Country", "Years to Evaluate", "Ages to Evaluate",
               "Scenario", "Market Outcome", "Efficacy", 
               "Duration of protection")

testP <- do.call(rbind, 
                 Map(data.frame, `Parameter of Interest`=paramN, Value=param))


