# HMS_Final: Model Validation #

## General Info:
* Code Refactoring: Model Output Valuation  
* Team members: Sameer Ali, Ana Pereda
* Format: Refactoring a piece of existing code (Option 2)

## Overall Goal: 
To create an R script that simulates a population, disease incidence, mortality data and vaccine coverage and then automates validation of model valuation 

## Detailed Plans and timeline:
30/11/22
Submit proposal/ discuss structure

3/12/22
Data Simulation

8/12/22
Code Refactoring

~ Time permitting ~ 
CLI / Graph output / Final validation report output

12/12/22
Present

14/12/22
Submit

# How to Run #

## Simulation ##
If running a simulation, run the Simulation.R script which will autopopulate the test_files

## Test Files ##
If including preprocessed data, add the following files:
* mu.csv = Mortality Rate
* coverage.csv = Vaccine Coverage
* incidence.csv = Incidence
* population.csv = Population
* model_out.csv = Model Output

## Running With Simulation ##

Within the "Data" file there is a simulation.R script that will create the necessary .csv for data validation.  

## Running refact_script_validation ##

With the simulation data prepared and correctly named, the validation can be executed by running the refact_script_validation.R script.
This script will create a gui for inputting the model parameters, and calls functions.R for data reshapping. refact_script_validation.R will
output a dataframe of the comparison between the expected and calculated models. This dataframe can be used for making plots, or can be otherwise outputted (not implemented).


