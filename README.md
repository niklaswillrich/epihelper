
# In development 
This package is under development.

# Aim of the package epietr
This package supports the European programme for intervention epidemiology training (EPIET) of the European Centre for Disease Prevention and Control (ECDC). 

Functions help to identify the source of outbreaks and communicate results as well as enter data of questionnaires. 

# Most important functions

* plotEpicurve: This function generates an epicurve designed according to EPIET standards
* generateOutbreak: This function generates a sample of outbreak data
* enterData: This is an example of how to enter questionnaire data. It can be adapted to different scenarios
* calcRelativeRisk: This funcion calculates relative risk 


# Contributers
This package wants to be improved by all persons affiliated with the programm including th Epiet Alumni Network (EAN)

# Installation
After installing R and R-Studio type into the R prompt the following two lines:
install.packages("devtools")
devtools::install_github("jakobschumacher/epietr")
require("epietr")
