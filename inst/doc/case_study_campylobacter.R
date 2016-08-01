## ---- warning=FALSE, message=FALSE, error=FALSE, comment=FALSE, echo=FALSE----
# Set knitr options to not evaluate anything
knitr::opts_chunk$set(eval=FALSE, warning=FALSE, message=FALSE, error=FALSE, comment=FALSE)

## ------------------------------------------------------------------------
#  # Set the correct directory
#  #setwd("~/Documents/Projekte/epihelper")
#
#  # Initalize the epihelper package
#  library(epihelper)
#
#  # Load data
#  load(campy)

## ------------------------------------------------------------------------
#  str(campy)

## ------------------------------------------------------------------------
#  campy$diluted <- ifelse(campy$concentrated==1 | campy$powder==1, 1, 0)
#
#  # stata commands
#  # generate diluted = 1 if concentrated==1 | powder==1
#  # replace diluted = 0 if concentrated==0 & powder==0

## ------------------------------------------------------------------------
#  tabulate case
#  bysort case: summarize age, detail
#  To test whether the age distribution is normal among both cases and controls, you can run:
#  bysort case: swilk age //Shapiro-Wilk test1 for both cases and controls swilk age if case==1 //Shapiro-Wilk test for cases
#  swilk age if case==0 //Shapiro-Wilk test for controls
#  You can also visualise the distribution of age among cases and controls:
#  histogram age if case==1, frequency
#  histogram age if case==0, frequency
#  Age does not appear to be very normally distributed.
#  Now that you are absolutely sure that the hypothesis of normality in the variable age is not really the case, you choose to run Wilcoxon’s ranksum test2:
#  ranksum age, by(case)
#  If you had gone for the t-test, the command would have been:
#  ttest age, by(case)
#

## ------------------------------------------------------------------------
#  plotEpicurve()

## ------------------------------------------------------------------------
#  # cc case breastfeeding
#  # cc case dishwasher
#  # cc case supply
#  # cctable case supply
#  # cctable case tap-diluted
#  # cctable case tap-diluted, or
#  #  the latter command runs cctable for all variables between tap and diluted in the dataset.  the option or in the cctable command sorts the results by the odds ratio.
#  # Another way to see whether there is an association between two dichotomous variables is, of course, the χ2 test. This does not give you the odds ratio, though.
#  # tab case bottled, chi2
#  #  Adding the options col or row, which would provide you the percentages by column or row
#  # respectively, can help you identify the direction of the association.
#  # The results (odds ratios and 95% confidence intervals) for the univariate analysis are shown in Table 5 on the next page.
#

