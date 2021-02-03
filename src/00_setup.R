# Tidy Data Project
# Andrea De Angelis
# 7th of June 2019

# This scripts sets up the data analysis' project:
# - installing (or loading) the needed packages 
# - sourcing the needed functions

# Credits: part of the code is from Kruschke (2014) Doing Bayesian Data
# Analysis. A Tutorial with R, JAGS, and Stan.
# browseURL("https://www.amazon.de/Doing-Bayesian-Data-Analysis-Tutorial/dp/0124058884")

# Installing packages ---------------------------------------------------------------------------------

want = c("here","stargazer","rio","lme4")   # list of required packages
have = want %in% rownames(installed.packages())
if ( any(!have) ) { install.packages( want[!have] ) }
rm(have, want)

# Attaching the needed packages (uncomment to run)
# lapply(want, library, character.only = TRUE)

# Sourcing needed functions ---------------------------------------------------------------------------
source(here::here("src","00_functions.R"))
