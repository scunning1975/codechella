#---------------------------------------------------
#      Empirical Exercise based on Cheng and Hoekstra (2013, JHR)
#      Effect of "Castle Doctrine" law on homicides and violent crime
#---------------------------------------------------
#-----------------------------------------------------------------------------

# Load packages
#-----------------------------------------------------------------------------
# Libraries
library(haven)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(bacondecomp) 
library(TwoWayFEWeights)
library(fixest)
library(glue)

#---------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------
# Load Cheng and Hoekstra (2013, JHR) Castle Data
castle <- haven::read_dta("https://github.com/scunning1975/mixtape/raw/master/castle.dta")
#-----------------------------------------------------------------------------
# Do some data manipulations
# replace NA treatment_date with Inf
castle$treatment_date = ifelse(is.na(castle$treatment_date), Inf, castle$treatment_date)
# Create treatment dummy: 1 if treated by that year, 0 otherwise
castle$treated <- as.numeric(castle$year >= castle$treatment_date)
#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
# Start the analysis
#---------------------------------------------------------------------------------------
# Get TWFE coefficient
twfe <- fixest::feols(l_homicide ~ treated| sid + year, 
                      data = castle,
                      cluster = ~sid)

summary(twfe)
#---------------------------------------------------------------------------------------
# Get Bacon decomposition (Just for comparison)
df_bacon <- bacon(l_homicide ~ treated,
                  data = castle,
                  id_var = "sid",
                  time_var = "year")
#---------------------------------------------------------------------------------------
# Get de Chaisemartin and D'Haultfoeuille (dCDH) Decomposition
dCDH_decomp <- twowayfeweights(
  df = castle, 
  Y = "l_homicide", 
  G = "sid",
  T = "year", 
  D ="treated",
  #weights = "W",
  cmd_type =  "feTR"
)

# Weakly Positive weights
dCDH_positive <- sum(dCDH_decomp$weight[dCDH_decomp$weight>=0])

# Negative weights
dCDH_negative <- sum(dCDH_decomp$weight[dCDH_decomp$weight<0])
#---------------------------------------------------------------------------------------
