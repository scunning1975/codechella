###################################################################################
# Program:       Effect of castle doctrine reform on log homicides
# Author:        Scott Cunningham
# Affiliation:   Baylor University
# Created:       5/7/2021
# Date Modified: 6/7/2021
# Modified by:   Scott Cunningham and Grant McDermott
###################################################################################


# Libraries ---------------------------------------------------------------

## Install (if necessary) and load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, haven, gsynth, panelView)


# Data --------------------------------------------------------------------

# Read in the castle dataset from the web
castle = read_dta('https://github.com/scunning1975/mixtape/raw/master/castle.dta')

## Untreated units (currently "NA") should have an effective year of 0
castle$effyear = replace_na(castle$effyear, 0)

# Quick look at the data. We have a balanced panel with 21 treated units (i.e.
# states) and variable treatment timing
panelView(l_homicide ~ post, data = data.frame(castle),
  index = c('sid', 'year'), pre.post = TRUE, by.timing = TRUE)

# Similar, but in tabular form
castle %>%
  group_by(sid) %>%
  mutate(treated = any(post>0)) %>%
  group_by(treated) %>%
  summarise(
  n_states = n_distinct(sid),
  n_cities = n(),
  mean_treatment_perc = mean(post)*100 ## Percentage of treated periods
  )


# Matrix completion -------------------------------------------------------

# This will create an object called "reg1" that will contain all the values
# listed in `?gsynth`
reg1 = gsynth(l_homicide ~ post,        # "regress" log homicides on post treated status
              data = castle,            # specify our dataset
              index = c("sid", "year"), # Our panel unit and time FEs
              estimator = "mc",         # NB: Sets estimation method to matrix completion!
              nlambda = 10,             # Number of lambda to search
              CV = TRUE,                # Runs cross-validation to choose lambda
              k = 10,                   # Number of folds for cross-validation
              force = "two-way",        # Unit and time fixed effects
              se = TRUE,                # Compute standard errors
              nboots = 1000,            # Number of bootstraps to run
              na.rm = TRUE,             # Remove missing values
              parallel = TRUE,          # Run parallel computing (should decrease time)
              seed = 011235)            # Seed for reproducibility

# Let's check the output
reg1

# In plot form
plot(reg1)

# Alternative representation
plot(reg1, type = "counterfactual", raw = "all")

## We can also extract the overall average treatment effect
reg1$est.avg

## And the cross-validation lambda (i.e. optimal hyper-parameter chosen via CV)
reg1$lambda.cv