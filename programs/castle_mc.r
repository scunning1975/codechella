###################################################################################
# Program:       Effect of castle doctrine reform on log homicides
# Author:        Scott Cunningham
# Affiliation:   Baylor University
# Created:       5/7/2021
# Date Modified: 5/7/2021
# Modified by:   Scott Cunningham
###################################################################################

## ## Install from Github (development version)
## install.packages('devtools', repos = 'http://cran.us.r-project.org') # if not already installed
# install.packages('devtools', repos = 'http://cran.us.r-project.org') # if not already installed
# devtools::install_github('xuyiqing/gsynth')
# install.packages("readstata13")

## ## Install from Github (development version)
## install.packages('devtools', repos = 'http://cran.us.r-project.org') # if not already installed
# install.packages('devtools', repos = 'http://cran.us.r-project.org') # if not already installed
# devtools::install_github('xuyiqing/gsynth')
# install.packages("readstata13")

if (! "gsynth" %in% installed.packages()){
  install.packages("gsynth")
}


if (! "tidyverse" %in% installed.packages()){
  install.packages("tidyverse")
}

if (! "stargazer" %in% installed.packages()){
  install.packages("stargazer")
}


if (! "ggthemes" %in% installed.packages()){
  install.packages("ggthemes")
}

if (! "zoo" %in% installed.packages()){
  install.packages("zoo")
}

if (! "knitr" %in% installed.packages()){
  install.packages("knitr")
}

if (! "dplyr" %in% installed.packages()){
  install.packages("dplyr")
}

# calls the library so you can use the commands
# calls the library so you can use the commands
library(ggthemes)
library(tidyverse)
library(panelView)
library(gsynth)
library(haven)
library(stargazer)
library(readstata13)
library(ggplot2)
library(zoo)
library(knitr)
library(dplyr)


# Reads a Stata file (new dataset name is masterfile, the arrow means storing the RHS dataset into that "object", which
# is a group of things - in this case it's data. Arrow is equals in Stata to some degree)
castle <- data.frame(read.dta13('https://github.com/scunning1975/mixtape/raw/master/castle.dta'))
castle$effyear[is.na(castle$effyear)] <- 0 # untreated units have effective year of 0

# Limits data to variables of interest for testing purposes
#   This section obviously needs to be thought through and done correctly
#   before moving forward with the project, but this should give you an idea
#   as to how the matrix completion stuff works.
castle2<-castle[,c("l_homicide","year","sid","effyear","l_police","post")]

# Keeps only complete cases - which I think is a panel balancing command, bc matrix completion needs a balanced panel
castle2<-castle2[complete.cases(castle2),]

# First let's check the distribution of numbers of observation by city

castle2 %>% 
  group_by(sid) %>% 
  summarise(n_obs = n(),
            treated_entries = sum(post)) %>%
  group_by(n_obs) %>%
  summarise(n_cities = n(),
            n_treated = sum(treated_entries > 0),
            mean_treated_entries = mean(treated_entries))

# Looks fine -- let's subset
#castle2<-castle2[castle2$n==180,]

# A subset for testing
# Won't subset -- only 246 cities-- doable size

# This removes the cldata from memory bc R lets you hold multiple datasets and objects in memory, and we need memory bc
# gsynth is HUNGRY. This stands for "release memory", but sometimes objects are not entirely emptied.
rm(castle)

# frees up memory to prevent crashes due to overloaded memory. This stands for "garbage collection". So even when
# you rm you may still need to run gc again.
gc()


# Matrix Completion. This will create an object called reg1 that will contain all the values listed in "help gsynth"
reg<-gsynth(l_homicide ~ post,
             data = castle2,
             index = c("sid","year"),
             estimator = "mc",  # Sets estimation method to matrix completion
             nlambda = 10,      # Number of lambda to search
             CV = T,            # Runs cross-validation to choose lambda
             k = 10,            # Number of folds for cross-validation
             force = "two-way", # Unit and time fixed effects
             se = T,            # Compute standard errors
             nboots = 1000,     # Number of bootstraps to run
             na.rm=T,           # Listwise deletion of all missing values
             parallel = T,      # Run parallel computing (should decrease time)
             seed = 011235)     # Sets the seed so any randomization (e.g. bootstrapping)
                                #   returns the same value

print(reg)
reg$est.att
reg$est.avg
reg$est.beta

plot(reg)

# OK so the issue is that there are dates where there are no untreated observations. Let's drop those dates
# and see what happens

castle3 <- castle2 %>% 
  group_by(year) %>% 
  filter(mean(post) < .9)
  
# Run again with new data
# Matrix Completion. This will create an object called reg1 that will contain all the values listed in "help gsynth"
reg<-gsynth(l_homicide ~ post,
            data = castle2,
            index = c("sid","year"),
            estimator = "mc",  # Sets estimation method to matrix completion
            nlambda = 10,      # Number of lambda to search
            CV = T,            # Runs cross-validation to choose lambda
            k = 10,            # Number of folds for cross-validation
            force = "two-way", # Unit and time fixed effects
            se = T,            # Compute standard errors
            nboots = 1000,      # Number of bootstraps to run
            na.rm=T,           # Listwise deletion of all missing values
            parallel = T,      # Run parallel computing (should decrease time)
            seed = 011235)     # Sets the seed so any randomization (e.g. bootstrapping)
#   returns the same value

lambda_star <- cv_run$lambda.cv

reg<-gsynth(l_homicide ~ post,
            data = castle2,
            index = c("sid","year"),
            estimator = "mc",  # Sets estimation method to matrix completion
            lambda = lambda_star,      # Specifies lambda to use
            CV = T,            # Runs cross-validation to choose lambda
            k = 10,            # Number of folds for cross-validation
            force = "two-way", # Unit and time fixed effects
            se = T,            # Compute standard errors
            nboots = 1000,      # Number of bootstraps to run
            na.rm=T,           # Listwise deletion of all missing values
            parallel = T,      # Run parallel computing (should decrease time)
            seed = 011235)     # Sets the seed so any randomization (e.g. bootstrapping)
#   returns the same value


# Ok -- a new error "Some units have too few pre-treatment observations. Try to remove them."
# Let's take a look at the distribution of when the treatment kicks in

castle3 %>%
  arrange(sid, year) %>%
  group_by(sid) %>%
  # mutate adds a new column
  mutate(treatment_has_kicked_in = cumsum(post) > 0) %>%
  group_by(year) %>% 
  summarise(n_treated_so_far = sum(treatment_has_kicked_in),
            n()) %>%
  ggplot(aes(x = year, y = n_treated_so_far)) +
  geom_bar(stat = "identity", alpha = 0.4) +
  theme_pander()


print(reg)
reg$est.att
reg$est.avg
reg$est.beta

plot(reg)

# Running placebo estimation

# your main data is called castle2


N_reps <- 1000
# Create an empty data frame with number of rows equal to the N_reps and number of columns equal to the number of months whose treatment effects you want
output_df <- as.data.frame(matrix(NA, N_reps, N_months))

for(i in 1:N_reps) {
  data_with_permuted_treatment <- castle2 %>% 
    group_by(sid) %>%
    mutate(permuted_treatment = as.numeric(year >= sample(year, 1)))

  cldata4 <- data_with_permuted_treatment %>% 
    group_by(date) %>% 
    filter(mean(treat) < .9)
  
  # Run your model here using data_with_permuted_treatment, but a) fixing the penalty to whatever value you worked out was the right one in the estimation step, rather than doing it via cross-validation, and not running the bootstrap
  reg<-gsynth(f_all_pc ~ treat,
              data = cldata4,
              index = c("id","date"),
              estimator = "mc",  # Sets estimation method to matrix completion
              nlambda = 10,      # Number of lambda to search
              CV = T,            # Runs cross-validation to choose lambda
              k = 10,            # Number of folds for cross-validation
              force = "two-way", # Unit and time fixed effects
              se = T,            # Compute standard errors
              nboots = F,      # Number of bootstraps to run
              na.rm=T,           # Listwise deletion of all missing values
              parallel = T,      # Run parallel computing (should decrease time)
              seed = 011235)     # Sets the seed so any randomization (e.g. bootstrapping)
  
  output_df[i,] <- t(your_monthly_treatment_effects) # t() is a transpose of a vector, since we're filling in the rows
}

# More ideas for posting results both visually and numerically is contained at his website:
#    http://yiqingxu.org/software/gsynth/gsynth_examples.html

reg<-gsynth(m_all_pc ~ treat,
            data = cldata3,
            index = c("id","date"),
            estimator = "mc",  # Sets estimation method to matrix completion
            nlambda = 10,      # Number of lambda to search
            CV = T,            # Runs cross-validation to choose lambda
            k = 10,            # Number of folds for cross-validation
            force = "two-way", # Unit and time fixed effects
            se = T,            # Compute standard errors
            nboots = 500,      # Number of bootstraps to run
            na.rm=T,           # Listwise deletion of all missing values
            parallel = T,      # Run parallel computing (should decrease time)
            seed = 011235)     # Sets the seed so any randomization (e.g. bootstrapping)
#   returns the same value


cldata3 %>%
  arrange(id, date) %>%
  group_by(id) %>%
  # mutate adds a new column
  mutate(treatment_has_kicked_in = cumsum(treat) > 0) %>%
  group_by(date) %>% 
  summarise(n_treated_so_far = sum(treatment_has_kicked_in),
            n()) %>%
  ggplot(aes(x = date, y = n_treated_so_far)) +
  geom_bar(stat = "identity", alpha = 0.4) +
  theme_pander()


print(reg)
reg$est.att
reg$est.avg
reg$est.beta

plot(reg)

