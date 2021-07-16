#---------------------------------------------------
#      Application: Meyer, Viscusi and Durbin (1995, AER)
#---------------------------------------------------
#-----------------------------------------------------------------------------
# Startup - clear memory, load packages, set working directory, and import the dataset
# Clear memory
rm(list=ls())

library(wooldridge)
#library(sem)
library(here)
library(DRDID)
library(fixest)
#-----------------------------------------------------------------------------
# import the data
injury <- wooldridge::injury
# Get unit's id (repeated cross section)
injury$id <- 1:dim(injury)[1]

#-----------------------------------------------------------------------------
# create two state subsets
# Kentucky subset
injury_ky <- subset(injury, injury$ky==1)
# Michigan subset
injury_mi <- subset(injury, injury$mi==1)
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
#     A. Kentucky Sample
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
# Unconditional DiD analysis
#-----------------------------------------------------------------------------
# Duration of benefits
twfe_ky_dur <- fixest::feols(durat ~ highearn + afchnge + I(highearn * afchnge), 
                      data = injury_ky,
                      cluster = ~id)

summary(twfe_ky_dur)

# Log of duration of benefits
twfe_ky_ldur <- fixest::feols(ldurat ~ highearn + afchnge + I(highearn * afchnge), 
                             data = injury_ky,
                             cluster = ~id)

summary(twfe_ky_ldur)

# Medical cost
twfe_ky_med <- fixest::feols(totmed ~ highearn + afchnge + I(highearn * afchnge), 
                             data = injury_ky,
                             cluster = ~id)

summary(twfe_ky_med)

# log of Medical cost
twfe_ky_lmed <- fixest::feols(ltotmed ~ highearn + afchnge + I(highearn * afchnge), 
                             data = injury_ky,
                             cluster = ~id)

summary(twfe_ky_lmed)

#-----------------------------------------------------------------------------
# Conditional DiD analysis
#-----------------------------------------------------------------------------
# What kind of covariates we "should" include?!
# Reported injuries are potentially affected by policy, so we will NOT include them

#-----------------------------------------------------------------------------
# Doubly Robust DiD procedures
#-----------------------------------------------------------------------------
# Doubly Robust: Duration of benefits
dr_ky_dur <- drdid(yname = "durat",
                   tname = "afchnge",
                   idname = "id",
                   dname = "highearn",
                   xformla = ~ male + married + manuf + construc + lage,
                   data = injury_ky,
                   panel = FALSE,
                   estMethod = "imp",
                   boot = TRUE,
                   boot.type = "weighted"
                   )

# Doubly Robust: Log duration of benefits
dr_ky_ldur <- drdid(yname = "ldurat",
                   tname = "afchnge",
                   idname = "id",
                   dname = "highearn",
                   xformla = ~ male + married + manuf + construc + lage,
                   data = injury_ky,
                   panel = FALSE,
                   estMethod = "imp",
                   boot = TRUE,
                   boot.type = "weighted"
)

# Doubly Robust: Medical cost
dr_ky_med <- drdid(yname = "totmed",
                   tname = "afchnge",
                   idname = "id",
                   dname = "highearn",
                   xformla = ~ male + married + manuf + construc + lage,
                   data = injury_ky,
                   panel = FALSE,
                   estMethod = "imp",
                   boot = TRUE,
                   boot.type = "weighted"
)

# Doubly Robust: Log Medical cost
dr_ky_lmed <- drdid(yname = "ltotmed",
                    tname = "afchnge",
                    idname = "id",
                    dname = "highearn",
                    xformla = ~ male + married + manuf + construc + lage,
                    data = injury_ky,
                    panel = FALSE,
                    estMethod = "imp",
                    boot = TRUE,
                    boot.type = "weighted"
)


#-----------------------------------------------------------------------------
# Regression Adjustments DiD Procedures
#-----------------------------------------------------------------------------

# Regression Adjustment: Duration of benefits
ra_ky_dur <- ordid(yname = "durat",
                   tname = "afchnge",
                   idname = "id",
                   dname = "highearn",
                   xformla = ~ male + married + manuf + construc + lage,
                   data = injury_ky,
                   panel = FALSE,
                   estMethod = "imp",
                   boot = TRUE,
                   boot.type = "weighted"
)

# Regression Adjustment: Log duration of benefits
ra_ky_ldur <- ordid(yname = "ldurat",
                    tname = "afchnge",
                    idname = "id",
                    dname = "highearn",
                    xformla = ~ male + married + manuf + construc + lage,
                    data = injury_ky,
                    panel = FALSE,
                    estMethod = "imp",
                    boot = TRUE,
                    boot.type = "weighted"
)

# Regression Adjustment: Medical cost
ra_ky_med <- ordid(yname = "totmed",
                   tname = "afchnge",
                   idname = "id",
                   dname = "highearn",
                   xformla = ~ male + married + manuf + construc + lage,
                   data = injury_ky,
                   panel = FALSE,
                   estMethod = "imp",
                   boot = TRUE,
                   boot.type = "weighted"
)

# Regression Adjustment: Log Medical cost
ra_ky_lmed <- ordid(yname = "ltotmed",
                    tname = "afchnge",
                    idname = "id",
                    dname = "highearn",
                    xformla = ~ male + married + manuf + construc + lage,
                    data = injury_ky,
                    panel = FALSE,
                    estMethod = "imp",
                    boot = TRUE,
                    boot.type = "weighted"
)

#-----------------------------------------------------------------------------
# Inverse-probability weighted DiD Procedures
#-----------------------------------------------------------------------------

# IPW: Duration of benefits
ipw_ky_dur <- ipwdid(yname = "durat",
                   tname = "afchnge",
                   idname = "id",
                   dname = "highearn",
                   xformla = ~ male + married + manuf + construc + lage,
                   data = injury_ky,
                   panel = FALSE,
                   estMethod = "imp",
                   boot = TRUE,
                   boot.type = "weighted"
)

# IPW: Log duration of benefits
ipw_ky_ldur <- ipwdid(yname = "ldurat",
                    tname = "afchnge",
                    idname = "id",
                    dname = "highearn",
                    xformla = ~ male + married + manuf + construc + lage,
                    data = injury_ky,
                    panel = FALSE,
                    estMethod = "imp",
                    boot = TRUE,
                    boot.type = "weighted"
)

# IPW: Medical cost
ipw_ky_med <- ipwdid(yname = "totmed",
                   tname = "afchnge",
                   idname = "id",
                   dname = "highearn",
                   xformla = ~ male + married + manuf + construc + lage,
                   data = injury_ky,
                   panel = FALSE,
                   estMethod = "imp",
                   boot = TRUE,
                   boot.type = "weighted"
)

# IPW: Log Medical cost
ipw_ky_lmed <- ipwdid(yname = "ltotmed",
                    tname = "afchnge",
                    idname = "id",
                    dname = "highearn",
                    xformla = ~ male + married + manuf + construc + lage,
                    data = injury_ky,
                    panel = FALSE,
                    estMethod = "imp",
                    boot = TRUE,
                    boot.type = "weighted"
)





#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
#     B. Michigan Sample
#-----------------------------------------------------------------------------
 #Your turnn!