install.packages(c("readstata13", "devtools","ggplot2", "here", "tidyverse","dplyr",
                   "fixest","estimatr","MASS", "Hmisc", "foreign","statar",
                   "ggpubr", "scales", "ggrepel","bacondecomp", "glue","TwoWayFEWeights",
                   "ggthemes", "patchwork", "ggtext","gridExtra","plm",
                   "sem", "matlib", "trust","foreach", "doSNOW", "doRNG",
                   "reshape2", "ggridges","RColorBrewer","wesanderson",
                   "gridExtra"), repos = "http://cran.rstudio.com", dependencies = TRUE)


devtools::install_github("bcallaway11/BMisc", dependencies = TRUE)
devtools::install_github("pedrohcgs/DRDID", dependencies = TRUE)
devtools::install_github("bcallaway11/did", dependencies = TRUE)
