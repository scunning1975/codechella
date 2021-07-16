**********************************************************************
* name: 2sdid.do
* author: scott cunningham (baylor) using kyle butts (colorado) ado
* description: estimate treatment effects using 2sDiD
* date: June 2, 2021
**********************************************************************

net install did2s, replace from("https://raw.githubusercontent.com/kylebutts/did2s_stata/main/ado/")
net install cleanplots, replace from("https://tdmize.github.io/data/cleanplots")
* ssc install did2s

* load data
use https://github.com/scunning1975/mixtape/raw/master/castle.dta, clear
set scheme cleanplots

** Static specification and population weights

** Begin TWFE specification.  
xi: xtreg l_homicide i.year post [aweight=popwt], fe vce(cluster sid)
    

** Begin Manual 2SDiD
* Step 1: Manually (note standard errors are off)
reg l_homicide i.sid i.year [aweight=popwt] if post == 0

* Step 2: Regress transformed outcome onto treatment status for all units
predict adj, residuals
reg adj i.post [aweight=popwt], vce(cluster sid) nocons
* 1.post .075

** Begin Butts' did2s ado file
did2s l_homicide [aweight=popwt], first_stage(i.sid i.year) second_stage(i.post) treatment(post) cluster(sid)
