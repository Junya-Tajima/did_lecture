/* Install the required packages if they are not already installed
   ssc install reghdfe
   ssc install ftools
   ssc install event_plot
   ssc install addplot
   ssc install drdid
   ssc install csdid
   */

****************************************************************************************************************************
** Simulation 1: Staggered DID + Heterogenous effects + Event Study + No Real Pre-Trends
** N = 30000
** There are 3 groups in the sample
** Treatment begins at 6th(group1), 12th(group2), and 18th year(group3)
** y = rnormal(3, 0.5^2) + year + rnormal(0.9*(year - each timing), 0.5^2)*treat*post for group1
** y = rnormal(3, 0.5^2) + year + rnormal(0.3*(year - each timing), 0.5^2)*treat*post for group2
** y = rnormal(3, 0.5^2) + year + rnormal(0.1*(year - each timing), 0.5^2)*treat*post for group3
** Run the DID regression
** Create Event study plot
****************************************************************************************************************************
*** Create the data
* Clear the data
clear

* Set the number of observations to 30000
set obs 30000

* Set the seed for reproducibility
set seed 111

* Create the panel data
gen id = _n
gen num = _n
gen year = 1

forvalues k = 1000(1000)29000{
	replace id = id - 1000 if num > `k'
}

forvalues t = 1000(1000)29000{
	replace year = year + 1 if num > `t'
}

* Specify the group based on the ID variable
gen group = 0
replace group = 1 if id < = 250
replace group = 2 if id > 250 & id <= 500
replace group = 3 if id > 500 & id <= 750
replace group = 4 if id > 750 & id <= 1000

* Create the treatment dummy variable for each group
gen D = 0
replace D = 1 if group == 1 & year >= 6
replace D = 1 if group == 2 & year >= 12
replace D = 1 if group == 3 & year >= 18

* Create the outcome variable
gen y = rnormal(3, 0.5^2) + year
replace y = y + rnormal(0.9*(year - 6), 0.5^2) if group == 1 & year >= 6
replace y = y + rnormal(0.3*(year - 12), 0.5^2) if group == 2 & year >= 12
replace y = y + rnormal(0.1*(year - 18), 0.5^2) if group == 3 & year >= 18

* Create the variable which specifies the treatment timing for each group
gen timing = 0
replace timing = 6 if group == 1
replace timing = 12 if group == 2
replace timing = 18 if group == 3

* Create the variable indicating how many years has it been since each group received the treatment
gen relative = year - timing

*** Plot the trends for each group
bysort group year : egen Outcome = mean(y)
twoway(line Outcome year if group == 1, color(stc1))(line Outcome year if group == 2, color(stc2)) ///
      (line Outcome year if group == 3, color(stc3))(line Outcome year if group == 4, color(stc4)) ///
	  (scatter Outcome year if group == 1, color(stc1))(scatter Outcome year if group == 2, color(stc2)) ///
	  (scatter Outcome year if group == 3, color(stc3))(scatter Outcome year if group == 4, color(stc4)) ///
	  , xline(6) xline(12) xline(18) xlabel(, nogrid) ylabel(, nogrid) legend(order(1 "Group1" 2 "Group2" 3 "Group3" 4 "Group4"))


*** Conduct event study analysis using two-way fixed effects estimator
* Create a lag variable indicating how many years each group has been treated
forvalues l = 0/24 {
	gen lag`l' = (relative == `l')
}

* Create a lead variable indicating how many years before each group receives the treatment
forvalues l = 1/17 {
	gen lead`l' = (relative == -`l')
}

* Normalise lead1 indicating one period before the treatment to 0
replace lead1 = 0 

* Conduct Event study DID estimation
reghdfe y lead* lag* , a(id year) cluster(id)

* Plot the resuls of event study analysis
* Limit the range of the graph to the five years before and after the treatment
event_plot, default_look stub_lag(lag#) stub_lead(lead#) together graph_opt(xtitle("Event time") ///
            ytitle("Estimates") title("TWFE Event Study") xlabel(-5(1)5, nogrid) ylabel(, nogrid)) ///
			trimlead(5) trimlag(5) 
			
* Add the red line to the graph showing the true treatment effects			
addplot: (scatteri 0 -5 0 0 3 5, xlabel(-5(1)5) recast(line) lp(dash) lc(red))

****************************************************************************************************************************
** Simulation 2: Staggered DID + Heterogenous effects + Event Study + Dropping never treated + No Real Pre-Trends
** N = 30000
** There are 3 groups in the sample
** Treatment begins at 6th(group1), 12th(group2), and 18th year(group3)
** y = rnormal(3, 0.5^2) + year + rnormal(0.9*(year - each timing), 0.5^2)*treat*post for group1
** y = rnormal(3, 0.5^2) + year + rnormal(0.3*(year - each timing), 0.5^2)*treat*post for group2
** y = rnormal(3, 0.5^2) + year + rnormal(0.1*(year - each timing), 0.5^2)*treat*post for group3
** Run the DID regression
** Create Event study plot
****************************************************************************************************************************
*** Create the data
* Clear the data
clear

* Set the number of observations to 30000
set obs 30000

* Set the seed for reproducibility
set seed 222

* Create the panel data
gen id = _n
gen num = _n
gen year = 1

forvalues k = 1000(1000)29000{
	replace id = id - 1000 if num > `k'
}

forvalues t = 1000(1000)29000{
	replace year = year + 1 if num > `t'
}

* Specify the group based on the ID variable
gen group = 0
replace group = 1 if id < = 250
replace group = 2 if id > 250 & id <= 500
replace group = 3 if id > 500 & id <= 750
replace group = 4 if id > 750 & id <= 1000

* Create the treatment dummy variable for each group
gen D = 0
replace D = 1 if group == 1 & year >= 6
replace D = 1 if group == 2 & year >= 12
replace D = 1 if group == 3 & year >= 18

* Create the outcome variable
gen y = rnormal(3, 0.5^2) + year
replace y = y + rnormal(0.9*(year - 6), 0.5^2) if group == 1 & year >= 6
replace y = y + rnormal(0.3*(year - 12), 0.5^2) if group == 2 & year >= 12
replace y = y + rnormal(0.1*(year - 18), 0.5^2) if group == 3 & year >= 18

* Create the variable which specifies the treatment timing for each group
gen timing = 0
replace timing = 6 if group == 1
replace timing = 12 if group == 2
replace timing = 18 if group == 3

* Create the variable indicating how many years has it been since each group received the treatment
gen relative = year - timing

* Drop the group4
drop if group == 4

*** Plot the trends for each group
bysort group year : egen Outcome = mean(y)
twoway(line Outcome year if group == 1, color(stc1))(line Outcome year if group == 2, color(stc2)) ///
      (line Outcome year if group == 3, color(stc3))(scatter Outcome year if group == 1, color(stc1)) ///
	  (scatter Outcome year if group == 2, color(stc2))(scatter Outcome year if group == 3, color(stc3)) ///
	  , xline(6) xline(12) xline(18) xlabel(, nogrid) ylabel(, nogrid) legend(order(1 "Group1" 2 "Group2" 3 "Group3"))

*** Conduct event study analysis using two-way fixed effects estimator
* Create a lag variable indicating how many years each group has been treated
forvalues l = 0/24 {
	gen lag`l' = (relative == `l')
}

* Create a lead variable indicating how many years before each group receives the treatment
forvalues l = 1/17 {
	gen lead`l' = (relative == -`l')
}

* Normalise lead1 indicating one period before the treatment to 0
replace lead1 = 0 

* Conduct Event study DID estimation
reghdfe y lead* lag* , a(id year) cluster(id)

* Plot the resuls of event study analysis
* Limit the range of the graph to the five years before and after the treatment
event_plot, default_look stub_lag(lag#) stub_lead(lead#) together graph_opt(xtitle("Event time") ///
            ytitle("Estimates") title("TWFE Event Study") xlabel(-5(1)5, nogrid) ylabel(, nogrid)) ///
			trimlead(5) trimlag(5) 
			
* Add the red line to the graph showing the true treatment effects			
addplot: (scatteri 0 -5 0 0 3 5, xlabel(-5(1)5) recast(line) lp(dash) lc(red))

****************************************************************************************************************************
** Simulation 3: Comparisons of alternative estimators
** N = 30000
** There are 3 groups in the sample
** Treatment begins at 6th(group1), 12th(group2), and 18th year(group3)
** y = rnormal(3, 0.5^2) + year + rnormal(0.9*(year - each timing), 0.5^2)*treat*post for group1
** y = rnormal(3, 0.5^2) + year + rnormal(0.3*(year - each timing), 0.5^2)*treat*post for group2
** y = rnormal(3, 0.5^2) + year + rnormal(0.1*(year - each timing), 0.5^2)*treat*post for group3
** Use alternative estimators
** Create Event study plot
****************************************************************************************************************************
*** Create the data
* Clear the data
clear

* Set the number of observations to 30000
set obs 30000

* Set the seed for reproducibility
set seed 333

* Create the panel data
gen id = _n
gen num = _n
gen year = 1

forvalues k = 1000(1000)29000{
	replace id = id - 1000 if num > `k'
}

forvalues t = 1000(1000)29000{
	replace year = year + 1 if num > `t'
}

* Specify the group based on the ID variable
gen group = 0
replace group = 1 if id < = 250
replace group = 2 if id > 250 & id <= 500
replace group = 3 if id > 500 & id <= 750
replace group = 4 if id > 750 & id <= 1000

* Create the treatment dummy variable for each group
gen D = 0
replace D = 1 if group == 1 & year >= 6
replace D = 1 if group == 2 & year >= 12
replace D = 1 if group == 3 & year >= 18

* Create the outcome variable
gen y = rnormal(3, 0.5) + year
replace y = y + rnormal(0.9*(year - 6), 0.5^2) if group == 1 & year >= 6
replace y = y + rnormal(0.3*(year - 12), 0.5^2) if group == 2 & year >= 12
replace y = y + rnormal(0.1*(year - 18), 0.5^2) if group == 3 & year >= 18

* Create the variable which specifies the treatment timing for each group
gen timing = 0
replace timing = 6 if group == 1
replace timing = 12 if group == 2
replace timing = 18 if group == 3

* Create the variable indicating how many years has it been since each group received the treatment
gen relative = year - timing

* Drop the group4
drop if group == 4

*** Conduct event study analysis using two-way fixed effects estimator
* Create a lag variable indicating how many years each group has been treated
forvalues l = 0/24 {
	gen lag`l' = (relative == `l')
}

* Create a lead variable indicating how many years before each group receives the treatment
forvalues l = 1/17 {
	gen lead`l' = (relative == -`l')
}

* Normalise lead1 indicating one period before the treatment to 0
replace lead1 = 0 

* Conduct Event study DID estimation
reghdfe y lead* lag* , a(id year) cluster(id)

* Plot the resuls of event study analysis
* Limit the range of the graph to the five years before and after the treatment
event_plot, default_look stub_lag(lag#) stub_lead(lead#) together graph_opt(xtitle("Event time") ///
            ytitle("Estimates") title("TWFE Event Study") xlabel(-5(1)5, nogrid) ylabel(, nogrid) name(g1, replace)) ///
			trimlead(5) trimlag(5)
			
* Add the red line to the graph showing the true treatment effects			
addplot: (scatteri 0 -5 0 0 3 5, xlabel(-5(1)5) recast(line) lp(dash) lc(red))
	
* Save the proceesed data as "maindata.dta"
save "maindata.dta", replace

*** Stacked Regression (Cengiz et al., 2019)
* Restrict the sample to only comparisons that are safe for Group 1
* Save the restricted sample as "sub1.dta"
use "maindata.dta", clear
keep if (year >= 6 - 5) & (year <= 6 + 5)
gen sub = 1
save "sub1.dta", replace

* Restrict the sample to only comparisons that are safe for Group 2
* Save the restricted sample as "sub2.dta"
use "maindata.dta", clear
keep if (year >= 12 - 5) & (year <= 12 + 5)
keep if group >= 2
gen sub = 2
save "sub2.dta", replace

* Restrict the sample to only comparisons that are safe for Group 3
use "maindata.dta", clear
keep if (year >= 18 - 5) & (year <= 18 + 5)
keep if group == 3
gen sub = 3

* Stack each restricted sample
append using "sub1.dta"
append using "sub2.dta"

* Conduct event study DID estimation (including sab-individual and sub-year fixed effects)
reghdfe y  lag* lead* , abs(i.id#i.sub i.year#i.sub) vce(cl id)

* Plot the resuls of event study analysis
* Limit the range of the graph to the five years before and after the treatment
event_plot, default_look stub_lag(lag#) stub_lead(lead#) together graph_opt(xtitle("Event time") ///
            ytitle("Estimates") title("Stacked Regression") xlabel(-5(1)5, nogrid) ylabel(, nogrid) name(g2, replace)) ///
			trimlead(5) trimlag(5)
			
* Add the red line to the graph showing the true treatment effects			
addplot: (scatteri 0 -5 0 0 3 5, xlabel(-5(1)5) recast(line) lp(dash) lc(red))

*** Callaway and Sant'Anna estimator (2021)
* Download the maindata
use "maindata.dta", clear

* Conduct event study DID estimation using the estimator developed from Callaway and Sant'Annna (2021)
csdid y, ivar(id) time(year) gvar(timing) notyet wboot method(dripw)
estat event, estore(cs)

* Plot the resuls of event study analysis
* Limit the range of the graph to the five years before and after the treatment
event_plot cs, default_look stub_lag(Tp#) stub_lead(Tm#) together graph_opt(xtitle("Event time") ///
            ytitle("Estimates") title("Callaway and Sant'Anna") xlabel(-5(1)5, nogrid) ylabel(, nogrid) name(g3, replace)) ///
			trimlead(5) trimlag(5)
			
* Add the red line to the graph showing the true treatment effects			
addplot: (scatteri 0 -5 0 0 3 5, xlabel(-5(1)5) recast(line) lp(dash) lc(red))

*** Imputation method (Gardner, 2021)
* Download the maindata
use "maindata.dta", clear

* Run the regression with individual and year fixed effects for untreatd units
reg y i.id i.year if D == 0, nocons

* Residualize the outcome variable using estimated fixed effects
predict yhat , residual

* Conduct event study estimation
* Remove the observations which have no comprison group
reg yhat lead* lag* if group != 3 | year < 18, nocons vce(cl id)

* Plot the resuls of event study analysis
* Limit the range of the graph to the five years before and after the treatment
event_plot, default_look stub_lag(lag#) stub_lead(lead#) together graph_opt(xtitle("Event time") ///
            ytitle("Estimates") title("Gardner") xlabel(-5(1)5, nogrid) ylabel(, nogrid) name(g4, replace)) ///
			trimlead(5) trimlag(5)
			
* Add the red line to the graph showing the true treatment effects			
addplot: (scatteri 0 -5 0 0 3 5, xlabel(-5(1)5) recast(line) lp(dash) lc(red))

*** Combine all graphs
graph combine g1 g2 g3 g4, scale(0.8)



