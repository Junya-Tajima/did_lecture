/* Install the required packages if they are not already installed
   ssc install reghdfe
   ssc install ftools
   ssc install event_plot
   ssc install drdid
   ssc install csdid
   */

**********************************************************************************************************************************************************************
** Simulation: Interpreting Event-Studies from Recent Difference-in-difference methods
** N = 30000
** There are 2 groups in the sample (treatment/control)
** There are 30 periods in the sample
** Treatment begins at year 16 
** Assume linear violation for the trend of the outcome variable between the treatment and control groups
** Conventional event study DID set one period before the treatment as the baseline point for all estimates
** Callaway and Sant'Anna Estimator (2021) uses one period before the target year as the baseline point for the calculation of pre-treatment estimates 
** Because of the above difference, the two mwthods might have different results
** To make the results from CS estimator comparable to the conventional event study, we need to add the "long2" option when using the csdid command
** We first conduct the conventional event study analysis using TWFE estimator
** We next conduct the CS event study analysis to see the difference of the results
** We finally add long2 option to make the results of CSDID cmparable to TWFEDD
**********************************************************************************************************************************************************************

**********************************************************************************************************************************************************************
** Data Creation
**********************************************************************************************************************************************************************
* Clear the data
clear

* Set the number of observations to 30000
set obs 30000

* Create the panel data
* The panel data has 1000 units and 30 periods 
gen id = _n
gen num = _n
gen year = 1

forvalues k = 1000(1000)29000{
	replace id = id - 1000 if num > `k'
}

forvalues t = 1000(1000)29000{
	replace year = year + 1 if num > `t'
}

* Generate the indicator variable which equals to one for treatment group
gen treat = (id <= 500)

* Generate the indicator variable which equals to one for post-treatment period
gen post = (year >= 16)

* Generate the treatment dummy variable
gen D = treat*post

* Generate the outcome variable 
* Assume linear violation for the trend of the outcome variable between the treatment and control groups
gen y = rnormal(0.8, 0.2) + year
replace y = rnormal(0.08, 0.02) + 1.01*year if treat == 1

* Save the processed data as "Event_Data.dta"
save "Event_Data.dta", replace

**********************************************************************************************************************************************************************
** Event Study Plot using Conventional TWFEDD
**********************************************************************************************************************************************************************
use "Event_Data.dta", clear
reghdfe y treat##ib15.year, abs(id year)

gen coef = .
gen se = .

forvalues i = 1(1)30{
	replace coef = _b[1.treat#`i'.year] if year == `i'
    replace se = _se[1.treat#`i'.year] if year ==`i'
}
	
gen citop = coef + 1.96*se
gen cibottom = coef - 1.96*se

keep year coef se citop cibottom 
duplicates drop
sort year

twoway(sc coef year, connect(line) color(blacl)) (rline citop cibottom year, color(black)) ///
      , xtitle("Event time") xline(15) yline(0, lcolor(red)) ytitle("Estimated Coefficients") ///
	  legend(order( 3)) name(g1, replace) title("Conventional TWFEDD")

**********************************************************************************************************************************************************************
** Event Study Plot using Callaway and Sant'Anna Estimator (2021)
**********************************************************************************************************************************************************************
* Conduct CSDID estimation
* Estore the event study results from CSDID
use "Event_Data", clear
egen gvar = csgvar(D), ivar(id) tvar(year)
csdid y D, ivar(id) time(year) gvar(gvar)
estat event, estore(cs)

* Plot the resuls of event study anlysis
csdid_plot, name(g2, replace) title("Callaway and Sant'Anna (2021)")

* Compare the results of CSDID to the results of TWFEDD
graph combine g1 g2, scale(0.8)

**********************************************************************************************************************************************************************
** Adding "long2" to make CS Event-Study result comparable to TWFEDD
**********************************************************************************************************************************************************************
* Conduct CSDID estimation
* Estore the event study results from CSDID
* Add the long2 option to make CS results comparable to the results of TWFEDD
use "Event_Data", clear
egen gvar = csgvar(D), ivar(id) tvar(year)
csdid y D, ivar(id) time(year) gvar(gvar) long2
estat event, estore(cs)

* Plot the resuls of event study anlysis
csdid_plot, name(g3, replace) title("Callaway and Sant'Anna (2021)")

* Compare the results of CSDID to the results of TWFEDD
graph combine g1 g3, scale(0.8)
