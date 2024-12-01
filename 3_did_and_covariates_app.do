/* Install the required packages if they are not already installed
   ssc install reghdfe
   ssc install ftools
   */

******************************************************************************************************************************
** Data Analysis : The Impact of Corporate Governance Reforms in Japan on Firm Performance
** Data File : performance.dta 
** Data Source : Nikkei NEEDS FinancialQuest, Japan Company Handbook CD-ROM
** Practice of DID analysis with covariates
** Find out how covariates changes the results of DID
******************************************************************************************************************************

******************************************************************************************************************************
** Import the data and create the variables
******************************************************************************************************************************
** Set your working directoy
cd ""

** Import the data
use "performance.dta", clear

** Generate the indicator variable which equals to one for treatment firms
generate treat = (market == 1)

** Generate the indicator variable  which equals to one for post-treatment periods
generate post = (year >= 2016)

** Generate the treatment dummy variable
generate D = treat*post

** Save the processed data
save "Processed_Data.dta", replace

******************************************************************************************************************************
** Check the data and variables
******************************************************************************************************************************
** Summarize variables
summarize 

** Create a histogram of ROA for the treatment group and the control group in the pre-treatment period
twoway(histogram roa if treat == 1 & post == 0, color(stc1%70))(histogram roa if treat == 0 & post == 0, color(stc2%70)) ///
      , legend(order(1 "Treatment" 2 "Control")) xlabel(, nogrid) ylabel(, nogrid)

** Create a histogram of the ratio for the treatment group and the control group in the pre-treatment period (excluding firms whose ROA exceeds 0.2)
twoway(histogram roa if treat == 1 & post == 0 & roa <= 10, color(stc1%70)) ///
      (histogram roa if treat == 0 & post == 0 & roa <= 10, color(stc2%70)) ///
	  , legend(order(1 "Treatment" 2 "Control")) xlabel(, nogrid) ylabel(, nogrid)

** Plot the means for the four groups: pre- and post-treatment for both treatment and control firms
bysort treat post: egen roa_mean = mean(roa)
twoway(line roa_mean post if treat == 1, color(stc1))(line roa_mean post if treat == 0, color(stc2)) ///
      (scatter roa_mean post if treat == 1, color(stc1) msize(large))(scatter roa_mean post if treat == 0, color(stc2) msize(large) msymbol(triangle)) ///
	  , xline(0.5) legend(order(3 "Treatment" 4 "Control")) xtitle("POST") ytitle("ROA") xlabel(#2, nogrid) ylabel(, nogrid)

******************************************************************************************************************************
** Difference-in-differences estimator without covariates
******************************************************************************************************************************  
** Static estimation 
reghdfe roa D, abs(id year) vce(cl id) nocons

** Event study estimation 
* Import the processed data
use "Processed_Data", clear

* Generate the time variable which defines event time
gen time = 0
replace time = year - 2009 if year <= 2014
replace time = year - 2010 if year >= 2016

* Two-way fixed effects estimator allowing for dynamic effects
reghdfe roa treat##ib5.time, abs(id year) vce(cl id)

* Plotting event study estimates and confidence intervals
gen coef = .
gen se = .

forvalues i = 1(1)10{
	replace coef = _b[1.treat#`i'.time] if time == `i'
    replace se = _se[1.treat#`i'.time] if time ==`i'
}
	
gen citop = coef + 1.96*se
gen cibottom = coef - 1.96*se

keep time coef se citop cibottom 
duplicates drop
sort time

twoway(rspike citop cibottom time, lwidth(vvvthick) mcolor(gray%0) lcolor(gray%25) fcolor(gray%0))(sc coef time, color(stc2) msize(large) msymbol(diamond)) ///
      , xtitle("Event Time") xline(5) yline(0, lcolor(black)) ytitle("Estimated Effects") legend(order(4)) ///
	  xlabel(1 "-5" 2 "-4" 3 "-3" 4 "-2" 5 "-1" 6 "0" 7 "1" 8 "2" 9 "3" 10 "4", nogrid) ylabel(, nogrid) title("Event Study Plot")

******************************************************************************************************************************
** Difference-in-differences estimator including time-varying covariates
******************************************************************************************************************************	  
** Static estimation 
* Import the processed data
use "Processed_Data", clear
reghdfe roa D logta logage lev cfta salta, abs(id year) vce(cl id) nocons

** Event study estimation 
* Import the processed data
use "Processed_Data", clear

* Generate the time variable which defines event time
gen time = 0
replace time = year - 2009 if year <= 2014
replace time = year - 2010 if year >= 2016

* Two-way fixed effects estimator allowing for dynamic effects
reghdfe roa treat##ib5.time logta logage lev cfta salta, abs(id year) vce(cl id)

* Plotting event study estimates and confidence intervals
gen coef = .
gen se = .

forvalues i = 1(1)10{
	replace coef = _b[1.treat#`i'.time] if time == `i'
    replace se = _se[1.treat#`i'.time] if time ==`i'
}
	
gen citop = coef + 1.96*se
gen cibottom = coef - 1.96*se

keep time coef se citop cibottom 
duplicates drop
sort time

twoway(rspike citop cibottom time, lwidth(vvvthick) mcolor(gray%0) lcolor(gray%25) fcolor(gray%0))(sc coef time, color(stc2) msize(large) msymbol(diamond)) ///
      , xtitle("Event Time") xline(5) yline(0, lcolor(black)) ytitle("Estimated Effects") legend(order(4)) ///
	  xlabel(1 "-5" 2 "-4" 3 "-3" 4 "-2" 5 "-1" 6 "0" 7 "1" 8 "2" 9 "3" 10 "4", nogrid) ylabel(, nogrid) title("Event Study Plot")

******************************************************************************************************************************
** Difference-in-differences estimator including pre-treatment time-varying covariates
******************************************************************************************************************************
** Doubly-Robust DID
* Import the data
use "Processed_Data", clear

* Create a variable specifying  the treatment timing
replace year = year - 1 if year >= 2016
egen gvar = csgvar(D), ivar(id) tvar(year)

* Callaway and Sant'Anna Estimator (2021)
csdid roa D logta logage lev cfta salta, ivar(id) time(year) gvar(gvar) method(dripw) long2

* Simple aggregation 
estat simple

* Event study plot
estat event
csdid_plot

******************************************************************************************************************************
** Difference-in-differences estimator including time-invariant covariates
******************************************************************************************************************************
** Doubly-Robust DID
* Import the data
use "Processed_Data", clear

* Create a variable specifying  the treatment timing
replace year = year - 1 if year >= 2016
egen gvar = csgvar(D), ivar(id) tvar(year)

* Callaway and Sant'Anna Estimator (2021)
csdid roa D indus pref, ivar(id) time(year) gvar(gvar) method(dripw) long2

* Simple aggregation 
estat simple

* Event study plot
estat event
csdid_plot

******************************************************************************************************************************
** Difference-in-differences estimator including pre-treatment time-varying covariates and time-invariant covariates
******************************************************************************************************************************
** Doubly-Robust DID
* Import the data
use "Processed_Data", clear

* Create a variable specifying  the treatment timing
replace year = year - 1 if year >= 2016
egen gvar = csgvar(D), ivar(id) tvar(year)

* Callaway and Sant'Anna Estimator (2021)
csdid roa D logta logage lev cfta salta indus pref, ivar(id) time(year) gvar(gvar) method(dripw) long2

* Simple aggregation 
estat simple

* Event study plot
estat event
csdid_plot






