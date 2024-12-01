/* Install the required packages if they are not already installed
   ssc install reghdfe
   ssc install ftools
   ssc install honestdid
   */

*****************************************************************************************************************
** Simulation 1: Difference-in-Differences and Sensitivity Analysis
** N = 10000
** There are 2 groups in the sample (treatment/control)
** There are 10 periods in the sample
** Treatment begins at year 6 
** y = rnormal(0.8, 0.2) + year + rnormal(0.3, 0.03)*D
** We first conduct the conventional event study analysis
** We next conduct the sensitivity analysis which is recently developed by Rambachan and Roth (2023)
** Confirm that when the variance of the treatment effect is small, the significant results do not change 
*****************************************************************************************************************

*****************************************************************************************************************
** Data Creation
*****************************************************************************************************************
* Clear the data
clear

* Set the number of observations to 10000
set obs 10000

* Create the panel data
* The panel data has 1000 units and 10 periods 
gen id = _n
gen num = _n
gen year = 1

forvalues k = 1000(1000)9000{
	replace id = id - 1000 if num > `k'
}

forvalues t = 1000(1000)9000{
	replace year = year + 1 if num > `t'
}

* Generate the indicator variable which equals to one for treatment group
gen treat = (id <= 500)

* Generate the indicator variable which equals to one for post-treatment period
gen post = (year >= 6)

* Generate the treatment dummy variable
gen D = treat*post

* Generate the outcome variable 
gen y = rnormal(0.8, 0.2) + year + rnormal(0.3, 0.03)*D

* Save the processed data as "Event_Data.dta"
save "Event_Data.dta", replace

*****************************************************************************************************************
** Event Study Plot using Conventional TWFEDD
*****************************************************************************************************************
* Import the data
use "Event_Data.dta", clear

* Static estimation
reghdfe y D, abs(id year)

* Event study estimation
reghdfe y treat##ib5.year, abs(id year)

gen coef = .
gen se = .

forvalues i = 1(1)10{
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
	  legend(order( 3)) title("Event Study DID") xlabel(, nogrid) ylabel(, nogrid) xline(5)
	  
*****************************************************************************************************************
** Conducting sensitivity analysis to check the validity of parallel trends (Rambachan and Roth, 2023)
*****************************************************************************************************************
* Confirm each estimate
matrix list e(b)

* Sensitivity analysis for the 1st period
matrix l_vec = 1\0\0\0\0
honestdid, l_vec(l_vec) pre(1/4) post(5/9) mvec(0.5(0.5)2) omit coefplot `plotopts'

* Sensitivity analysis for the 2nd period
matrix l_vec = 0\1\0\0\0
honestdid, l_vec(l_vec) pre(1/4) post(5/9) mvec(0.5(0.5)2) omit coefplot `plotopts'

* Sensitivity analysis for the 3rd period
honestdid, l_vec(l_vec) pre(1/4) post(5/9) mvec(0.5(0.5)2) omit coefplot `plotopts'

* Sensitivity analysis for the 4th period
matrix l_vec = 0\0\0\1\0
honestdid, l_vec(l_vec) pre(1/4) post(5/9) mvec(0.5(0.5)2) omit coefplot `plotopts'

* Sensitivity analysis for the 5th period
matrix l_vec = 0\0\0\0\1
honestdid, l_vec(l_vec) pre(1/4) post(5/9) mvec(0.5(0.5)2) omit coefplot `plotopts'

* Sensitivity analysis for the average effect
matrix l_vec = 0.2\0.2\0.2\0.2\0.2
honestdid, l_vec(l_vec) pre(1/4) post(5/9) mvec(0.5(0.5)2) omit coefplot `plotopts'

*****************************************************************************************************************
** Simulation 2: Difference-in-Differences and Sensitivity Analysis
** N = 10000
** There are 2 groups in the sample (treatment/control)
** There are 10 periods in the sample
** Treatment begins at year 6 
** y = rnormal(0.8, 0.2) + year + rnormal(0.3, 3)*D
** We first conduct the conventional event study analysis
** We next conduct the sensitivity analysis which is recently developed by Rambachan and Roth (2023)
** Confirm that when the variance of the treatment effect is large, the results can easily be changed
*****************************************************************************************************************

*****************************************************************************************************************
** Data Creation
*****************************************************************************************************************
* Clear the data
clear

* Set the number of observations to 10000
set obs 10000

* Create the panel data
* The panel data has 1000 units and 10 periods 
gen id = _n
gen num = _n
gen year = 1

forvalues k = 1000(1000)9000{
	replace id = id - 1000 if num > `k'
}

forvalues t = 1000(1000)9000{
	replace year = year + 1 if num > `t'
}

* Generate the indicator variable which equals to one for treatment group
gen treat = (id <= 500)

* Generate the indicator variable which equals to one for post-treatment period
gen post = (year >= 6)

* Generate the treatment dummy variable
gen D = treat*post

* Generate the outcome variable 
gen y = rnormal(0.8, 0.2) + year + rnormal(0.3, 3)*D

* Save the processed data as "Event_Data.dta"
save "Event_Data.dta", replace

*****************************************************************************************************************
** Event Study Plot using Conventional TWFEDD
*****************************************************************************************************************
* Import the data
use "Event_Data.dta", clear

* Static estimation
reghdfe y D, abs(id year)

* Event study estimation
reghdfe y treat##ib5.year, abs(id year)

gen coef = .
gen se = .

forvalues i = 1(1)10{
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
	  legend(order( 3)) title("Event Study DID") xlabel(, nogrid) ylabel(, nogrid) xline(5)

*****************************************************************************************************************
** Conducting sensitivity analysis to check the validity of parallel trends (Rambachan and Roth, 2023)
*****************************************************************************************************************
* Confirm each estimate
matrix list e(b)

* Sensitivity analysis for the 1st period
matrix l_vec = 1\0\0\0\0
honestdid, l_vec(l_vec) pre(1/4) post(5/9) mvec(0.5(0.5)2) omit coefplot `plotopts'

* Sensitivity analysis for the 2nd period
matrix l_vec = 0\1\0\0\0
honestdid, l_vec(l_vec) pre(1/4) post(5/9) mvec(0.5(0.5)2) omit coefplot `plotopts'

* Sensitivity analysis for the 3rd period
honestdid, l_vec(l_vec) pre(1/4) post(5/9) mvec(0.5(0.5)2) omit coefplot `plotopts'

* Sensitivity analysis for the 4th period
matrix l_vec = 0\0\0\1\0
honestdid, l_vec(l_vec) pre(1/4) post(5/9) mvec(0.5(0.5)2) omit coefplot `plotopts'

* Sensitivity analysis for the 5th period
matrix l_vec = 0\0\0\0\1
honestdid, l_vec(l_vec) pre(1/4) post(5/9) mvec(0.5(0.5)2) omit coefplot `plotopts'

* Sensitivity analysis for the average effect
matrix l_vec = 0.2\0.2\0.2\0.2\0.2
honestdid, l_vec(l_vec) pre(1/4) post(5/9) mvec(0.5(0.5)2) omit coefplot `plotopts'

