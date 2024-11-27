/* Install the required packages if they are not already installed
   ssc install reghdfe
   ssc install ftools
   */

************************************************************************************************************************************** 
** Simulation: Difference-in-differences estimation
** N = 20000
** There are 2 groups in the sample (treatment and control)
** Treatment begins at year 6
** y = rnormal(3, 0.5^2) + rnormal(2, 0.5^2)*treat + rnormal(0.2, 0.5^2)*year + rnormal(4, 0.5^2)*D + rnormal()
** Run the DID regression
** Conduct pre-trends checks, such as trends plot, placebo tests, and event study
************************************************************************************************************************************** 

********************************************************************************************************************
** Data creation
********************************************************************************************************************
** Create the data
* Clear the data
clear

* Set the number of observations to 30000
set obs 20000

* Set the seed for reproducibility
set seed 1

* Create the panel data
gen id = _n
gen num = _n
gen year = 1

forvalues k = 2000(2000)18000{
	replace id = id - 2000 if num > `k'
}

forvalues t = 2000(2000)18000{
	replace year = year + 1 if num > `t'
}

* Create a dummy variable that equals 1 for the treatment group
gen treat = 0
replace treat = 1 if id <= 1000

* Create a dummy variable that equals 1 for the post-treatment period
gen post = 0
replace post = 1 if year >= 6

* Create the treatment dummy variable
gen D = treat*post

* Generate the outcome variable
* Average treatment effect on the treated is 4 
gen y = rnormal(3, 0.5^2) + rnormal(2, 0.5^2)*treat + rnormal(0.2, 0.5^2)*year + rnormal(4, 0.5^2)*D + rnormal()

* save the data as "did.dta"
save "did.dta", replace

********************************************************************************************************************
** Check the data and variables
********************************************************************************************************************
** Summarize the outcome variable
summarize y

** Summarize the outcome variable in detail
summarize y, detail

** Create a histogram of the outcome for the treatment group and the control group in the pre-treatment period
twoway(histogram y if treat == 1 & post == 0, color(stc1%70))(histogram y if treat == 0 & post == 0, color(stc2%70)) ///
      , legend(order(1 "Treatment" 2 "Control")) xlabel(, nogrid) ylabel(, nogrid)

** Plot the means for the four groups: pre- and post-treatment for both treatment and control groups
bysort treat post: egen y_mean = mean(y)

twoway(line y_mean post if treat == 1, color(stc1))(line y_mean post if treat == 0, color(stc2)) ///
      (scatter y_mean post if treat == 1, color(stc1) msize(large))(scatter y_mean post if treat == 0, color(stc2) msize(large) msymbol(triangle)) ///
	  , xline(0.5) legend(order(3 "Treatment" 4 "Control")) xtitle("POST") ytitle("Outcome Variable") xlabel(#2, nogrid) ylabel(, nogrid)

** Plot the difference-in-differences estimate
sum y_mean if treat == 1 & post == 0
generate y_treat_pre = r(mean)

sum y_mean if treat == 0 & post == 0
generate y_cont_pre = r(mean)

generate diff = y_treat_pre - y_cont_pre
gen y_mean_plus_prediff = y_mean + diff if treat == 0

twoway(line y_mean post if treat == 1, color(stc1))(line y_mean_plus_prediff post if treat == 0, color(stc2)) ///
      (scatter y_mean post if treat == 1, color(stc1) msize(large))(scatter y_mean_plus_prediff post if treat == 0, color(stc2) msize(large) msymbol(triangle)) ///
	  , xline(0.5) legend(order(3 "Treatment" 4 "Control")) xtitle("POST") ytitle("Outcome Variable") xlabel(#2, nogrid) ylabel(, nogrid)

********************************************************************************************************************
** Difference-in-differences estimation using two-way fixed effects estimator
********************************************************************************************************************
** Difference-in-differences estimator using two-way fixed effects
reghdfe y D, abs(id year) vce(cl id) nocons

********************************************************************************************************************
** Conducting pre-trends check
********************************************************************************************************************
** Trends plot
* Calculate the annual mean of the outcome variable separately for the treatment group and the control group
bysort treat year: egen y_mean_year = mean(y)

* Plotting trends of the outcome variable
twoway(line y_mean_year year if treat == 1, color(black))(line y_mean_year year if treat == 0, color(black) lp(dash)) ///
	  (scatter y_mean_year year if treat == 1 , color(stc1) msymbol(circle) msize(large)) ///
	  (scatter y_mean_year year if treat == 0 , color(stc2) msymbol(triangle) msize(large)) ///
	  , xline(5) xtitle("Year") ytitle("Outcome Variable") xlabel(, nogrid) ylabel(, nogrid) ///
	  legend(order(3 "Treatment" 4 "Control")) 

** Placebo tests
* Prepare empty vectors to store the estimated values
matrix define B = J(4, 1, .)
matrix define SE = J(4, 1, .)

* Conduct a placebo test for each pre-treatment year
forvalues k = 1(1)4{

use "did.dta", clear
drop if post == 1
gen placebopost = (year >= `k' + 1)
gen PLACEBO = treat*placebopost

reghdfe y PLACEBO, abs(id year) vce(cl id) nocons

matrix  B[`k',1] = r(table)[1,1]
matrix  SE[`k',1] = r(table)[2,1]
}

* Delete all the data
clear

set obs 4
gen time = _n

gen b =.
gen se = .

forvalues k = 1(1)4{
	replace b = B[`k', 1] if time == `k'
}

forvalues k = 1(1)4{
	replace se = SE[`k', 1] if time == `k'
}

gen citop = b + 1.96*se
gen cibottom = b - 1.96*se

* Plot each placebo estimate and 95% CI
twoway(rspike citop cibottom time, lwidth(vvvthick) mcolor(gray%0) lcolor(gray%25) fcolor(gray%0))(sc b time, color(stc2) msize(large) msymbol(diamond)) ///
     , xtitle("Placebo Year") xline(-1) yline(0, lcolor(black)) ytitle("Estimated Effects") legend(order(4)) xlabel(1 "2" 2 "3" 3 "4" 4 "5", nogrid) ///
	   ylabel(, nogrid) title("Placebo Tests")

** Event Study
* Import the processed data
use "did.dta", clear

* Two-way fixed effects estimator allowing for dynamic effects
reghdfe y treat##ib5.year, abs(id year) vce(cl id)

* Plotting event study estimates and confidence intervals
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

twoway(rarea citop cibottom year, lwidth(vvvthick) mcolor(gray%0) lcolor(gray%25) fcolor(gray%0))(sc coef year, color(stc2) msize(large) msymbol(diamond)) ///
      , xtitle("Event Time") xline(5) yline(0, lcolor(black)) ytitle("Estimated Effects") legend(order(4)) ///
	  xlabel(1 "-5" 2 "-4" 3 "-3" 4 "-2" 5 "-1" 6 "0" 7 "1" 8 "2" 9 "3" 10 "4", nogrid) ylabel(, nogrid) title("Event Study Plot")

* Plotting event study results with different baselines
forvalues k = 1(1)4{

use "did.dta", clear

reghdfe y treat##ib`k'.year, abs(id year) vce(cl id) 

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

twoway(rarea citop cibottom year , lwidth(vvvthick) mcolor(gray%0) lcolor(gray%25) fcolor(gray%0)) ///
      (sc coef year, color(stc2) msize(large) msymbol(diamond)), xtitle("Event Time") xline(`k') yline(0, lcolor(black)) ///
	  ytitle("Estimated Effects") legend(order(4)) xlabel(1 "-5" 2 "-4" 3 "-3" 4 "-2" 5 "-1" 6 "0" 7 "1" 8 "2" 9 "3" 10 "4", nogrid) ///
	  ylabel(, nogrid) title("Event Study Plot") name(e`k', replace)
} 

* Compare each event study result
graph combine e1 e2 e3 e4, scale(0.8)




