/* Install the required packages if they are not already installed
   ssc install reghdfe
   ssc install ftools
   */

********************************************************************************************************************
** Data Analysis : The Impact of Corporate Governance Reforms on Distribution Inequality : Micro Evidence from Japan
** Data File : distribution.dta 
** Data Source : Nikkei NEEDS FinancialQuest, Japan Company Handbook CD-ROM, and Nikkei NEEDS Labor Situation
********************************************************************************************************************

********************************************************************************************************************
** Main Analysis using Difference-in-differences estimation
********************************************************************************************************************
*** Import and create the data
** Set your working directoy
cd ""

** Import the data
use "distribution.dta", clear

** Drop firms from Nagoya, Fukuoka, and Sapporo exchanges
drop if market == 5 | market == 6 | market == 7

** Generate the indicator variable which equals to one for treatment firms
generate treat = (market == 1 | market == 2)

** Generate the indicator variable  which equals to one for post-treatment periods
generate post = (year >= 2016)

** Generate the treatment dummy variable
generate D = treat*post

** Generate the outcome variable
generate ratio = (div + rep)/(wage*emp)

** Save the processed data
save "Processed_Data.dta", replace

*** Check the data and variables
** Summarize variables
summarize rep div wage emp ratio

** Summarize shareholder-worker ratio in detail
summarize ratio, detail

** Create a histogram of the ratio for the treatment group and the control group in the pre-treatment period
twoway(histogram ratio if treat == 1 & post == 0, color(stc1%70))(histogram ratio if treat == 0 & post == 0, color(stc2%70)) ///
      , legend(order(1 "Treatment" 2 "Control")) xlabel(, nogrid) ylabel(, nogrid)

** Create a histogram of the ratio for the treatment group and the control group in the pre-treatment period (excluding firms whose the ratio exceeds 10)
twoway(histogram ratio if treat == 1 & post == 0 & ratio <= 10 & ratio >= 0, color(stc1%70)) ///
      (histogram ratio if treat == 0 & post == 0 & ratio <= 10 & ratio >= 0, color(stc2%70)) ///
	  , legend(order(1 "Treatment" 2 "Control")) xlabel(, nogrid) ylabel(, nogrid)

** Plot the means for the four groups: pre- and post-treatment for both treatment and control firms
bysort treat post: egen ratio_mean = mean(ratio)

twoway(line ratio_mean post if treat == 1, color(stc1))(line ratio_mean post if treat == 0, color(stc2)) ///
      (scatter ratio_mean post if treat == 1, color(stc1) msize(large))(scatter ratio_mean post if treat == 0, color(stc2) msize(large) msymbol(triangle)) ///
	  , xline(0.5) legend(order(3 "Treatment" 4 "Control")) xtitle("POST") ytitle("Shareholder-Worker Ratio") xlabel(#2, nogrid) ylabel(, nogrid)

** Plot the difference-in-differences estimate
sum ratio_mean if treat == 1 & post == 0
generate ratio_treat_pre = r(mean)

sum ratio_mean if treat == 0 & post == 0
generate ratio_cont_pre = r(mean)

generate diff = ratio_treat_pre - ratio_cont_pre
gen ratio_mean_plus_prediff = ratio_mean + diff if treat == 0

twoway(line ratio_mean post if treat == 1, color(stc1))(line ratio_mean_plus_prediff post if treat == 0, color(stc2)) ///
      (scatter ratio_mean post if treat == 1, color(stc1) msize(large))(scatter ratio_mean_plus_prediff post if treat == 0, color(stc2) msize(large) msymbol(triangle)) ///
	  , xline(0.5) legend(order(3 "Treatment" 4 "Control")) xtitle("POST") ytitle("Shareholder-Worker Ratio") xlabel(#2, nogrid) ylabel(, nogrid)


*** Estimating the effects of the corporate governance reform on distrubution inequality
** Difference-in-differences estimator using two-way fixed effects
reghdfe ratio D, abs(id year) vce(cl id) nocons

** Check the mean of the ratio for the treatment group in the pre-treatment period
summarize ratio if treat == 1 & post == 0

*** Pre-trennds checks
** Trends plot
* Calculate the annual mean of the outcome variable separately for the treatment group and the control group
bysort treat year: egen ratio_mean_year = mean(ratio)

* Plotting trends of the outcome variable
twoway(line ratio_mean_year year if treat == 1 & year <= 2014, color(black))(line ratio_mean_year year if treat == 0 & year <= 2014, color(black) lp(dash)) ///
      (line ratio_mean_year year if treat == 1 & year >= 2016, color(black))(line ratio_mean_year year if treat == 0 & year >= 2016, color(black) lp(dash)) ///
	  (scatter ratio_mean_year year if treat == 1 & year <= 2014, color(stc1) msymbol(circle) msize(large)) ///
	  (scatter ratio_mean_year year if treat == 0 & year <= 2014, color(stc2) msymbol(triangle) msize(large)) ///
	  (scatter ratio_mean_year year if treat == 1 & year >= 2016, color(stc1) msymbol(circle) msize(large)) ///
	  (scatter ratio_mean_year year if treat == 0 & year >= 2016, color(stc2) msymbol(triangle) msize(large)) ///
	  , xline(2015) xtitle("Year") ytitle("Shareholder-Worker Ratio") xlabel(, nogrid) ylabel(, nogrid) legend(order(5 "Treatment" 8 "Control")) 

** Placebo tests
* Prepare empty vectors to store the estimated values
matrix define B = J(4, 1, .)
matrix define SE = J(4, 1, .)

* Conduct a placebo test for each pre-treatment year
forvalues k = 1(1)4{

use "Processed_Data.dta", clear
drop if post == 1
gen placebopost = (year >= 201`k')
gen PLACEBO = treat*placebopost

reghdfe ratio PLACEBO, abs(id year) vce(cl id) nocons

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
     , xtitle("Placebo Year") xline(-1) yline(0, lcolor(black)) ytitle("Estimated Effects") legend(order(4)) xlabel(1 "2011" 2 "2012" 3 "2013" 4 "2014", nogrid) ///
	   ylabel(, nogrid) title("Placebo Tests")

** Event Study
* Import the processed data
use "Processed_Data", clear

* Generate the time variable which defines event time
gen time = 0
replace time = year - 2009 if year <= 2014
replace time = year - 2010 if year >= 2016

* Two-way fixed effects estimator allowing for dynamic effects
reghdfe ratio treat##ib5.time, abs(id year) vce(cl id)

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

* Plotting event study results with different baselines
forvalues k = 1(1)4{

use "Processed_Data", clear

gen time = 0
replace time = year - 2009 if year <= 2014
replace time = year - 2010 if year >= 2016

reghdfe ratio treat##ib`k'.time, abs(id year) vce(cl id) 

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

twoway(rspike citop cibottom time, lwidth(vvvthick) mcolor(gray%0) lcolor(gray%25) fcolor(gray%0)) ///
      (sc coef time, color(stc2) msize(large) msymbol(diamond)), xtitle("Event Time") xline(`k') yline(0, lcolor(black)) ///
	  ytitle("Estimated Effects") legend(order(4)) xlabel(1 "-5" 2 "-4" 3 "-3" 4 "-2" 5 "-1" 6 "0" 7 "1" 8 "2" 9 "3" 10 "4", nogrid) ///
	  ylabel(, nogrid) title("Event Study Plot") name(e`k', replace)
} 

* Compare each event study result
graph combine e1 e2 e3 e4, scale(0.8)

*** Testing common shock assumption
** Import the data 
use "Processed_Data.dta", clear

** Create a histogram of inst for the treatment group and the control group in the pre-treatment period
twoway(histogram inst if treat == 1 & post == 0, color(stc1%70))(histogram inst if treat == 0 & post == 0, color(stc2%70)) ///
      , legend(order(1 "Treatment" 2 "Control")) xlabel(, nogrid) ylabel(, nogrid) title("INST")

** Create a histogram of roe for the treatment group and the control group in the pre-treatment period
twoway(histogram roe if treat == 1 & post == 0, color(stc1%70))(histogram roe if treat == 0 & post == 0, color(stc2%70)) ///
      , legend(order(1 "Treatment" 2 "Control")) xlabel(, nogrid) ylabel(, nogrid) title("ROE")

** Difference-in-differneces estimator
reghdfe ratio D inst roe, abs(id year) vce(cl id)

** Event study plot controlling for confounding events

gen time = 0
replace time = year - 2009 if year <= 2014
replace time = year - 2010 if year >= 2016

reghdfe ratio treat##ib5.time inst roe, abs(id year) vce(cl id)

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

twoway(rspike citop cibottom time, lwidth(vvvthick) mcolor(gray%0) lcolor(gray%25) fcolor(gray%0)) ///
      (sc coef time, color(stc2) msize(large) msymbol(diamond)), xtitle("Event Time") xline(5) yline(0, lcolor(black)) ///
	  ytitle("Estimated Effects") legend(order(4)) xlabel(1 "-5" 2 "-4" 3 "-3" 4 "-2" 5 "-1" 6 "0" 7 "1" 8 "2" 9 "3" 10 "4", nogrid) ///
	  ylabel(, nogrid) title("Controlling for Confounding Events")


*** Testing no spillover assumption
** Import the data
use "distribution.dta", clear

** Drop firms from 1st and 2nd sections of TSE and Nagoya stock exchange
drop if market == 1 | market == 2 | market == 5

** Generate the outcome variable
generate ratio = (div + rep)/(wage*emp)

** Generate the indicator variable which equals to one for placebo treatment firms
gen treat = (market == 3 | market == 4)

** Generate the placebo treatment dummy variable
gen placebo =  treat*(year >= 2016)

** Difference-in-differences estimation using placebo sample
reghdfe ratio placebo, abs(id year) vce(cl id) nocons


** Event study plot using placebo sample
gen time = 0
replace time = year - 2009 if year <= 2014
replace time = year - 2010 if year >= 2016

reghdfe ratio treat##ib5.time, abs(id year) vce(cl id)

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

twoway(rspike citop cibottom time, lwidth(vvvthick) mcolor(gray%0) lcolor(gray%25) fcolor(gray%0)) ///
      (sc coef time, color(stc2) msize(large) msymbol(diamond)), xtitle("Event Time") xline(5) yline(0, lcolor(black)) ///
	  ytitle("Estimated Effects") legend(order(4)) xlabel(1 "-5" 2 "-4" 3 "-3" 4 "-2" 5 "-1" 6 "0" 7 "1" 8 "2" 9 "3" 10 "4", nogrid) ///
	  ylabel(, nogrid) title("Testing No Spillover Assumption")

********************************************************************************************************************
** Mechanism Analysis using  Difference-in-differences estimation
********************************************************************************************************************
*** Examine the driver 
** Import the processed data
use "Processed_Data", clear

** Generate the outcome variables 
gen payout = (div + rep)/ta
gen logwages = log(wage*emp)

** Difference-in-differences estimator
reghdfe payout D, abs(id year) vce(cl id)
reghdfe logwages D, abs(id year) vce(cl id)












