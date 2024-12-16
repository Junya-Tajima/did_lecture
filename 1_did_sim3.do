/* Install the required packages if they are not already installed
   ssc install reghdfe
   ssc install ftools
   ssc install ppmlhdfe
   */  
 
***********************************************************************************************
** Simulation 1: Poisson Difference-in-differences
** N = 1000000
** id 1:100000
** There are 10 periods in the sample
** There are 2 groups (treatment/control) in the sample
** Treatment begins at 6th period
** Outcome (Y) is non-negative
** We first conduct standard DID estimation using TWFE estimator
** We next conduct poisson DID estimation
***********************************************************************************************

***********************************************************************************************
** Data Creation
***********************************************************************************************
* Clean environment
clear all
macro drop _all
set more off

* Set the number of observations to 1000000
set obs 1000000

* Create the panel data
* Generate panel data for 100000 individuals over 10 periods
gen num = _n
gen id = _n
gen time = 1

forvalues k = 100000(100000)900000{
	replace id = id - 100000 if num > `k'
}

forvalues t = 100000(100000)900000{
	replace time = time + 1 if num > `t'
}

* Create a dummy variable for the treatment group
* Define individuals with id <= 50000 as the treatment group
gen treat = (id <= 50000)

* Create a dummy variable for the post-treatment period
gen post = (time >= 6)

* Create a treatment dummy variable
gen D = treat*post

* Generate the outcome variable
* Y is the non-negative outcome variable
gen Y = int(runiform()*5) + int(runiform()*50)*treat + int(runiform()*5)*post + int(runiform()*5)*D

* Save the processed data as "poisson.dta"
save "poisson.dta", replace

* Plot the trends for treatment and control groups
bysort treat time: egen Y_mean = mean(Y)
twoway(line Y_mean time if treat == 1, color(stc1))(line Y_mean time if treat == 0, color(stc2)) ///
      (scatter Y_mean time if treat == 1, color(stc1)) (scatter Y_mean time if treat == 0, color(stc2)) ///
	  , legend(order(1 "Treatment" 2 "Control")) xline(6)

***********************************************************************************************
** Standard (Linear) Difference-in-Differences Estimation
***********************************************************************************************
reghdfe Y D, abs(id time) vce(cl id)

***********************************************************************************************
** Poisson DID Estimation
***********************************************************************************************
ppmlhdfe Y D, abs(id time) vce(cl id)

***********************************************************************************************
** Event Study DID Estimation
***********************************************************************************************
* Event Study Plot (Linear DID)
use "poisson.dta", clear
reghdfe Y treat##ib5.time, abs(id time) vce(cl id)

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

twoway(rarea citop cibottom time, mcolor(gray%0) lcolor(gray%0) fcolor(gray%25))(line coef time, color(stc1)) ///
      (sc coef time, color(stc1) msize(large) msymbol(diamond)), xtitle("Event Time") xline(5) yline(0, lcolor(black)) ///
	  ytitle("Estimated Effects") legend(order(4)) name(q1, replace) 

* Event Study Plot (Poisson DID)
use "poisson.dta", clear
ppmlhdfe Y treat##ib5.time, abs(id time) vce(cl id)


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

twoway(rarea citop cibottom time, mcolor(gray%0) lcolor(gray%0) fcolor(gray%25))(line coef time, color(stc1)) ///
      (sc coef time, color(stc1) msize(large) msymbol(diamond)), xtitle("Event Time") xline(5) yline(0, lcolor(black)) ///
	  ytitle("Estimated Effects") legend(order(4)) name(q2, replace)

* Compare each event study graph
graph combine q1 q2, scale(0.8)

***********************************************************************************************
** Simulation 2: Difference-in-differences using the log-transformed outcome variable
** N = 1000000
** id 1:500000
** There are 2 periods in the sample
** There are 2 groups (treatment/control) in the sample
** The outcome variable y follows a normal distribution
** We fisrt conduct DID estimation using the level of y
** We next conduct DID estimation using log-transformed y
** Confirm whether two specifications create the same result
***********************************************************************************************

***********************************************************************************************
** Data Creation
***********************************************************************************************
* Clear the data
clear

* Set the number of observations to 1000000
set obs 1000000

* Create the panel data
* Generate panel data for 500000 individuals over 2 periods
gen num =_n
gen id = _n
gen post = 0

replace id = id - 500000 if num > 500000
replace post = 1 if num > 500000

* Create a dummy variable for the treatment group
* Define individuals with ID <= 5000 as the treatment group
gen treat = 0
replace treat = 1 if id <= 5000

* Create a treatment dummy variable
gen D = treat*post

* Generate the outcome variable
gen y = rnormal(20, 0.5)
replace y = rnormal(100, 0.5) if treat == 1
replace y = y + rnormal(10, 0.5) if treat == 0 & post == 1
replace y = y + rnormal(30, 0.5) if treat == 1 & post == 1

* Log-transform the outcome variable
gen logy = log(y)

***********************************************************************************************
** Difference-in-differences estimation using the level of the outcome variable
***********************************************************************************************
reg y D treat post 

***********************************************************************************************
** Difference-in-differences estimation using the log-transformed outcome variable
***********************************************************************************************
reg logy D treat post






