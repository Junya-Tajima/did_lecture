/* Install the required packages if they are not already installed
   ssc install reghdfe 
   ssc install ftools
   ssc install sdid
   ssc install sdid_event
   */

********************************************************************************************************************
** Data Analysis : Analyze the impact of the socialist market economy on economic growth in China
** Data File : SME.dta 
** Data Source : World Bank Open Data
********************************************************************************************************************

* Clean environment
clear all
macro drop _all
set more off

* Set the working directly
cd ""

* Download the data
use "SME.dta", clear

* Display the unique country names in the dataset
tab country

* Display the years in the dataset
tab year

* Generate a indicator variable equal to 1 if the country's name is China
gen treat = (country == "China")

* Generate a treatment dummy variable
gen D = (country == "China" & year >= 1992)

* Generate the outcome variable
gen loggdppc = log(gdppc)

* Summarize tha data
summarize loggdppc gdppc

* Create the histogram of the outcome variable 
twoway (hist loggdppc if year <= 1991, color(stc1)), xlabel(, nogrid) ylabel(, nogrid)

* Create histograms of the outcome variable before treatment
* Separate histograms for treatment group and control group
twoway (hist loggdppc if year <= 1991 & treat == 1, color(stc1%50)) ///
      (hist loggdppc if year <= 1991 & treat == 0, color(stc2%50)) ///
       ,xlabel(, nogrid) ylabel(, nogrid) legend(order(1 "Treat" 2 "Control"))

* Generate histograms of the outcome variable in the year 1991
* Separate histograms for treatment group and control group 
twoway (hist loggdppc if year == 1991 & treat == 1, color(stc1%50)) ///
       (hist loggdppc if year == 1991 & treat == 0, color(stc2%50)) ///
        ,xlabel(, nogrid) ylabel(, nogrid) legend(order(1 "Treat" 2 "Control"))

* Calculate the mean of the outcome variable for treatment and control groups separately
bysort treat year: egen y_mean = mean(loggdppc)

* Plot the trend of the outcome variable over time for treatment and control groups separately
twoway(line y_mean year if treat == 1, color(stc2)) (line y_mean year if treat == 0, color(stc1)) ///
(scatter y_mean year if treat == 1, color(stc2)) (scatter y_mean year if treat == 0, color(stc1)) ///
, xlabel(, nogrid) ylabel(, nogrid) xline(1992) legend(order(3 "Treat" 4 "Control"))

* Save the processed data
save "processed.dta", replace

********************************************************************************************************************
*** Differece-in-differences estimation
******************************************************************************************************************** 
* Conduct difference-in-differences estimation
reghdfe loggdppc D, abs(country year) vce(cl country) nocons

* Conduct event study difference-in-differences analysis
* Conduct event study estimation using two-way fixed effects estimator
reghdfe loggdppc treat##ib1991.year, abs(country year) vce(cl country)

* Create empty variables
gen coef = .
gen se = .

* Loop over each year and calculate the event study estimate, storing it in the created variables
forvalues i = 1960(1)2022{
	replace coef = _b[1.treat#`i'.year] if year == `i'
    replace se = _se[1.treat#`i'.year] if year ==`i'
}

* Calculate the 95% confidence interval for the event study estimates
gen citop = coef + 1.96*se
gen cibottom = coef - 1.96*se

* Keep only the variables necessary for the event study plot
keep year coef se citop cibottom 

* Remove duplicate observations 
duplicates drop

* Sort the data by year in ascending order
sort year

* Plot the event study results with 95% confidence intervals
twoway(rarea citop cibottom year, lwidth(thick) mcolor(gray%0) lcolor(gray%0) fcolor(gray%25)) ///
      (sc coef year, color(stc2) msize(small) msymbol(circle)), xtitle("Event Time") xline(1991) ///
	  yline(0, lcolor(black)) ytitle("Estimated Effects") legend(order(4)) xlabel(, nogrid) ylabel(, nogrid) ///
	  title("Event Study Plot")

********************************************************************************************************************
*** Synthetic differece-in-differences estimation
********************************************************************************************************************
* Restore the processed data
use "processed.dta", clear

* Conduct synthetic difference-in-differences estimation
sdid loggdppc country year D, vce(placebo) seed(111) reps(200) method(sdid) g1on graph 

* Conduct synthetic difference-in-differences estimation
sdid loggdppc country year D, vce(placebo) seed(222) reps(200) method(did) g1on graph 

********************************************************************************************************************
*** Placebo test 
********************************************************************************************************************
*Restore the processed data
use "processed.dta", clear

* Drop the data from the post-treatment period
drop if year >= 1992

* Generate a placebo treatment dummy variable
gen Placebo = (country == "China" & year >= 1976)

* SDID estimation
sdid loggdppc country year Placebo, vce(placebo) seed(333) reps(200) method(sdid) g1on graph 

********************************************************************************************************************
***Event Study Plot 
********************************************************************************************************************
* Restore the processed data
use "processed.dta", clear 

* Set the seed for random number generation to ensure reproducibility
set seed 123
 
* Conduct event study analysis with synthetic difference-in-differences
sdid_event loggdppc country year D ,placebo(all)  vce(placebo)  method(sdid) brep(200) 

* Preserve post-treatment and post-treatment effects in the matrix
matrix define H = e(H)
 
* Prepare empty vectors to store the estimated values
matrix define B = J(63, 1, .)
matrix define SE = J(63, 1, .)

* Put each effect stored in a matrix into vectors
forvalues k = 1(1)32{
	matrix define B[`k', 1] = H[`k' + 32, 1]
}

forvalues k = 1(1)31{
	matrix define B[`k' + 32, 1] = H[`k' + 1 , 1]
}

forvalues k = 1(1)32{
	matrix define SE[`k', 1] = H[`k' + 32, 2]
}

forvalues k = 1(1)31{
	matrix define SE[`k' + 32, 1] = H[`k' + 1 , 2]
}

* Clear the data
clear

* Set the number of observations to 63
* This is the number of pre-treatment and post-treatment effects
set obs 63

* Set the number from 1 to 63
* This indicates each period
gen time = _n

* Create empty variables
gen b = .
gen se = .

* Put each effect stored in vectors into empty variables
forvalues t = 1(1)63{
	replace b = B[`t', 1] if time == `t'
}

forvalues t = 1(1)63{
	replace se = SE[`t', 1] if time == `t'
}

* Calculate 95% confidence intervals
gen citop = b + 1.96*se
gen cibottom = b - 1.96*se

* Leave only the variables required for the event study plot
keep time b se citop cibottom 

* Re-arrange the data in period order
sort time

*P lot the results of event study
twoway(rarea citop cibottom time, lwidth(thick) mcolor(gray%0) lcolor(gray%0) fcolor(gray%25))(sc b time, color(stc2) ///  
      msize(small) msymbol(circle)), xtitle("Event Time") xline(33) yline(0, lcolor(black)) ytitle("Estimated Effects") ///
      legend(order(4)) xlabel(, nogrid) ylabel(, nogrid) title("Event Study Plot")








