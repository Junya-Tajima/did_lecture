/* Install the required packages if they are not already installed
   ssc install reghdfe
   ssc install ftools
   */

**************************************************************************************************************************
** Simulation: Analyze a staggered DID situation using the method from Wooldridge (2021) 
** Create panel data
** ID ranges from 1 to 3000
** There are 30 periods in the sample
** There are 3 groups in the sample
** Group 1 receives treatment in year 8, Group 2 in year 16, and Group 3 in year 24
** The treatment effect varies by group
** The treatment effect increases over time
**************************************************************************************************************************

**************************************************************************************************************************
** Data Creation
**************************************************************************************************************************
* Clear the data
clear

* Set the number of observations
set obs 90000

* Create the balanced panel data
* 3000 units and 30 periods
gen id = _n
gen N = _n
gen year = 1

forvalues k = 3000(3000)87000{
	replace id = id - 3000 if N > `k'
}

forvalues t = 3000(3000)87000{
	replace year = year + 1 if N > `t'
}

* Construct three dummy variables where each takes the value of 1 for a specific group
gen g1 = (id <= 1000)
gen g2 = (id > 1000 & id <= 2000)
gen g3 = (id > 2000 & id <= 3000)

* Create a variable that assigns numeric values to each group
gen group = 0
replace group = 1 if g1 == 1
replace group = 2 if g2 == 1
replace group = 3 if g3 == 1

* Create a treatment dummy variable
* Each group receives the treatment at a different timing
gen D = 0
replace D = 1 if year >= 8 & g1 == 1
replace D = 1 if year >= 16 & g2 == 1
replace D = 1 if year >= 24 & g3 == 1

* Create the outcome variable 
* Set heterogeneous and dynamic effects 
gen Y = rnormal(3, 0.5^2)
replace Y = Y + rnormal(10*(year - 7), 2^2)*D if g1 == 1
replace Y = Y + rnormal(5*(year - 15), 2^2)*D if g2 == 1
replace Y = Y + rnormal(1*(year - 23), 2^2)*D if g3 == 1

* Create a variable indicating the number of years since treatment 
* Set all pre-treatment periods to 0
gen S1 = 0
gen S2 = 0
gen S3 = 0

replace S1 = year - 7 if year >= 8
replace S2 = year - 15 if year >= 16
replace S3 = year - 23 if year >= 24

**************************************************************************************************************************
** Trends Plot
**************************************************************************************************************************
bysort group year: egen Y_mean = mean(Y)
twoway(line Y_mean year if group == 1, color(stc1))(line Y_mean year if group == 2, color(stc2)) ///
      (line Y_mean year if group == 3, color(stc3)) (scatter Y_mean year if group == 1, color(stc1)) ///
	  (scatter Y_mean year if group == 2, color(stc2))(scatter Y_mean year if group == 3, color(stc3)) ///
      , legend(order(1 "Group1" 2 "Group2" 3 "Group3")) xlabel(, nogrid) ylabel(, nogrid)

**************************************************************************************************************************
** Two-way fixed effects estimator
**************************************************************************************************************************
reghdfe Y D, abs(id year)

**************************************************************************************************************************
** Wooldridge(2021)
**************************************************************************************************************************
reghdfe Y i.g1##i.S1 i.g2##i.S2 i.g3##i.S3, abs(id year)

* Perhaps we should drop periods where proper comparisons cannot be made
drop if year >= 24
reghdfe Y i.g1##i.S1 i.g2##i.S2 i.g3##i.S3, abs(id year)



