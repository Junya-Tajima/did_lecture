/* Install the required packages if they are not already installed
   ssc install reghdfe
   ssc install ftools
   */
*********************************************************************************************************
** Simulation 1: Treatment at the same time
** N = 30000
** Treatment begins from the 16th year
** y = rnormal(3, 2^2) + year + rnormal(3*(year - 16), 2^2)*treat*post
** Run the DID regression
** True Effect is 21
*********************************************************************************************************
*** Create the data
* Clear the data
clear

* Set the number of observations to 30000
set obs 30000

* Set the seed for reproducibility
set seed 1

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

* Create a dummy variable that equals 1 for the treatment group
gen treat = 0
replace treat = 1 if id <= 500

* Create a dummy variable that equals 1 for the post-treatment period
gen post = 0
replace post = 1 if year >= 16

* Create the treatment dummy variable
gen D = treat*post

* Create the outcome variable
* Set the year trend variable that increases by 1 per year
* Set the dynamic effect that increases by 3 each year
gen y = rnormal(3, 2^2) + year
replace y = y + rnormal(3*(year - 16), 2^2) if treat == 1 & post == 1

*** Plot trends for each group
bysort treat year : egen Outcome = mean(y)
twoway(line Outcome year if treat == 1, color(stc1))(line Outcome year if treat == 0, color(stc2)) ///
      (scatter Outcome year if treat == 1, color(stc1))(scatter Outcome year if treat == 0, color(stc2)) ///
	  , xline(16) legend(order(1 "Treat" 2 "Control")) xlabel(, nogrid) ylabel(, nogrid)

*** Run the DID regression 
reghdfe y D, abs(id year) vce(cl id)

*********************************************************************************************************
** Simulation 2: Staggered DID with homogeneous treatment effect
** N = 30000
** There are 4 groups in the sample
** Treatment begins at 6th(group1), 12th(group2), and 18th year(group3)
** Group4 is never-treated
** y = rnormal(3, 2^2) + year + rnormal(3*(year - each timing), 2^2)*treat*post
** Run the DID regression
** True Effect is 28.5
*********************************************************************************************************
*** Create the data
* Clear the data
clear

* Set the number of observations to 30000
set obs 30000

* Set the seed for reproducibility
set seed 2

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
gen y = rnormal(3, 2^2) + year
replace y = y + rnormal(3*(year - 6), 2^2) if group == 1 & year >= 6
replace y = y + rnormal(3*(year - 12), 2^2) if group == 2 & year >= 12
replace y = y + rnormal(3*(year - 18), 2^2) if group == 3 & year >= 18

*** Plot the trends for each group
bysort group year : egen Outcome = mean(y)
twoway(line Outcome year if group == 1, color(stc1))(line Outcome year if group == 2, color(stc2)) ///
      (line Outcome year if group == 3, color(stc3))(line Outcome year if group == 4, color(stc4)) ///
	  (scatter Outcome year if group == 1, color(stc1))(scatter Outcome year if group == 2, color(stc2)) ///
	  (scatter Outcome year if group == 3, color(stc3))(scatter Outcome year if group == 4, color(stc4)) ///
	  , xline(6) xline(12) xline(18) xlabel(, nogrid) ylabel(, nogrid) legend(order(1 "Group1" 2 "Group2" 3 "Group3" 4 "Group4"))

*** Run the DID regression 
reghdfe y D, abs(id year) vce(cl id)


*********************************************************************************************************
** Simulation 3: Staggered DID with heterogenous treatment effects
** N = 30000
** There are 4 groups in the sample
** Treatment begins at 6th(group1), 12th(group2), and 18th year(group3)
** Group4 is never-treated
** y = rnormal(3, 2^2) + year + rnormal(9*(year - each timing), 2^2)*treat*post for group1
** y = rnormal(3, 2^2) + year + rnormal(3*(year - each timing), 2^2)*treat*post for group2
** y = rnormal(3, 2^2) + year + rnormal(1*(year - each timing), 2^2)*treat*post for group3
** Run the DID regression
** True Effect is 49.2
*********************************************************************************************************
*** Create the data
*Clear the data
clear

* Set the number of observations to 30000
set obs 30000

* Set the seed for reproducibility
set seed 3

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
gen y = rnormal(3, 2^2) + year
replace y = y + rnormal(9*(year - 6), 2^2) if group == 1 & year >= 6
replace y = y + rnormal(3*(year - 12), 2^2) if group == 2 & year >= 12
replace y = y + rnormal(1*(year - 18), 2^2) if group == 3 & year >= 18

*** Plot the trends for each group
bysort group year : egen Outcome = mean(y)
twoway(line Outcome year if group == 1, color(stc1))(line Outcome year if group == 2, color(stc2)) ///
      (line Outcome year if group == 3, color(stc3))(line Outcome year if group == 4, color(stc4)) ///
	  (scatter Outcome year if group == 1, color(stc1))(scatter Outcome year if group == 2, color(stc2)) ///
	  (scatter Outcome year if group == 3, color(stc3))(scatter Outcome year if group == 4, color(stc4)) ///
	  , xline(6) xline(12) xline(18) xlabel(, nogrid) ylabel(, nogrid) legend(order(1 "Group1" 2 "Group2" 3 "Group3" 4 "Group4"))

*** Run the DID regression 
reghdfe y D, abs(id year) vce(cl id)

*********************************************************************************************************
** Simulation 4: Staggered DID with heterogenous treatment effects (dropping never-treated)
** N = 30000
** There are 3 groups in the sample
** Treatment begins at 6th(group1), 12th(group2), and 18th year(group3)
** y = rnormal(3, 2^2) + year + rnormal(9*(year - each timing), 2^2)*treat*post for group1
** y = rnormal(3, 2^2) + year + rnormal(3*(year - each timing), 2^2)*treat*post for group2
** y = rnormal(3, 2^2) + year + rnormal(1*(year - each timing), 2^2)*treat*post for group3
** Run the DID regression
** True Effect is 49.2
*********************************************************************************************************
*** Create the data
* Clear the data
clear

* Set the number of observations to 30000
set obs 30000

* Set the seed for reproducibility
set seed 4

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
gen y = rnormal(3, 2^2) + year
replace y = y + rnormal(9*(year - 6), 2^2) if group == 1 & year >= 6
replace y = y + rnormal(3*(year - 12), 2^2) if group == 2 & year >= 12
replace y = y + rnormal(1*(year - 18), 2^2) if group == 3 & year >= 18

* Drop samples that belong to group 4
drop if group == 4

*** Plot the trends for each group
bysort group year : egen Outcome = mean(y)
twoway(line Outcome year if group == 1, color(stc1))(line Outcome year if group == 2, color(stc2)) ///
      (line Outcome year if group == 3, color(stc3))(scatter Outcome year if group == 1, color(stc1)) ///
	  (scatter Outcome year if group == 2, color(stc2))(scatter Outcome year if group == 3, color(stc3)) ///
	  , xline(6) xline(12) xline(18) xlabel(, nogrid) ylabel(, nogrid) legend(order(1 "Group1" 2 "Group2" 3 "Group3"))

*** Run the DID regression 
reghdfe y D, abs(id year) vce(cl id)

*********************************************************************************************************
** Simulation 5: Staggered DID with heterogenous treatment effects and no dynamic effects
** N = 30000
** There are 4 groups in the sample
** Treatment begins at 6th(group1), 12th(group2), and 18th year(group3)
** Group4 is never-treated
** y = rnormal(3, 2^2) + year + rnormal(9, 2^2)*treat*post for group1
** y = rnormal(3, 2^2) + year + rnormal(3, 2^2)*treat*post for group2
** y = rnormal(3, 2^2) + year + rnormal(1, 2^2)*treat*post for group3
** Run the DID regression
** True Effect is 4.3
*********************************************************************************************************
*** Create the data
* Clear the data
clear

* Set the number of observations to 30000
set obs 30000

* Set the seed for reproducibility
set seed 5

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
gen y = rnormal(3, 2^2) + year
replace y = y + rnormal(9, 2^2) if group == 1 & year >= 6
replace y = y + rnormal(3, 2^2) if group == 2 & year >= 12
replace y = y + rnormal(1, 2^2) if group == 3 & year >= 18

*** Plot the trends for each group
bysort group year : egen Outcome = mean(y)
twoway(line Outcome year if group == 1, color(stc1))(line Outcome year if group == 2, color(stc2)) ///
      (line Outcome year if group == 3, color(stc3))(line Outcome year if group == 4, color(stc4)) ///
	  (scatter Outcome year if group == 1, color(stc1))(scatter Outcome year if group == 2, color(stc2)) ///
	  (scatter Outcome year if group == 3, color(stc3))(scatter Outcome year if group == 4, color(stc4)) ///
	  , xline(6) xline(12) xline(18) xlabel(, nogrid) ylabel(, nogrid) legend(order(1 "Group1" 2 "Group2" 3 "Group3" 4 "Group4"))

*** Run the DID regression 
reghdfe y D, abs(id year) vce(cl id)

*********************************************************************************************************
** Simulation 6: Staggered DID with homogeneous treatment effect and no dynamic effects
** N = 30000
** There are 4 groups in the sample
** Treatment begins at 6th(group1), 12th(group2), and 18th year(group3)
** Group4 is never-treated
** y = rnormal(3, 2^2) + year + rnormal(3, 2^2)*treat*post for group1
** y = rnormal(3, 2^2) + year + rnormal(3, 2^2)*treat*post for group2
** y = rnormal(3, 2^2) + year + rnormal(3, 2^2)*treat*post for group3
** Run the DID regression
** True Effect is 3
*********************************************************************************************************
*** Create the data
*Clear the data
clear

* Set the number of observations to 30000
set obs 30000

* Set the seed for reproducibility
set seed 6

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
gen y = rnormal(3, 2^2) + year
replace y = y + rnormal(3, 2^2) if group == 1 & year >= 6
replace y = y + rnormal(3, 2^2) if group == 2 & year >= 12
replace y = y + rnormal(3, 2^2) if group == 3 & year >= 18


*** Plot the trends for each
bysort group year : egen Outcome = mean(y)
twoway(line Outcome year if group == 1, color(stc1))(line Outcome year if group == 2, color(stc2)) ///
      (line Outcome year if group == 3, color(stc3))(line Outcome year if group == 4, color(stc4)) ///
	  (scatter Outcome year if group == 1, color(stc1))(scatter Outcome year if group == 2, color(stc2)) ///
	  (scatter Outcome year if group == 3, color(stc3))(scatter Outcome year if group == 4, color(stc4)) ///
	  , xline(6) xline(12) xline(18) xlabel(, nogrid) ylabel(, nogrid) legend(order(1 "Group1" 2 "Group2" 3 "Group3" 4 "Group4"))

*** Run the DID regression 
reghdfe y D, abs(id year) vce(cl id)




