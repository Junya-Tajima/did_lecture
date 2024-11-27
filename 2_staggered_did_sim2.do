/* Install the required packages if they are not already installed
   ssc install reghdfe
   ssc install ftools
   ssc install bacondecomp
   */
************************************************************************************************************************************** 
** Simulation 1: Bacon decomposition (2021) and Jakiela's diagnosis (2021) (Staggered DID + Heterogeneous/Dynamic Effects)
** N = 30000
** There are 4 groups in the sample
** Treatment begins at 6th(group1), 12th(group2), and 18th year(group3)
** Group4 is never-treated
** y = rnormal(3, 2^2) + year + rnormal(9*(year - each timing), 2^2)*treat*post for group1
** y = rnormal(3, 2^2) + year + rnormal(3*(year - each timing), 2^2)*treat*post for group2
** y = rnormal(3, 2^2) + year + rnormal(1*(year - each timing), 2^2)*treat*post for group3
** Run the DID regression
************************************************************************************************************************************** 
***Create the data
*clear the data
clear

*Set the number of observations to 3000
set obs 30000

*Set the seed for reproducibility
set seed 111

*Create the panel data
gen id = _n
gen num = _n
gen year = 1

forvalues k = 1000(1000)29000{
	replace id = id - 1000 if num > `k'
}

forvalues t = 1000(1000)29000{
	replace year = year + 1 if num > `t'
}

*Specify the group based on the ID variable
gen group = 0
replace group = 1 if id < = 250
replace group = 2 if id > 250 & id <= 500
replace group = 3 if id > 500 & id <= 750
replace group = 4 if id > 750 & id <= 1000

*Create the treatment dummy variable for each group
gen D = 0
replace D = 1 if group == 1 & year >= 6
replace D = 1 if group == 2 & year >= 12
replace D = 1 if group == 3 & year >= 18

*Create the outcome variable
gen y = rnormal(3, 2^2) + year
replace y = y + rnormal(9*(year - 6), 2^2) if group == 1 & year >= 6
replace y = y + rnormal(3*(year - 12), 2^2) if group == 2 & year >= 12
replace y = y + rnormal(1*(year - 18), 2^2) if group == 3 & year >= 18

*Plot the trends for both groups
bysort group year : egen Outcome = mean(y)
twoway(line Outcome year if group == 1, color(stc1))(line Outcome year if group == 2, color(stc2)) ///
      (line Outcome year if group == 3, color(stc3))(line Outcome year if group == 4, color(stc4)) ///
	  (scatter Outcome year if group == 1, color(stc1))(scatter Outcome year if group == 2, color(stc2))  ///
	  (scatter Outcome year if group == 3, color(stc3))(scatter Outcome year if group == 4, color(stc4)), ///
	  xline(6) xline(12) xline(18) xlabel(, nogrid) ylabel(, nogrid) legend(order(1 "Group1" 2 "Group2" 3 "Group3" 4 "Group4"))

***Bacon decomposition (2021)
*Set the panel data structure using id as the group variable and year as the time variable
xtset id year

*Conduct Bacon-decomposition
bacondecomp y D, ddetail msymbols(oh t) 

***Jakiela's diagnosis (2021)
*Residualize the treatment dummy by controlling for fixed effects
qui: reg D i.year i.id
predict D_resid, resid

*Residualize the outcome variable by controlling for fixed effects
qui: reg y i.year i.id
predict y_resid, resid

*Plot the relationship between the residualized treatment dummy and the residualized outcome variable
twoway (scatter y_resid D_resid if D == 0, color(stc2%40))(scatter y_resid D_resid if D == 1,  color(stc1%40)) ///
       (lfit y_resid D_resid if D == 0, color(stc2)) (lfit y_resid D_resid if D == 1, color(stc1)), ///
	   xtitle("D residual") ytitle("Y residual") title("Heterogeneous/Dynamic Effect") ///
	   legend(order(1 "Treated" 2 "Control")) xlabel(, nogrid) ylabel(, nogrid)

************************************************************************************************************************************** 
** Simulation 2: Bacon decomposition (2021) and Jakiela's diagnosis (2021) (Staggered DID + Homogeneous/No Dynamic Effects)
** N = 30000
** There are 4 groups in the sample
** Treatment begins at 6th(group1), 12th(group2), and 18th year(group3)
** Group4 is never-treated
** y = rnormal(3, 2^2) + year + rnormal(3, 2^2)*treat*post 
** Run the DID regression
************************************************************************************************************************************** 
***Create the data
*clear the data
clear

*Set the number of observations to 3000
set obs 30000

*Set the seed for reproducibility
set seed 222

*Create the panel data
gen id = _n
gen num = _n
gen year = 1

forvalues k = 1000(1000)29000{
	replace id = id - 1000 if num > `k'
}

forvalues t = 1000(1000)29000{
	replace year = year + 1 if num > `t'
}

*Specify the group based on the ID variable
gen group = 0
replace group = 1 if id < = 250
replace group = 2 if id > 250 & id <= 500
replace group = 3 if id > 500 & id <= 750
replace group = 4 if id > 750 & id <= 1000

*Create the treatment dummy variable for each group
gen D = 0
replace D = 1 if group == 1 & year >= 6
replace D = 1 if group == 2 & year >= 12
replace D = 1 if group == 3 & year >= 18

*Create the outcome variable
gen y = rnormal(3, 2^2) + year
replace y = y + rnormal(3, 2^2) if group == 1 & year >= 6
replace y = y + rnormal(3, 2^2) if group == 2 & year >= 12
replace y = y + rnormal(3, 2^2) if group == 3 & year >= 18

*Plot the trends for both groups
bysort group year : egen Outcome = mean(y)
twoway(line Outcome year if group == 1, color(stc1))(line Outcome year if group == 2, color(stc2)) ///
      (line Outcome year if group == 3, color(stc3))(line Outcome year if group == 4, color(stc4)) ///
	  (scatter Outcome year if group == 1, color(stc1))(scatter Outcome year if group == 2, color(stc2))  ///
	  (scatter Outcome year if group == 3, color(stc3))(scatter Outcome year if group == 4, color(stc4)), ///
	  xline(6) xline(12) xline(18) xlabel(, nogrid) ylabel(, nogrid) legend(order(1 "Group1" 2 "Group2" 3 "Group3" 4 "Group4"))

***Bacon decomposition (2021)
*Set the panel data structure using id as the group variable and year as the time variable
xtset id year

*Conduct Bacon-decomposition
bacondecomp y D, ddetail msymbols(oh t) 

***Jakiela's diagnosis (2021)
*Residualize the treatment dummy by controlling for fixed effects
qui: reg D i.year i.id
predict D_resid, resid

*Residualize the outcome variable by controlling for fixed effects
qui: reg y i.year i.id
predict y_resid, resid

*Plot the relationship between the residualized treatment dummy and the residualized outcome variable
twoway (scatter y_resid D_resid if D == 0, color(stc2%40))(scatter y_resid D_resid if D == 1,  color(stc1%40)) ///
       (lfit y_resid D_resid if D == 0, color(stc2)) (lfit y_resid D_resid if D == 1, color(stc1)), ///
       xtitle("D residual") ytitle("Y residual") title("Homogeneous/No Dynamic Effect") ///
       legend(order(1 "Treated" 2 "Control")) xlabel(, nogrid) ylabel(, nogrid)
