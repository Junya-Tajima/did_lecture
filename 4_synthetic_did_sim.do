/* Install the required package if it is not already installed
   ssc install sdid
   */

**********************************************************************************************************************************************************
** Simulation 1 : Assess the prediction accuracy of three estimation methods (DID, SCM, and SDID)
** Use prefectural data from 1975 to 2007 obtained from e-Stat
** The outcome variable is the logarithmic transformation of the number of marriages
** A prefecture is randomly assigned to a pseudo-treatment group with a 20% probability
** The post-treatment period starts from a randomly selected year between 1976 and 2007
** Perform SDID, SC, and DID, and calculate and store the estimated ATT for each method
** Repeat the above process 1000 times and compute the RMSE
**********************************************************************************************************************************************************
* Download the data
use "marriage.dta", clear

* Confirm that the panel data is balanced
sum 
tab pref 
tab year

*** Create an empty vector for storing 1000 estimated values
matrix define SDID_DIFFERENCE_SQUARED = J(1000, 1, .)
matrix define SC_DIFFERENCE_SQUARED = J(1000, 1, .)
matrix define DID_DIFFERENCE_SQUARED = J(1000, 1, .)

* Set the seed 
set seed 999

*** Estimate and store 1000 squared differences
forvalues k = 1(1)1000{

** Download the data
use "marriage.dta", clear

** Creating the outcome variable
gen logy = log(marriage)

** Generate a dummy variable that takes the value 1 with a 20% probability
gen treat_random = rbinomial(1, 0.2)

** Determine treatment and control groups according to the value of treat_random in 1975
by pref, sort : gen treat = treat_random if year == 1975

** Impute missing values
sort pref year
by pref, sort : replace treat = treat[1] if missing(treat)

** Choose a treatment start year randomly from the range 1976 to 2022
generate treatment_year = runiformint(1976, 2007)
replace treatment_year = treatment_year[1] 


** Generate a indicator variable equal to 1 in the post-tretament period
gen post = (year >= treatment_year)

** Generate a treatment dummy variable 
gen D = treat*post 

** Synthetic difference-in-differences estimation
* Conduct SDID estimation
sdid logy pref year D, method(sdid) vce(noinference)

* Store the squared difference
matrix SDID_DIFFERENCE_SQUARED[`k', 1] = (e(ATT))^2

** Synthetic control methos
* Conduct SCM estimation
sdid logy pref year D, method(sc) vce(noinference)

* Store the squared difference
matrix SC_DIFFERENCE_SQUARED[`k', 1] = (e(ATT))^2

** Difference-in-differences estimation
* Conduct DID estimation
sdid logy pref year D, method(did) vce(noinference)

* Store the squared difference 
matrix DID_DIFFERENCE_SQUARED[`k', 1] = (e(ATT))^2

}

*** Calculate and compare RMSEs of three different estimation methods
** Clear the data
clear

** Set the number of observations to 1000
set obs 1000

** Generate three empty variables to store 1000 squared differences estimated from three estimation methods
gen SDID_DIFFERENCE_SQUARED = .
gen SC_DIFFERENCE_SQUARED = .
gen DID_DIFFERENCE_SQUARED = .

** Convert the 1000 squared differences stored in the vector into a variable
gen count = _n
forvalues s = 1(1)1000{

replace SDID_DIFFERENCE_SQUARED = SDID_DIFFERENCE_SQUARED[`s',1] if count == `s'
replace SC_DIFFERENCE_SQUARED = SC_DIFFERENCE_SQUARED[`s',1] if count == `s'
replace DID_DIFFERENCE_SQUARED = DID_DIFFERENCE_SQUARED[`s',1] if count == `s'

}

** Compute the Mean Squared Error (MSE)
egen SDID_MSE = mean(SDID_DIFFERENCE_SQUARED)
egen SC_MSE = mean(SC_DIFFERENCE_SQUARED)
egen DID_MSE = mean(DID_DIFFERENCE_SQUARED)

** Compute the Root Mean Squared Error (RMSE)
gen SDID_RMSE = sqrt(SDID_MSE)
gen SC_RMSE = sqrt(SC_MSE)
gen DID_RMSE = sqrt(DID_MSE)

** Preserve one observation and drop the rest from the dataset
keep if count == 1

** Keep RMSEs from the three estimation methods and drop the rest
keep SDID_RMSE SC_RMSE DID_RMSE

**********************************************************************************************************************************************************
** Simulation 2 : Assess the prediction accuracy of three estimation methods while considering the influence of fixed effects on treatment assignment
** Use prefectural data from 1975 to 2007 obtained from e-Stat
** The outcome variable is the logarithmic transformation of the number of marriages
** Estimate prefecture and year fixed effects using regression 
** The treatment assignment probability is set to increase with higher prefecture fixed effects
** The treatment start year is selected from one of the three years with the smallest year fixed effects
** Perform SDID, SC, and DID, and calculate and store the estimated ATT for each method
** Repeat the above process 1000 times and compute the RMSE
**********************************************************************************************************************************************************

*** Create an empty vector for storing 1000 estimated values
matrix define SDID_DIFFERENCE_SQUARED = J(1000, 1, .)
matrix define SC_DIFFERENCE_SQUARED = J(1000, 1, .)
matrix define DID_DIFFERENCE_SQUARED = J(1000, 1, .)

*** Set the seed
set seed 888

*** Estimate and store 1000 squared differences
forvalues k = 1(1)1000{

** Download the data
use "marriage.dta", clear

** Creating the outcome variable
gen logy = log(marriage)

** Convert a string variable into a numeric variable 
encode pref, gen(pref2)

** Run the regression to estimate prefecture and year fixed effetcs
reg logy i.pref2 i.year

** Generate a variable to store the estimated prefecture fixed effects
gen pref_fe = .
forvalues I = 1(1)47{
	replace pref_fe = e(b)[1, `I'] if pref2 == `I'
}

** Generate a variable to store the estimated year fixed effects
gen year_fe = .
forvalues T = 48(1)80{
	replace year_fe = e(b)[1, `T'] if year == `T' + 1927
}

** Set the treatment assignment probability to be determined by the prefecture fixed effects
* Generate a variable p ranging from 0 to 1 depending on prefecture fixed effects
gen p = exp(pref_fe)/(1+exp(pref_fe))

* Generate a dummy variable with a value of 1 based on probability p
gen treat_assign = rbinomial(1, p)

* Determine the treatment and control groups based on the value of treat_assign in 1975
by pref, sort : gen treat = treat_assign if year == 1975

* Impute missing values
sort pref year
by pref, sort : replace treat = treat[1] if missing(treat)

** Select the treatment start year from the top three years with the lowest year fixed effects
* Sort the data by year fixed effects in ascending order
sort year_fe

* Generate a variable that randomly selects a value from 1 to 141 (47*3 = 141)
gen r = runiformint(1, 141)

* Set the treatment_year to be one of the three years with the lowest year fixed effects
gen treatment_year = year[r]

* Set the treatment start year for the sample to the value of treat_year from the first row
replace treatment_year = treatment_year[1] 

* Generate a indicator variable equal to 1 in the post-tretament period
gen post = (year >= treatment_year)

* Generate a treatment dummy variable 
gen D = treat*post 

** Synthetic difference-in-differences estimation
* Conduct SDID estimation
sdid logy pref year D, method(sdid) vce(noinference)

* Store the squared difference
matrix SDID_DIFFERENCE_SQUARED[`k', 1] = (e(ATT))^2

** Synthetic control methos
* Conduct SCM estimation
sdid logy pref year D, method(sc) vce(noinference)

* Store the squared difference
matrix SC_DIFFERENCE_SQUARED[`k', 1] = (e(ATT))^2

** Difference-in-differences estimation
* Conduct DID estimation
sdid logy pref year D, method(did) vce(noinference)

* Store the squared difference 
matrix DID_DIFFERENCE_SQUARED[`k', 1] = (e(ATT))^2

}

*** Calculate and compare RMSEs of three different estimation methods
** Clear the data
clear

** Set the number of observations to 1000
set obs 1000

** Generate three empty variables to store 1000 squared differences estimated from three estimation methods
gen SDID_DIFFERENCE_SQUARED = .
gen SC_DIFFERENCE_SQUARED = .
gen DID_DIFFERENCE_SQUARED = .

** Convert the 1000 squared differences stored in the vector into a variable
gen count = _n
forvalues s = 1(1)1000{

replace SDID_DIFFERENCE_SQUARED = SDID_DIFFERENCE_SQUARED[`s',1] if count == `s'
replace SC_DIFFERENCE_SQUARED = SC_DIFFERENCE_SQUARED[`s',1] if count == `s'
replace DID_DIFFERENCE_SQUARED = DID_DIFFERENCE_SQUARED[`s',1] if count == `s'

}

** Compute the Mean Squared Error (MSE)
egen SDID_MSE = mean(SDID_DIFFERENCE_SQUARED)
egen SC_MSE = mean(SC_DIFFERENCE_SQUARED)
egen DID_MSE = mean(DID_DIFFERENCE_SQUARED)

** Compute the Root Mean Squared Error (RMSE)
gen SDID_RMSE = sqrt(SDID_MSE)
gen SC_RMSE = sqrt(SC_MSE)
gen DID_RMSE = sqrt(DID_MSE)

** Preserve one observation and drop the rest from the dataset
keep if count == 1

** Keep RMSEs from the three estimation methods and drop the rest
keep SDID_RMSE SC_RMSE DID_RMSE



