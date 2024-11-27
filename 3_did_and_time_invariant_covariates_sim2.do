/*Installing packages
ssc install egenmore
ssc install ftools
ssc install reghdfe
ssc install drdid
*/

*************************************************************************************************************
** Simulation 1: Difference-in-differeces with covariates
** N = 20000 (10000 units and two periods)
** 2×2 design (Pre and Post, Treat and Control)
** Parallel Trends Assumption can hold only after conditioning on the pre-treatment level of covariates
** What if we control for the change of covariates using TWFEDD in the above situation
*************************************************************************************************************
*** Create the data
* Clear the data
clear

* Set the number of observations to 20000
set obs 20000

* Create the panel data
* Generate panel data for 1000 individuals over 2 periods
gen num = _n
gen id = _n
gen post = 0

replace id = id - 10000 if num > 10000
replace post = 1 if num > 10000

* Create a dummy variable for the treatment group
* Define individuals with ID <= 3000 as the treatment group
gen treat = 0
replace treat = 1 if id <= 3000

* Create a treatment dummy variable
gen D = treat*post

* Create a time-varing covariate x
* x has different distributions between the treatment group and the control group
gen x = rnormal(2, 0.5) + rnormal(1, 0.5^2)*post
replace x = rnormal(3, 0.5) + rnormal(1.2, 0.5^2)*post if treat == 1

* Make x a time-invariant covariate X
by id, sort : egen X = first(x)

* Create a time-varing covariate w
* w has different distributions between the treatment group and the control group
gen w = rnormal(5, 0.5) + rnormal(2, 0.5^2)*post
replace w = rnormal(1, 0.5) + rnormal(3.5, 0.5^2)*post if treat == 1

* Make w a time-invariant covariate W
by id, sort : egen W = first(w)


* Generate the outcome variable
* The trend of the outcome variable is influenced by W and X
* True treatment effct is 0.5
gen y = 0.2 + 0.1*treat + 0.3*post + rnormal(0.5, 0.5^2)*D + 0.4*X*post + 0.6*W*post + rnormal()

* Plotting the trend of the coutcome variable
bysort treat post: egen y_mean = mean(y)
twoway(line y_mean post if treat == 1)(line y_mean post if treat == 0), ///
      xline(0.5) legend(order(1 "Treat" 2 "Control")) xlabel(, nogrid) ylabel(, nogrid)


*************************************************************************************************************
*** Differece-in-differences estimation without controlling for covariates (Biased)
*************************************************************************************************************
reg y D treat post

*************************************************************************************************************
*** Difference-in-differences estimation controlling for the change of a covariate (Biased)
*************************************************************************************************************
reg y D treat post x w

*************************************************************************************************************
*** Difference-in-differences estimation controlling for the level of pre-treatment covariate (Unbiased)
*************************************************************************************************************
reg y D treat post c.X#i.post c.W#i.post

*************************************************************************************************************
** Simulation 2: Difference-in-differeces with covariates
** N = 20000 (10000 units and two periods)
** 2×2 design (Pre and Post, Treat and Control)
** Parallel Trends Assumption can hold only after conditioning on the change of covariates
** What if we control for the level of covariates in the above situation
*************************************************************************************************************
*** Create the data
* Clear the data
clear

* Set the number of observations to 20000
set obs 20000

* Create the panel data
* Generate panel data for 1000 individuals over 2 periods
gen num = _n
gen id = _n
gen post = 0

replace id = id - 10000 if num > 10000
replace post = 1 if num > 10000

* Create a dummy variable for the treatment group
* Define individuals with ID <= 3000 as the treatment group
gen treat = 0
replace treat = 1 if id <= 3000

* Create a treatment dummy variable
gen D = treat*post

* Create a time-varing covariate x
* x has different distributions between the treatment group and the control group
gen x = rnormal(2, 0.5) + rnormal(1, 0.5^2)*post
replace x = rnormal(3, 0.5) + rnormal(1.2, 0.5^2)*post if treat == 1

* Make x a time-invariant covariate X
by id, sort : egen X = first(x)

* Create a time-varing covariate w
* w has different distributions between the treatment group and the control group
gen w = rnormal(5, 0.5) + rnormal(2, 0.5^2)*post
replace w = rnormal(1, 0.5) + rnormal(3.5, 0.5^2)*post if treat == 1

* Make w a time-invariant covariate W
by id, sort : egen W = first(w)


* Generate the outcome variable
* The trend of the outcome variable is influenced by w and x (changes of covariates)
* True treatment effct is 0.5
gen y = 0.2 + 0.1*treat + 0.3*post + rnormal(0.5, 0.5^2)*D + 0.4*x + 0.6*w + rnormal()

* Plotting the trend of the coutcome variable
bysort treat post: egen y_mean = mean(y)
twoway(line y_mean post if treat == 1)(line y_mean post if treat == 0), ///
      xline(0.5) legend(order(1 "Treat" 2 "Control")) xlabel(, nogrid) ylabel(, nogrid)

*************************************************************************************************************
*** Differece-in-differences estimation without controlling for covariates (Biased)
*************************************************************************************************************
reg y D treat post

*************************************************************************************************************
*** Difference-in-differences estimation controlling for the change of a covariate (Unbiased)
*************************************************************************************************************
reg y D treat post x w

*************************************************************************************************************
*** Difference-in-differences estimation controlling for the level of pre-treatment covariate (Biased)
*************************************************************************************************************
reg y D treat post c.X#i.post c.W#i.post


