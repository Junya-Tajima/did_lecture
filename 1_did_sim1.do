/* Install the required packages if they are not already installed
   ssc install reghdfe
   ssc install ftools
   */

*******************************************************************************************************
** Simulation : Comparing five different difference-in-differences estimation methods
** N = 20000
** There are two groups (treatment and control)in the sample
** There are two periods (post and pre) in the sample
** DGP: Y = 30 + 10*POST + 10*TREAT + 30*(TREAT*POST) + U; U ~ N(0,1)
** Compute DID estimates using explained five methods
** Compare each DID estimates
*******************************************************************************************************

***Create the data and variables
*Delete all data
clear

*Set the number of observations
set obs 20000

*Set the seed to ensure reproducibility
set seed 1210

*Assign numbers from 1 to 20000
gen n = _n

*Create individual IDs
gen id = _n
replace id = id - 10000 if n > 10000

*Generate the indicator variable which equals to one for post-treatment period
gen post = (n > 10000)

*Generate the indicator variable which equals to one for treatment group
gen treat = (id <= 5000)

*Generate the treatment dummy variable
generate D = treat*post

*Confirm DID's 4 blocks
tab treat post

*Generate the outcome variable 
gen y = rnormal(30, 30) + rnormal(10, 10)*post + rnormal(10, 10)*treat + rnormal(30, 30)*D + rnormal()

*Calculate the mean for each group/period
bysort treat post: egen y_mean = mean(y)

*Conduct the trends plot
twoway(line y_mean post if treat == 1, color(stc1))(scatter y_mean post if treat == 1, color(stc1) msize(large))(line y_mean post if treat == 0, color(stc2))(scatter y_mean post if treat == 0, color(stc2) msize(large)), xlabel(minmax, nogrid) ylabel(, nogrid) legend(order(2 "Treatment" 4 "Control"))

***Comparing each DID estimation 
**Create an empty vector to save five estimates
matrix DID_Estimate = J(5, 1, .)

*Using sample mean
summarize y if treat == 1 & post == 0
generate y10 = r(mean)

summarize y if treat == 1 & post == 1
generate y11 = r(mean)

summarize y if treat == 0 & post == 0
generate y00 = r(mean)

summarize y if treat == 0 & post == 1
generate y01 = r(mean)

generate DID = (y11 - y10) - (y01 - y00)
sum DID

matrix DID_Estimate[1,1] = r(mean)

*Standard regression
reg y D treat post, vce(cl id)

matrix DID_Estimate[2,1] = e(b)[1,1]

*Two-way fixed effects
reghdfe y D, abs(id post) vce(cl id)

matrix DID_Estimate[3,1] = e(b)[1,1]

*Cross sectional regression using the change of outcome variable
xtset id post
generate y_change = y - l.y

reg y_change treat if post == 1 , vce(cl id)

matrix DID_Estimate[4,1] = e(b)[1,1]

*Imputation method
quietly reg y treat post if D == 0, vce(cl id)
predict y_hat, xb

generate DID_imp = y - y_hat
sum DID_imp if treat == 1 & post == 1

matrix DID_Estimate[5,1] = r(mean)

*Compare each DID estimate
matrix list DID_Estimate
