/*Installing packages
ssc install egenmore
ssc install ftools
ssc install reghdfe
ssc install drdid
*/

*******************************************************************************************************
** Simulation: Difference-in-differences with time-invariant covariates
** N = 2000000 (1000000 students and two periods)
** 2×2 design (Pre and Post, Treat and Control)
** Y is the test score of each student
** Each Student's probability of getting into Aclass depends on pre-treatment GPA and grade
** Each Student's test score growth depends on pre-treatment GPA and grade
** Students in Aclass receive the treatment in the post period
** True treatment effect is 5
*******************************************************************************************************

*******************************************************************************************************
** Data Creation
*******************************************************************************************************

* Clear the data
clear

* Set the number of observations to 2000000
set obs 2000000

* Create the panel data
* Generate panel data for 1000000 individuals over 2 periods
gen num = _n
gen id = _n
gen year = 1

replace id = id - 1000000 if num > 1000000
replace year = 2 if num > 1000000

* Create a variable representing each student's GPA
gen GPA = runiform(0,4)
by id, sort: egen gpa = first(GPA)

* Create a variable representing each student's grade
gen GRADE = int(runiform(0,4)) + 1
by id, sort: egen grade = first(GRADE)

* Create the squared term of the GPAs
gen gpa2 = gpa^2

* Create the squared term of the grades
gen grade2 = grade^2

* Create a dummy variable for the treatment group
* Each Student's probability of getting into Aclass depends on pre-treatment GPA and grade
gen TREAT = rbinomial(1, logistic(0.02*gpa2 + 0.02*grade2 + 0.01*gpa2*grade2 + 0.001))
by id, sort: egen treat = first(TREAT)

* Generate the indicator variable which equals to one for post-treatment period
gen post = 0
replace post = 1 if year == 2

* Generate the treatment dummy variable
gen D = treat*post

* Generate the outcome variable 
* Each Student's test score growth depends on pre-treatment GPA and grade
gen Y = rnormal(20 + 5*treat, 0.5^2) + 2*post + 3*post*gpa + 3*post*grade + post*gpa*grade + rnormal()
replace Y = Y + rnormal(5, 0.5^2) if post == 1 & treat == 1

* Plotting the difference in distribution of covariates between the two classes
* GPA Distribution
twoway(hist gpa if treat == 1 & post == 0, color(blue%30)) (hist gpa if treat == 0 & post == 0, color(red%30)) ///
      , legend(order(1 "A Class" 2 "B Class")) xtitle("") title("GPA Distribution")

* Grade Distribution
twoway(hist grade if treat == 1 & post == 0, color(stc1)), title("A Class") name(m1, replace) xtitle("")
twoway(hist grade if treat == 0 & post == 0, color(stc2)), title("B Class")name(m2, replace) xtitle("")
graph combine m1 m2, scale(0.8) title("Grade Distribution")

* Plotting the difference in trend of test scores between the two classes
bysort treat post : egen Score = mean(Y)
twoway(line Score post if treat == 1, color(stc1))(line Score post if treat == 0, color(stc2))(scatter Score post if treat == 1, color(stc1) msize(large)) ///
      (scatter Score post if treat == 0, color(stc2) msize(large) msymbol(triangle)), xline(0.5) legend(order (3 "A Class" 4 "B Class")) xtitle("") ///
	  xlabel(, nogrid) ylabel(, nogrid)
	  
*******************************************************************************************************
** DID regression without covariates
*******************************************************************************************************
reghdfe Y D, abs(id post)

*******************************************************************************************************
** DID regression with covariates (correct specification)
*******************************************************************************************************
reghdfe Y D i.post#c.gpa i.post# c.grade i.post#c.gpa#c.grade, abs(id post)

*******************************************************************************************************
** DID regression with covariates (incorrect specification)
*******************************************************************************************************
reghdfe Y D i.post#c.gpa2 i.post#c.grade2 i.post#c.gpa2#c.grade2, abs(id post)

*******************************************************************************************************
** Outcome Regression DID  (correct specification)
*******************************************************************************************************
drdid Y c.gpa c.grade c.gpa#c.grade, ivar(id) time(post) treatment(treat) reg

*******************************************************************************************************
** Outcome Regression DID  (incorrect specification)
*******************************************************************************************************
drdid Y c.gpa2 c.grade2 c.gpa2#c.grade2, ivar(id) time(post) treatment(treat) reg

*******************************************************************************************************
** Inverse Probability Weighting DID  (correct specification)
*******************************************************************************************************
drdid Y c.gpa2 c.grade2 c.gpa2#c.grade2, ivar(id) time(post) treatment(treat) ipw

*******************************************************************************************************
** Inverse Probability Weighting DID  (incorrect specification)
*******************************************************************************************************
drdid Y c.gpa c.grade c.gpa#c.grade, ivar(id) time(post) treatment(treat) ipw

*******************************************************************************************************
** Doubly Robust  DID  (OR is correctly specified but PS is not correctly specified)
*******************************************************************************************************
drdid Y c.gpa c.grade c.gpa#c.grade, ivar(id) time(post) treatment(treat) dripw

*******************************************************************************************************
** Doubly Robust  DID  (OR is not correctly specified but PS is correctly specified)
*******************************************************************************************************
drdid Y c.gpa2 c.grade2 c.gpa2#c.grade2, ivar(id) time(post) treatment(treat) dripw





