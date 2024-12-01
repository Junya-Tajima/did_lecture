/* Install the required packages if they are not already installed
   ssc install reghdfe 
   ssc install ftools
   ssc install bacondecomp
   ssc install event_plot
   ssc install drdid
   ssc install csdid
   */

*************************************************************************************************************************************************
** Data Analysis : Technology adoption and innovation: The establishment of airmail and aviation innovation in the United States, 1918–1935
** Data File : airmail.dta
** Data Source : https://github.com/ehsohn/airmail_data
*************************************************************************************************************************************************
* Clean environment
clear all
macro drop _all
set more off

* Set the working directly
cd ""

* Download the data
use "airmail.dta", clear

* Summarize the dataset
summarize

* Display the unique fips codes in the dataset
tab fips

* Display the years in the dataset
tab year

* Summarize tha data
summarize all_patno

* Create the histogram of the outcome variable 
twoway (hist all_patno, color(stc1)), xlabel(, nogrid) ylabel(, nogrid)

* Visualize each treatment timing
by year, sort: egen POST_mean = mean(postentry)
twoway (line POST_mean year, color(stc1)) (scatter POST_mean year, color(stc1)) ///
       , xlabel(, nogrid) ylabel(, nogrid) ytitle("ratio of treated") legend(order(3 ""))
	  
*************************************************************************************************************************************************
*** Two-way fixed effects estimator
*************************************************************************************************************************************************
*** Conduct static DID estimation using two-way fixed effects estimator
* Conduct event study DID estimation
reghdfe all_patno postentry, abs(fips year) vce(cl fips) nocons

* Confirm the mean of the outcome variable to estimate effect size
sum all_patno 

* Create the variable which specifies the treatment timing for each group
egen timing = csgvar(postentry), ivar(fips) tvar(year)

* Confirm each treatment timing
tab timing

* Create the variable indicating how many years has it been since each group received the treatment
gen relative = year - timing

* Confirm each relative year 
tab relative

*** Conduct event study analysis using two-way fixed effects estimator
* Create a lag variable indicating how many years each group has been treated
forvalues l = 0/17 {
	gen lag`l' = (relative == `l')
}

* Create a lead variable indicating how many years before each group receives the treatment
forvalues l = 1/15 {
	gen lead`l' = (relative == -`l')
}

* Normalise lead1 indicating one period before the treatment to 0
replace lead1 = 0 

* Conduct event study DID estimation
reghdfe all_patno lead* lag* , abs(fips year) cluster(fips) nocons

* Plot the resuls of event study anlysis
* Display results for the entire period
event_plot, default_look stub_lag(lag#) stub_lead(lead#) together graph_opt(xtitle("Event time") ///
            ytitle("Estimates") title("TWFE Event Study") xlabel(, nogrid) ylabel(, nogrid)) ///
			trimlead(15) trimlag(17)
						
* Plot the resuls of event study anlysis
* Limit the range of the graph to the 5 years before and after the treatment
event_plot, default_look stub_lag(lag#) stub_lead(lead#) together graph_opt(xtitle("Event time") ///
            ytitle("Estimates") title("TWFE Event Study") xlabel(-5(1)5, nogrid) ylabel(, nogrid) name(g1, replace)) ///
			trimlead(5) trimlag(5)
			
*** Examine if there are significant dynamic effects when the pre-treatment mean is used as the reference point
* Normalise all lead variables to 0
forvalues k = 1(1)15{
	replace lead`k' = 0 
}

* Conduct event study DID estimation
reghdfe all_patno lead* lag* , abs(fips year) cluster(fips) nocons

* Plot the resuls of event study analysis
* Display results for the entire period
event_plot, default_look stub_lag(lag#) stub_lead(lead#) together graph_opt(xtitle("Event time") ///
            ytitle("Estimates") title("TWFE Event Study") xlabel(, nogrid) ylabel(, nogrid)) 
			

*************************************************************************************************************************************************
*** Diagnosis for heterogeneous treatment effects
*************************************************************************************************************************************************	
*** Bacon decomposition (2021)
* Set the panel data structure using fips as the group variable and year as the time variable
xtset fips year

* Conduct Bacon-decomposition
bacondecomp all_patno postentry, ddetail 

*** Jakiela's diagnosis (2021)
* Residualize the treatment dummy by controlling for fixed effects
qui: reg postentry i.year i.fips
predict POST_resid, resid

* Residualize the outcome variable by controlling for fixed effects
qui: reg all_patno i.year i.fips
predict all_patno_resid, resid

* Plot the relationship between the residualized treatment dummy and the residualized outcome variable
twoway (scatter all_patno_resid POST_resid if postentry == 0, color(stc2%40))(scatter all_patno_resid POST_resid if postentry== 1,  color(stc1%40)) ///
       (lfit all_patno_resid POST_resid if postentry== 0, color(stc2)) (lfit all_patno_resid POST_resid if postentry == 1, color(stc1)), ///
	   xtitle("postentry residual") ytitle("all_patno residual") title("Jakiela's diagnosis (2021)") ///
	   legend(order(1 "Treated" 2 "Control")) xlabel(, nogrid) ylabel(, nogrid)
	   
*************************************************************************************************************************************************
*** Callaway and Sant'Anna Estimator (2021)
*************************************************************************************************************************************************  
*** Revisit the main result using CS DID estimator
* Conduct CS DID estimation 
* Request using observations never treated and those not yet treated as control group
csdid all_patno postentry, ivar(fips) time(year) gvar(timing) notyet 

* Calculate the simple weighted average of treatment effects
estat simple

* Calculate the weighted average of treatment effects for each group
estat group

* Calculate the weighted average of treatment effects for each calendar time
estat calendar	   

* Calculate the weighted average of treatment effects for each event time
estat event, estore(cs)

* Plot the resuls of event study analysis
* Display results for the entire period
event_plot cs, default_look stub_lag(Tp#) stub_lead(Tm#) together graph_opt(xtitle("Event time") ///
ytitle("Estimates") title("Callaway and Sant'Anna (2021)") xlabel(, nogrid) ylabel(, nogrid)) 
			
* Limit the range of the graph to the five years before and after the treatment
event_plot cs, default_look stub_lag(Tp#) stub_lead(Tm#) together graph_opt(xtitle("Event time") ///
            ytitle("Estimates") title("Callaway and Sant'Anna (2021)") xlabel(-5(1)5, nogrid) ylabel(, nogrid) name(g2, replace)) ///
			trimlead(5) trimlag(5)
			
* Alternative design of event study plot
estat event 
csdid_plot

*** Combine all graphs
graph combine g1 g2, scale(0.8)

*** Robustness Check: Dropping notyet-treated
* Conduct CS DID estimation 
* Request using observations never treated as control group
csdid all_patno postentry, ivar(fips) time(year) gvar(timing) 

* Calculate the simple weighted average of treatment effects
estat simple

* Calculate the weighted average of treatment effects for each group
estat group

* Calculate the weighted average of treatment effects for each calendar time
estat calendar	   

* Calculate the weighted average of treatment effects for each event time
estat event, estore(cs)

* Plot the resuls of event study analysis
* Display results for the entire period
event_plot cs, default_look stub_lag(Tp#) stub_lead(Tm#) together graph_opt(xtitle("Event time") ///
ytitle("Estimates") title("Callaway and Sant'Anna (2021)") xlabel(, nogrid) ylabel(, nogrid)) 
			
* Limit the range of the graph to the five years before and after the treatment
event_plot cs, default_look stub_lag(Tp#) stub_lead(Tm#) together graph_opt(xtitle("Event time") ///
            ytitle("Estimates") title("Callaway and Sant'Anna (2021)") xlabel(-5(1)5, nogrid) ylabel(, nogrid) name(g2, replace)) ///
			trimlead(5) trimlag(5)
			
* Alternative design of event study plot
estat event 
csdid_plot

*** Combine all graphs
graph combine g1 g2, scale(0.8)























