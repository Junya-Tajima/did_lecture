# did_lecture_code

This repository contains the stata code for my lectures, entitled "Recent Discussions in Difference-in-Differences". It is designed for students who want to understand recent discussions on difference-in-differences (DID) and implement recently developed methods using stata. 

## Data

Some files require a separate dta file to be downloaded. Each dta file can be downloaded at the links below:

https://github.com/Junya-Tajima/did_lecture_data

You need these packages to run do-files:

- `reghdfe`
- `ftools`
- `ppmlhdfe`
- `event_plot`
- `addplot`
- `drdid`
- `csdid`
- `bacondecomp`
- `egenmore`
- `sdid`
- `sdid_event`
- `honestdid`

You can install these packages with the following command:

```bash
ssc install reghdfe
ssc install ftools
ssc install ppmlhdfe
ssc install event_plot
ssc install addplot
ssc install drdid
ssc install csdid
ssc install bacondecomp
ssc install egenmore
ssc install sdid
ssc install sdid_event
ssc install honestdid
