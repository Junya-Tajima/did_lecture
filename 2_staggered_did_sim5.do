/* Install the required packages if they are not already installed
   ssc install reghdfe
   ssc install ftools
   */

*************************************************************************************
*Wooldridge(2021)の手法でStaggered な状況を分析してみよう
*パネルデータを作成する
*idは1から3000
*サンプル期間は30年
*グループは3つあり、1000ごとにidで区分される
*グループ1は8年目、グループ2は16年目、グループ３は24年目に処置を受ける
*処置効果はグループごとに異なる
*処置効果は時間がたつにつれ増加する
*ssc install reghdfe
*ssc install ftools

* Simulation: Analyze a staggered situation using the method from Wooldridge (2021)
* Create panel data
* id ranges from 1 to 3000
* The sample period is 30 years
* There are 3 groups, divided by id in increments of 1000
* Group 1 receives treatment in year 8, Group 2 in year 16, and Group 3 in year 24
* The treatment effect varies by group
* The treatment effect increases over time
* ssc install reghdfe
* ssc install ftools

*************************************************************************************

clear
set obs 90000

gen id = _n
gen N = _n
gen year = 1

forvalues k = 3000(3000)87000{
	replace id = id - 3000 if N > `k'
}

forvalues t = 3000(3000)87000{
	replace year = year + 1 if N > `t'
}

gen g1 = (id <= 1000)
gen g2 = (id > 1000 & id <= 2000)
gen g3 = (id > 2000 & id <= 3000)

gen group = 0
replace group = 1 if g1 == 1
replace group = 2 if g2 == 1
replace group = 3 if g3 == 1

gen D = 0
replace D = 1 if year >= 8 & g1 == 1
replace D = 1 if year >= 16 & g2 == 1
replace D = 1 if year >= 24 & g3 == 1

gen Y = rnormal(3, 0.5^2)
replace Y = Y + rnormal(10*(year - 7), 2^2)*D if g1 == 1
replace Y = Y + rnormal(5*(year - 15), 2^2)*D if g2 == 1
replace Y = Y + rnormal(1*(year - 23), 2^2)*D if g3 == 1

gen S1 = 0
gen S2 = 0
gen S3 = 0

replace S1 = year - 7 if year >= 8
replace S2 = year - 15 if year >= 16
replace S3 = year - 23 if year >= 24

*アウトカムの各グループごとのトレンドを確認する
bysort group year: egen Y_mean = mean(Y)
twoway(line Y_mean year if group == 1)(line Y_mean year if group == 2)(line Y_mean year if group == 3), legend(order(1 "Group1" 2 "Group2" 3 "Group3"))

*通常のＴＷＦＥＤＤ
reghdfe Y D, abs(id year)

*Wooldridge(2021)
reghdfe Y i.g1##i.S1 i.g2##i.S2 i.g3##i.S3, abs(id year)

*いっそのこと適切な比較が出来ない期間は落としてしまおうか
drop if year >= 24

reghdfe Y i.g1##i.S1 i.g2##i.S2 i.g3##i.S3, abs(id year)



