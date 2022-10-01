**********************************************************************
**********************************************************************
/*
Cleaning data for Groh & McKenzie Application
*/
**********************************************************************
**********************************************************************

* cd _________ // change working directory;

use "Data/MacroinsuranceforMicroentrepreneurs.dta", clear 

**** Variables needed:
* profits truncated at 99th percentile following pre-analysis plan
drop x10 x11
gen x10=b_605_1 
replace x10=7000 if x10>7000 & x10~=.
gen x11=b_605_2 
replace x11=8000 if x11>8000 & x11~=.
gen numworkers=b_210A_1
gen anypaidworker=numworkers>0 & numworkers~=.
gen manufacturing=b_202>=10 & b_202<=32
gen retail=b_202==47
gen assets=0
forval x=1/8 {
replace assets=assets+b_508_`x'
}

gen inventory=b_509A
gen cashonhand=b_511
gen valueofgoodsatstartofday=b_510
gen totalassets=assets+inventory+cashonhand
gen completedhighschool=1 if ((b_106>3 & b_106~=.) | (b_106==2 & b_107>2))
replace completedhighschool=0 if completedh~=1
gen ageofowner=2012-b_103YEAR if b_103YEAR<=1993
gen ageofbusiness=2012-b_202Y
replace ageofbusiness=. if ageofbusiness<0

/// quick tabulations
* take-up among treated
tab epp if treat==1
* take-up among treated who renewed loan
tab epp if treat==1 & admin_loanrenewal==1

/// replacing variable
replace x1=x1/100

/// generate discounts of future by 30% or more
gen discounter=b_406VALU<=350
gen numeracy=( b_1001_1==50)& b_1001_2==83& b_1001_3==64& b_1002_1==93& b_1002_2==86& b_1002_3==79
pca b_1205_8 b_1205_11 b_1205_12 b_1205_15  b_1205_18 b_1205_20
predict wealth

gen noaccesstootherfinance=b_701==2
gen creditconstrained=b_901==2
gen insurance_understand=b_1006==1 & b_1007==2 & b_1008==1
gen riskaverse_ambiguityneutral=x2*x3
rename riskaverse ra_an

est drop _all

/// identify which pairs have a dropped endline survey response
bysort pair: gen times_present = _N

* attrition indicator
gen attrition = 1 if times_present == 2
replace attrition = 0 if missing(attrition)

* addl 
gen tookotherloan=m_401_2==1|m_401_4==1|m_401_3==1
gen makeinvest=m_601_2==1
gen amountinvest=m_601_2VA
gen newproduct=m_216==1
gen secondbus=m_202A>=2 & m_202A<=4 & m_208==1
gen m_inventories=m_702 if m_702<999998
gen b_inventories=b_509A
gen hireworker=m_222_1==1|m_222_2==1

* new variable
for num 1/3: replace m_903_X=. if m_903_X==99997|m_903_X==99998
gen m_profits=(m_903_1+m_903_2+m_903_3)/3
* closed businesses have zero profit
replace m_profits=0 if m_102==2|(m_207==1|m_207==2|m_207==4)

gen b_profits=(b_605_1+b_605_2+b_605_3)/3
gen miss_baseprofits=b_profits==.
replace b_profits=0 if b_profits==.

sum m_profits, d
gen m_prof_cap=m_profits
replace m_prof_cap=r(p99) if m_profits>r(p99) & m_profits~=.

* Column 2: Profits above the 95th percentile of control group
sum m_profits, d
gen highprofits=(m_profits>r(p95)) & m_profits~=.
replace highprofits=. if m_profits==.
sum b_profits, d
gen b_highprofits=(b_profits>r(p95)) & b_profits~=.

* Column 3: Revenue
for num 1/3: replace m_901_X=. if m_901_X==99997|m_901_X==99998
gen m_revenue=(m_901_1+m_901_2+m_901_3)/3
* closed businesses have zero revenue
replace m_revenue=0 if m_102==2|(m_207==1|m_207==2|m_207==4)

gen b_revenue=(b_604_1+b_604_2+b_604_3)/3
gen miss_baserevenue=b_revenue==.
replace b_revenue=0 if b_revenue==.

sum m_revenue, d
gen m_rev_cap=m_revenue
replace m_rev_cap=r(p99) if m_revenue>r(p99) & m_revenue~=.

* Column 4: Revenue above 95th percentile of control group distribution
sum m_revenue, d
gen highrevenue=(m_revenue>r(p95)) & m_revenue~=.
replace highrevenue=. if m_revenue==.
sum b_revenue, d
gen b_highrevenue=(b_revenue>r(p95)) & b_revenue~=.

* Column 5: Number of employees
gen m_paidemployees=m_223_1+m_223_2
sum m_paidemployees, d
replace m_paidemployees=r(p99) if m_paidemployees>r(p99) & m_paidemployees~=.
replace m_paidemployees=0 if m_102==2|(m_207==1|m_207==2|m_207==4)

gen base_paidemp=b_210A_1+b_210B_1

** Column 6: Any worker
gen anyworker=m_paidemp>0 
replace anyworker=. if m_paidemp==.
gen baseanyworker=base_paidemp>0
replace baseanyworker=. if base_paidemp==.

* Column 7: Hours Worked 
gen hours=m_213_1
replace hours=. if hours==999
replace hours=84 if hours>84 & hours<169

* hours are zero if not in business
replace hours=0 if m_102==2|(m_207==1|m_207==2|m_207==4)

gen b_hours=b_212A
replace b_hours=. if b_hours==999
replace b_hours=84 if b_hours>84 ^ b_hours~=.
gen b_hours_miss=b_hours==.
replace b_hours=0 if b_hours_miss==1

* Column 8: Consumption
gen consumption_food=b_1201_1+b_1201_2+ b_1201_3+ b_1201_4+b_1201_5+b_1201_6+b_1201_7+b_1201_8+b_1201_9 
gen consumption_monthlyexpenses=b_1202_1+b_1202_2+b_1202_3 +b_1202_4 +b_1202_5 +b_1202_6 +b_1202_7 +b_1202_8 +b_1202_9 +b_1202_10 +b_1202_11 +b_1202_12
gen consumption_yearlyexpenses= b_1203_1 +b_1203_2 +b_1203_3 +b_1203_4 +b_1203_5 +b_1203_6 +b_1203_7 +b_1203_8 +b_1203_9 +b_1203_10

foreach var of varlist consumption_food consumption_monthlyexpenses consumption_yearlyexpenses {
sum `var', d
replace `var'=r(p99) if `var'>r(p99) & `var'~=.
}
gen totalconsumption_monthly=consumption_food*4 + consumption_m + consumption_y/12

gen m_consumption_avg=52*m_1301_10+12*m_1302_12+2*m_1303_11 if m_1301_10<3000 & m_1302_12<9500 & m_1303_11<250000
egen m_consumption_avg95= pctile(m_consumption_avg), p(95)
cap drop  m_consumption_avg_cap
gen m_consumption_avg_cap= m_consumption_avg if m_consumption_avg<=m_consumption_avg95
replace m_consumption_avg_cap= m_consumption_avg95 if m_consumption_avg95 < m_consumption_avg
replace m_consumption_avg_cap=m_consumption_avg_cap/12

foreach var of varlist m_prof_cap highprofits m_rev_cap highrevenue m_paidemp anyworker hours m_consumption_avg_cap {
	sum `var' if treat==0
	local thismean = r(mean) 
	local thissd = r(sd) 
	di "`thismean'" 
	di "`thissd'" 
	cap drop z1_`var'
	gen z1_`var' = (`var'-`thismean')/(`thissd') 
} 

egen Table7index = rowmean(z1_m_prof_cap z1_highprofits z1_m_rev_cap z1_highrevenue z1_m_paidemp z1_anyworker z1_hours z1_m_consumption_avg_cap) 

label var Table7index "Aggregate Index of  outcomes in Table 7"


/// keep the relevant variables
keep attrition epp admin_loanrenewal epp_loanamount makeinvest amountinvest newproduct secondbus m_inventories b_inventories hireworker tookotherloan pair npairs treat x1 x4 x5 discounter x2 x8 x9 admin_numberloans female completedhighschool numeracy wealth noaccesstootherfinance creditconstrained insurance_understand m_revenue m_prof_cap highprofits m_rev_cap highrevenue m_paidemp anyworker hours m_consumption_avg_cap Table7index x3 ra_an b_profits miss_baseprofits b_revenue miss_baserevenue b_highrevenue miss_baserevenue base_paidemp baseanyworker Table7index m_consumption_avg_cap totalconsumption_monthly b_hours b_hours_miss b_highprofits miss_baseprofits

save "MacroInsurance_Cleaned.dta", replace
