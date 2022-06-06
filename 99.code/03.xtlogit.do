//______________________________________________________________________________
// Country-year level default estimation
// Data analysis yearly data set
// 06-06-2022 
//______________________________________________________________________________

// 0.0 set up ------------------------------------------------------------------

set more off
clear all

cd  "L:\99.BID app\01.tables\02.out tables\"
use "dt_yearly.dta"

// 0.1 preliminaries on variables ----------------------------------------------

** (1) indexes

* country encoding for xtset indexing
encode country, gen(n_country)

** (2) fiscal rules

* any kind of fiscal rule
gen fr = 0
replace fr = 1 if (er == 1 | rr == 1 | bbr == 1 | dr == 1)
label var fr "Any fiscal rule"

* lags for all fiscal rules
sort country year 
by   country : gen fr2 = fr[_n-2] // 2 years lag
label var fr2 "Any fiscal rule lagged 2 years"

sort country year 
by   country : gen fr5 = fr[_n-5] // 5 years lag
label var fr5 "Any fiscal rule lagged 5 years"

* lags for expenditure rules
sort country year 
by   country : gen er2 = er[_n-2] // 2 years lag
label var er2 "Expenditure rule lagged 2 years"

sort country year 
by   country : gen er5 = er[_n-5] // 5 years lag
label var er5 "Expenditure rule lagged 5 years"

* lags for debt rules
sort country year 
by   country : gen dr2 = dr[_n-2] // 2 years lag
label var dr2 "Debt rule lagged 2 years"

sort country year 
by   country : gen dr5 = dr[_n-5] // 5 years lag
label var dr5 "Debt rule lagged 5 years"

** (3) General Goverment expenditure
sort country year 
by   country : gen GGTEXP_gdp2 = GGTEXP_gdp[_n-2] // 2 years lag
label var GGTEXP_gdp2 "General government total expenditure percent of gdp lagged 2 years"

sort country year 
by   country : gen GGTEXP_gdp5 = GGTEXP_gdp[_n-5] // 5 years lag
label var GGTEXP_gdp5 "General government total expenditure percent of gdp lagged 5 years"

** (4) General Goverment revenue
sort country year 
by   country : gen GGR_gdpg2 = GGR_gdp[_n-2] // 2 years lag
label var GGR_gdpg2 "General government revenue percent of gdp lagged 2 years"

sort country year 
by   country : gen GGR_gdp5 = GGR_gdp[_n-5] // 5 years lag
label var GGR_gdp5 "General government revenue percent of gdp lagged 5 years"

** (5) General Goverment Gorss debt
sort country year 
by   country : gen GGGD_gdp2 = GGGD_gdp[_n-2] // 2 years lag
label var GGGD_gdp2 "General government gross debt percent of gdp lagged 2 years"

sort country year 
by   country : gen GGGD_gdp5 = GGGD_gdp[_n-5] // 5 years lag
label var GGGD_gdp5 "General government gross debt percent of gdp lagged 5 years"

** (6) Rule of law: voice and accountability estimate
sort country year 
by   country : gen VA_estimate2 = VA_estimate[_n-2] // 2 years lag
label var VA_estimate2 "Voice and accountability estimate lagged 2 years"

sort country year 
by   country : gen VA_estimate5 = VA_estimate[_n-5] // 5 years lag
label var VA_estimate5 "Coice and accountability estimate lagged 5 years"

** (7) Rule of law: control of corruption estimate
sort country year 
by   country : gen CCRR_estimate2 = CCRR_estimate[_n-2] // 2 years lag
label var CCRR_estimate2 "Control of corruption  estimate lagged 2 years"

sort country year 
by   country : gen CCRR_estimate5 = CCRR_estimate[_n-5] // 5 years lag
label var CCRR_estimate5 "Control of corruption  estimate lagged 5 years"

// 0.1 yearly conditional logit LAC --------------------------------------------

xtset year n_country 
list country year  d_cy, sepby(year) noobs

** (0.1.1)  reg on gross debt
xtlogit d_cy GGGD_gdp if LAC  == 1, fe or i(n_country) nolog
estimates store reg011

** (0.1.2)  reg on further general gov features
xtlogit d_cy fr2 GGGD_gdp GGTEXP_gdp2 GGR_gdp  GDP_US CAB_gdp Gnsavings_gdp investment ne_gdi_totl_zs if LAC  == 1, fe or i(n_country) nolog
estimates store reg012

** (0.1.3)  reg on social contry level features
xtlogit d_cy GGGD_gdp unemploy GDPpercap_US if LAC  == 1, fe or i(n_country) nolog
estimates store reg013

** (0.1.4)  reg on public gobernance features

xtlogit d_cy GGGD_gdp fr VA_estimate ROL_estimate PS_estimate GE_estimate CCRR_estimate RQ_estimate if LAC  == 1, fe or i(n_country) nolog

estimates store reg014

** (0.1.5) general reg
xtlogit d_cy fr2 GGGD_gdp GGTEXP_gdp2 GGR_gdp GDP_US investment unemploy VA_estimate ROL_estimate PS_estimate GE_estimate CCRR_estimate RQ_estimate if LAC  == 1, fe or i(n_country) nolog
estimates store reg015
tab n_country if e(sample)

esttab reg011  reg012 reg013  reg014 reg015 using "0.1.general_countryyear_LAC.rtf", eform b(%5.3f) se(%5.3f) replace

// 0.1 yearly conditional logit LAC --------------------------------------------

** (0.1.5) general reg
xtlogit d_cy i.fr2 GGGD_gdp GGTEXP_gdp2 GGR_gdp GDP_US investment unemploy VA_estimate ROL_estimate PS_estimate GE_estimate CCRR_estimate RQ_estimate i.LAC , re or i(n_country) nolog
tab n_country if e(sample)

margins i.LAC,  at(GGGD_gdp=(0(50)600))
marginsplot, recast(line) recastci(rarea)

margins i.LAC,  at(ROL_estimate=(-2.6(0.5)2.6))
marginsplot, recast(line) recastci(rarea)

//______________________________________________________________________________
// Country-year-quarter level default estimation
// Data analysis yearly data set
// 06-06-2022 
//______________________________________________________________________________

// 1.0 set up for quaterly -----------------------------------------------------

set more off
clear all

cd  "L:\99.BID app\01.tables\02.out tables\"
use "dt_quarterly.dta"

// 1.1 preliminaries on variables ----------------------------------------------

** (1) indexes

* country encoding for xtset indexing
encode country, gen(n_country)
encode year_country, gen(n_year_country)

gen qdate = yq(year, quarter)
 format qdate %tq

** (2) fiscal rules

* any kind of fiscal rule
gen fr = 0
replace fr = 1 if (er == 1 | rr == 1 | bbr == 1 | dr == 1)
label var fr "Any fiscal rule"

** (3) monetary values

gen log_PSD_total_us = log(PSD_total_us)
label var log_PSD_total_us "Ln of Gross PSD, Total, All maturities, All instruments, Nominal Value, US$"

// 1.2 quaterly conditional logit LAC --------------------------------------------

xtset n_country qdate
list year_country qdate  d_cy, sepby(year) noobs

** (1.2.1)  reg on gross debt

xtlogit d_cy log_PSD_total_us  if LAC  == 1, fe or nolog i(n_country)
estimates store reg121
tab n_country if e(sample)

xtlogit d_cy VA_estimate PS_estimate GE_estimate RQ_estimate ROL_estimate CCRR_estimate , fe or nolog i(n_country)
estimates store reg122


xtlogit d_cy log_PSD_total_us  VA_estimate PS_estimate GE_estimate RQ_estimate ROL_estimate CCRR_estimate , fe or nolog i(n_country)
estimates store reg123
