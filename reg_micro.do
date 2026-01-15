/*
STEP ONE
OPEN DATA
*/
use "C:/Users/User/OneDrive - Fundacao Getulio Vargas - FGV/Green innovation/output/final_base/weather_quadro_societario.dta", clear


* Criar uma variável round com números sequenciais baseados nos anos
egen round = group(year)
egen id = group(mun_code)




keep if year > 1999

gen year_state_trend = year * code_state
gen  prop_verde_lag1 = prop_verde[_n-1]






*** Curto prazo ***


*Temperatura
reg2hdfespatial prop_verde cont_shock_temp  year_state_trend urban log_renda alphabetization_rate , timevar(round) panelvar(id) lat(lat) lon(lon) distcutoff(250) lagcutoff(6) 

*Chuva
reg2hdfespatial prop_verde cont_shock_precip year_state_trend urban log_renda alphabetization_rate , timevar(round) panelvar(id) lat(lat) lon(lon) distcutoff(250) lagcutoff(6) 

*Temperatura+Chuva
reg2hdfespatial prop_verde cont_shock_temp cont_shock_precip year_state_trend urban log_renda alphabetization_rate , timevar(round) panelvar(id) lat(lat) lon(lon) distcutoff(250) lagcutoff(6) 

lincom  cont_shock_temp+ cont_shock_precip

*** Médio prazo ***







* Create lag 1 manually
gen cont_shock_precip_lag1 = cont_shock_precip[_n-1]
* Create lag 2 manually
gen cont_shock_precip_lag2 = cont_shock_precip[_n-2]

* Create lag 3 manually
gen cont_shock_precip_lag3 = cont_shock_precip[_n-3]

* Create lag 4 manually
gen cont_shock_precip_lag4 = cont_shock_precip[_n-4]

* Create lag 5 manually
gen cont_shock_precip_lag5 = cont_shock_precip[_n-5]


* Create lag 1 manually
gen cont_shock_temp_lag1 = cont_shock_temp[_n-1]
* Create lag 2 manually
gen cont_shock_temp_lag2 = cont_shock_temp[_n-2]

* Create lag 3 manually
gen cont_shock_temp_lag3 = cont_shock_temp[_n-3]

* Create lag 4 manually
gen cont_shock_temp_lag4 = cont_shock_temp[_n-4]

* Create lag 5 manually
gen cont_shock_temp_lag5 = cont_shock_temp[_n-5]



gen cont_shock_precip_int_lag1 = cont_shock_precip*cont_shock_precip_lag1
gen cont_shock_precip_int_lag2 = cont_shock_precip*cont_shock_precip_lag2
gen cont_shock_precip_int_lag3 = cont_shock_precip*cont_shock_precip_lag3
gen cont_shock_precip_int_lag4 = cont_shock_precip*cont_shock_precip_lag4
gen cont_shock_precip_int_lag5 = cont_shock_precip*cont_shock_precip_lag5



gen cont_shock_temp_int_lag1 = cont_shock_temp*cont_shock_temp_lag1
gen cont_shock_temp_int_lag2 = cont_shock_temp*cont_shock_temp_lag2
gen cont_shock_temp_int_lag3 = cont_shock_temp*cont_shock_temp_lag3
gen cont_shock_temp_int_lag4 = cont_shock_temp*cont_shock_temp_lag4
gen cont_shock_temp_int_lag5 = cont_shock_temp*cont_shock_temp_lag5

summ cont_shock_temp_lag1
local mean_lag1 = r(mean)

summ cont_shock_temp_lag2
local mean_lag2 = r(mean)

summ cont_shock_temp_lag3
local mean_lag3 = r(mean)

summ cont_shock_temp_lag4
local mean_lag4 = r(mean)

summ cont_shock_temp_lag5
local mean_lag5 = r(mean)


*Temperatura
reg2hdfespatial prop_verde prop_verde_lag1 cont_shock_temp cont_shock_temp_lag1 cont_shock_temp_lag2 cont_shock_temp_lag3 cont_shock_temp_lag4 cont_shock_temp_lag5  cont_shock_temp_int_lag1  cont_shock_temp_int_lag2 cont_shock_temp_int_lag3 cont_shock_temp_int_lag4 cont_shock_temp_int_lag5  year_state_trend urban log_renda alphabetization_rate , timevar(round) panelvar(id) lat(lat) lon(lon) distcutoff(250) lagcutoff(6) 

lincom cont_shock_temp+ (`mean_lag1')*cont_shock_temp_int_lag1+ (`mean_lag2')*cont_shock_temp_int_lag2 + (`mean_lag3')*cont_shock_temp_int_lag3+ (`mean_lag4')*cont_shock_temp_int_lag4+(`mean_lag5')*cont_shock_temp_int_lag5 




*Chuva



summ cont_shock_precip_lag1
local mean_lag1 = r(mean)

summ cont_shock_precip_lag2
local mean_lag2 = r(mean)

summ cont_shock_precip_lag3
local mean_lag3 = r(mean)

summ cont_shock_precip_lag4
local mean_lag4 = r(mean)

summ cont_shock_precip_lag5
local mean_lag5 = r(mean)


reg2hdfespatial prop_verde prop_verde_lag1 cont_shock_precip cont_shock_precip_lag1 cont_shock_precip_lag2 cont_shock_precip_lag3 cont_shock_precip_lag4 cont_shock_precip_lag5  cont_shock_precip_int_lag1  cont_shock_precip_int_lag2 cont_shock_precip_int_lag3 cont_shock_precip_int_lag4 cont_shock_precip_int_lag5  year_state_trend urban log_renda alphabetization_rate , timevar(round) panelvar(id) lat(lat) lon(lon) distcutoff(250) lagcutoff(6) 

lincom cont_shock_precip+(`mean_lag1')*cont_shock_precip_int_lag1+ (`mean_lag2')*cont_shock_precip_int_lag2 +(`mean_lag3')*cont_shock_precip_int_lag3+ (`mean_lag4')*cont_shock_precip_int_lag4+ (`mean_lag5')*cont_shock_precip_int_lag5 

*Temperatura+Chuva

summ cont_shock_temp_lag1
local mean_lag1 = r(mean)

summ cont_shock_temp_lag2
local mean_lag2 = r(mean)

summ cont_shock_temp_lag3
local mean_lag3 = r(mean)

summ cont_shock_temp_lag4
local mean_lag4 = r(mean)

summ cont_shock_temp_lag5
local mean_lag5 = r(mean)


summ cont_shock_precip_lag1
local mean_lag6 = r(mean)

summ cont_shock_precip_lag2
local mean_lag7 = r(mean)

summ cont_shock_precip_lag3
local mean_lag8 = r(mean)

summ cont_shock_precip_lag4
local mean_lag9 = r(mean)

summ cont_shock_precip_lag5
local mean_lag10 = r(mean)


reg2hdfespatial prop_verde prop_verde_lag1 cont_shock_temp cont_shock_temp_lag1 cont_shock_temp_lag2 cont_shock_temp_lag3 cont_shock_temp_lag4 cont_shock_temp_lag5  cont_shock_temp_int_lag1  cont_shock_temp_int_lag2 cont_shock_temp_int_lag3 cont_shock_temp_int_lag4 cont_shock_temp_int_lag5 cont_shock_precip cont_shock_precip_lag1 cont_shock_precip_lag2 cont_shock_precip_lag3 cont_shock_precip_lag4 cont_shock_precip_lag5  cont_shock_precip_int_lag1  cont_shock_precip_int_lag2 cont_shock_precip_int_lag3 cont_shock_precip_int_lag4 cont_shock_precip_int_lag5 year_state_trend urban log_renda alphabetization_rate , timevar(round) panelvar(id) lat(lat) lon(lon) distcutoff(250) lagcutoff(6) 

lincom cont_shock_temp+ (`mean_lag1')*cont_shock_temp_int_lag1+ (`mean_lag2')*cont_shock_temp_int_lag2 + (`mean_lag3')*cont_shock_temp_int_lag3+ (`mean_lag4')*cont_shock_temp_int_lag4+(`mean_lag5')*cont_shock_temp_int_lag5 +cont_shock_precip+(`mean_lag6')*cont_shock_precip_int_lag1+ (`mean_lag7')*cont_shock_precip_int_lag2 +(`mean_lag8')*cont_shock_precip_int_lag3+ (`mean_lag9')*cont_shock_precip_int_lag4+ (`mean_lag10')*cont_shock_precip_int_lag5 









