/*
STEP ONE
OPEN DATA
*/
use "C:/Users/User/OneDrive - Fundacao Getulio Vargas - FGV/Green innovation/output/final_base/weather_rais.dta", clear

* Manter as variáveis necessárias
keep prop_verde ma_shock_temp_10yr ma_shock_precip_10yr cont_shock_temp cont_shock_precip lag5_cont_shock_temp lag5_cont_shock_precip lag10_cont_shock_temp lag10_cont_shock_precip mun_code lat lon cod_rgi year code_state	

* Criar uma variável round com números sequenciais baseados nos anos
egen round = group(year)
egen id = group(mun_code)
egen round = group(year )
tabulate year, generate(year_dummy)

// Criar identificador combinado
egen year_state = group(year code_state)

// Criar dummies a partir do identificador combinado
tabulate year_state, generate(year_state_dummy)	

keep if year > 1994

/* 
STEP TWO
GET DATA READY FOR TABLE
*/				

* Xtset data
xtset id year

* Define locals para as combinações de variáveis independentes
local dvar "prop_verde"
local ivars1 "ma_shock_temp_10yr ma_shock_precip_10yr"
local ivars2 "cont_shock_temp cont_shock_precip"
local ivars3 "lag5_cont_shock_temp lag5_cont_shock_precip"
local ivars4 "lag10_cont_shock_temp lag10_cont_shock_precip"

local ivar_labels1 "T1 P1"
local ivar_labels2 "T2 P2"
local ivar_labels3 "T3 P3"
local ivar_labels4 "T4 P4"

/* 
STEP THREE
MAKE TABLE
*/

* Loop through combinations of independent variables
local combinations "1 2 3 4"
local combination_names "ivars1 ivars2 ivars3 ivars4"
local combination_labels "ivar_labels1 ivar_labels2 ivar_labels3 ivar_labels4"

foreach comb of local combinations {
    local ivars = word("`ivars1' `ivars2' `ivars3' `ivars4'", `comb')
    local ilabels = word("`ivar_labels1' `ivar_labels2' `ivar_labels3' `ivar_labels4'", `comb')

    * xtreg
    xtreg `dvar' `ivars' i.year year_state_dummy*, fe cluster(mun_code)
    local obs_`comb' = string(e(N), "%9.0fc")
    foreach var of local `ivars' {
        local b_xt_`comb'_`var' = string(_b[`var'], "%9.4fc")
        local se_xt_`comb'_`var' = string(_se[`var'], "%9.4fc")
        local t_xt_`comb'_`var' = _b[`var'] / _se[`var']
        local p1_xt_`comb'_`var' = cond(abs(`t_xt_`comb'_`var'') > invnormal(0.95), "*", "")
        local p2_xt_`comb'_`var' = cond(abs(`t_xt_`comb'_`var'') > invnormal(0.975), "*", "")
        local p3_xt_`comb'_`var' = cond(abs(`t_xt_`comb'_`var'') > invnormal(0.995), "*", "")
        local p_xt_`comb'_`var' = "`p1_xt_`comb'_`var''`p2_xt_`comb'_`var''`p3_xt_`comb'_`var''"
    }

    * conley
    reg2hdfespatial `dvar' `ivars' year_dummy*, timevar(round) panelvar(mun_code) lat(lat) lon(lon) distcutoff(250) lagcutoff(6)
    foreach var of local `ivars' {
        local b_co_`comb'_`var' = string(_b[`var'], "%9.4fc")
        local se_co_`comb'_`var' = string(_se[`var'], "%9.4fc")
        local t_co_`comb'_`var' = _b[`var'] / _se[`var']
        local p1_co_`comb'_`var' = cond(abs(`t_co_`comb'_`var'') > invnormal(0.95), "*", "")
        local p2_co_`comb'_`var' = cond(abs(`t_co_`comb'_`var'') > invnormal(0.975), "*", "")
        local p3_co_`comb'_`var' = cond(abs(`t_co_`comb'_`var'') > invnormal(0.995), "*", "")
        local p_co_`comb'_`var' = "`p1_co_`comb'_`var''`p2_co_`comb'_`var''`p3_co_`comb'_`var''"
    }
}

/* 
Generate LaTeX table 
*/

/* Define output location */  
cap file close reg_output
local file "D:/Users/leonl/Desktop/Projeto/temp/Direction of inovation/Climate shocks and innovation/rais_estimation.tex"

/* Table header */
file open reg_output using "`file'", write replace
foreach mc in 1 2 3 4 5 6 7 8 {				
    file write reg_output " &\multicolumn{1}{c}{(`mc')} " 
}
file write reg_output " \\ "  _n 			
file write reg_output "\hline" _n

/* Table core */
foreach comb of local combinations {
    local ilabels = word("`ivar_labels1' `ivar_labels2' `ivar_labels3' `ivar_labels4'", `comb')
    foreach var of local `ivars' {
        file write reg_output "`ilabels'"
        foreach c in 1 2 {
            file write reg_output " & `b_xt_`comb'_`var'' "
        }
        file write reg_output " \\ " _n
        foreach c in 1 2 {
            file write reg_output " & (`se_xt_`comb'_`var'')`p_xt_`comb'_`var'' "						
        }
        file write reg_output " \\ " _n
        foreach c in 1 2 {
            file write reg_output " & [`se_co_`comb'_`var'']`p_co_`comb'_`var'' "					
        }
        file write reg_output " \\ " _n       
        file write reg_output "[1em]" _n
    }
}

/* Table footer */
file write reg_output "\hline" _n 
file write reg_output "Region-year trends & Y & N  & Y & N & Y & N & Y & N \\" _n 
file write reg_output "Region-year FE & N & Y & N & Y & N & Y & N & Y \\" _n 
file write reg_output "Observations "
foreach comb of local combinations {
    foreach c in 1 2 {
        file write reg_output " & `obs_`comb'' "
    }
}			
file write reg_output " \\ " _n
file close reg_output
