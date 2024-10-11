* ssc install ppmlhdfe
* ssc install ftools
* ssc install reghdfe

clear

* Load the dataset
import delimited using "Final_DATA.csv", clear

* Clean and convert FLOW to numeric
gen flow_num = real(flow)
gen findist_j_num = real(findist_j)
gen area_j_num = real(area_j)

* Identify the number of missing values
*summarize flow_num, detail
*count if missing(flow_num)


* Remove rows with missing values in flow_num
*drop if missing(flow_num)

* Generate log-transformed variables
gen lnPOP_i = log(pop_i)
gen lnPOP_j = log(pop_j)
gen lnDIST_ij = log(dist)
gen lnFINDIST_j = log(findist_j_num)
gen lnAREA_j = log(area_j_num)

* Generate interaction terms
gen lnFINDIST_D = lnFINDIST_j * d
gen lnAREA_D = lnAREA_j * d
gen lnPOP_D = lnPOP_j * d
gen BOR_D = bor * d

* Convert country_i and country_j to numeric
encode country_i, gen(country_i_num)
encode country_j, gen(country_j_num)

* Generate a unique panel identifier for each pair of countries
egen panel_id = group(country_i_num country_j_num)

* Set panel data structure
xtset panel_id year, yearly

* Estimate bilateral openness using PPML
ppmlhdfe flow_num d lnPOP_i lnPOP_j lnDIST_ij bor ///
    lnFINDIST_D lnAREA_D lnPOP_D BOR_D ///
    i.country_i_num i.country_j_num i.year, cluster(panel_id)

* Predict bilateral openness
predict linear_prediction, xb
gen omega_hat = exp(linear_prediction)

* Aggregate predicted bilateral openness to get multilateral openness
egen multilateral_openness = total(omega_hat), by(country_i_num year)

* Save results
save results2.dta, replace





* Display log-likelihood
estat ic

export delimited using "DATA_IV.csv", replace



* 산점도 생성
twoway (scatter flow_num omega_hat)

reg flow_num multilateral_openness
reg flow_num omega_hat

* 잔차 계산
gen residuals = flow_num - omega_hat

* 잔차의 분포 확인 (히스토그램)
histogram residuals, normal

* 잔차의 산점도 (실제 값 vs 잔차)
twoway (scatter residuals flow_num)
