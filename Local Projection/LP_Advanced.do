* ssc install estout
* ssc install locproj

clear
cap drop _all
cap graph drop _all


**** 데이터 불러오기 ****

import delimited "DATA_IV_C.csv", clear

keep if advanced == 1

describe // 데이터 구조 확인


**** 데이터 처리 ****

destring trade markup t10b50 top1 top10 gini unemp rgdp gdpg gspend ngov inf trade_iv, replace force // 'NA'으로 인해 문자형으로 저장된 데이터를 숫자형으로 변환 

describe

encode country, gen(country_id) // 문자열 변수 country를 숫자형 변수로 변환

xtset country_id year, yearly // 패널 데이터로 설정

gen lrgdp = 100 * ln(rgdp) // 로그 변환


**** Local Projection IRF for Markup ****

// locproj markup, tr(cmlt) yl(3) h(5) s(d.trade_iv) sl(3) c(l(0/3).d.rgdp l(0/3).d.inf l(0/3).d.unemp l(0/3).d.ngov) fe cluster(country_id) z conf(90 80) gropt(ytitle (Percent)) tti(Year) title(Response of Markup to a Shock to Trade_IV) // Good!

locproj markup, tr(cmlt) yl(3) h(4) s(l.d.trade_iv) sl(5) c(l(1/5).d.lrgdp l(1/5).d.inf l(1/5).d.unemp l(1/5).d.ngov i.year) fe cluster(country_id) z conf(95 85) tti(Year) title(Response of Markup to a Shock to Trade_IV (Advanced)) // 최적 시차 적용

capture log close
log using "irf_adv_markup.txt", text replace
locproj markup, tr(cmlt) yl(3) h(4) s(l.d.trade_iv) sl(5) c(l(1/5).d.lrgdp l(1/5).d.inf l(1/5).d.unemp l(1/5).d.ngov i.year) fe cluster(country_id) z conf(95 85) tti(Year) title(Response of Markup to a Shock to Trade_IV (Advanced))
log close


**** Local Projection IRF for Income ineqaulity ****

** Trade_IV -> Income Inequality
locproj gini, tr(cmlt) yl(3) h(4) s(l.d.trade_iv) sl(5) c(l(1/5).d.lrgdp l(1/5).d.inf l(1/5).d.unemp l(1/5).d.ngov i.year) fe cluster(country_id) z conf(95 85) tti(Year) title(Response of GINI to a Shock to Trade_IV (Advanced))

capture log close
log using "irf_adv_gini.txt", text replace
locproj gini, tr(cmlt) yl(3) h(4) s(l.d.trade_iv) sl(5) c(l(1/5).d.lrgdp l(1/5).d.inf l(1/5).d.unemp l(1/5).d.ngov i.year) fe cluster(country_id) z conf(95 85) tti(Year) title(Response of GINI to a Shock to Trade_IV (Advanced))
log close


locproj t10b50, tr(cmlt) yl(3) h(4) s(l.d.trade_iv) sl(5) c(l(1/5).d.lrgdp l(1/5).d.inf l(1/5).d.unemp l(1/5).d.ngov i.year) fe cluster(country_id) z conf(95 85) tti(Year) title(Response of Top 10 / Bot 50 to a Shock to Trade_IV (Advanced))

capture log close
log using "irf_adv_t10b50.txt", text replace
locproj t10b50, tr(cmlt) yl(3) h(4) s(l.d.trade_iv) sl(5) c(l(1/5).d.lrgdp l(1/5).d.inf l(1/5).d.unemp l(1/5).d.ngov i.year) fe cluster(country_id) z conf(95 85) tti(Year) title(Response of Top 10 / Bot 50 to a Shock to Trade_IV (Advanced))
log close


locproj top1, tr(cmlt) yl(3) h(4) s(l.d.trade_iv) sl(5) c(l(1/5).d.lrgdp l(1/5).d.inf l(1/5).d.unemp l(1/5).d.ngov i.year) fe cluster(country_id) z conf(95 85) tti(Year) title(Response of Top 1 to a Shock to Trade_IV (Advanced))

capture log close
log using "irf_adv_top1.txt", text replace
locproj top1, tr(cmlt) yl(3) h(4) s(l.d.trade_iv) sl(5) c(l(1/5).d.lrgdp l(1/5).d.inf l(1/5).d.unemp l(1/5).d.ngov i.year) fe cluster(country_id) z conf(95 85) tti(Year) title(Response of Top 1 to a Shock to Trade_IV (Advanced))
log close


locproj top10, tr(cmlt) yl(3) h(4) s(l.d.trade_iv) sl(5) c(l(1/5).d.lrgdp l(1/5).d.inf l(1/5).d.unemp l(1/5).d.ngov i.year) fe cluster(country_id) z conf(95 85) tti(Year) title(Response of Top 10 to a Shock to Trade_IV (Advanced))

capture log close
log using "irf_adv_top10.txt", text replace
locproj top10, tr(cmlt) yl(3) h(4) s(l.d.trade_iv) sl(5) c(l(1/5).d.lrgdp l(1/5).d.inf l(1/5).d.unemp l(1/5).d.ngov i.year) fe cluster(country_id) z conf(95 85) tti(Year) title(Response of Top 10 to a Shock to Trade_IV (Advanced))
log close


** Markup -> Income Inequality
//locproj gini, tr(cmlt) yl(3) h(4) s(l.d.markup) sl(5) c(l(1/5).d.lrgdp l(1/5).d.inf l(1/5).d.unemp l(1/5).d.ngov i.year) fe cluster(country_id) z conf(90 80) tti(Year) title(Response of GINI to a Shock to Markup)

// locproj t10b50, tr(cmlt) yl(3) h(4) s(l.d.markup) sl(5) c(l(1/5).d.lrgdp l(1/5).d.inf l(1/5).d.unemp l(1/5).d.ngov i.year) fe cluster(country_id) z conf(90 80) tti(Year) title(Response of Top 10 / Bot 50 to a Shock to Markup)

// locproj top1, tr(cmlt) yl(3) h(4) s(l.d.markup) sl(5) c(l(1/5).d.lrgdp l(1/5).d.inf l(1/5).d.unemp l(1/5).d.ngov i.year) fe cluster(country_id) z conf(90 80) tti(Year) title(Response of Top 1 to a Shock to markup)

// locproj top10, tr(cmlt) yl(3) h(4) s(l.d.markup) sl(5) c(l(1/5).d.lrgdp l(1/5).d.inf l(1/5).d.unemp l(1/5).d.ngov i.year) fe cluster(country_id) z conf(90 80) tti(Year) title(Response of Top 10 to a Shock to markup)
