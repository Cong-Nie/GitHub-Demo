


******************** 股权激励公告日数据 ********************

import excel "股权激励数据.xlsx", sheet("3") firstrow clear
* keep if 股灾剔除样本==0
bysort stkcd: gen n=_n
tostring n, replace
gen id=stkcdN+n
drop stkcd n
save event_date,replace
 
import excel "股权激励数据.xlsx", sheet("3") firstrow clear
* keep if 股灾剔除样本==0
bysort stkcd: gen event_num=_N
keep stkcdN stkcd event_num
duplicates drop stkcdN stkcd event_num, force	
save event_num, replace

******************** 合并数据 ********************

use 个股交易数据.dta, clear
merge m:1 stkcd using event_num
keep if _m==3
drop _m
expand event_num
bysort stkcd date: gen n=_n
tostring n, replace
gen id=stkcdN+n
drop n
merge m:1 id using event_date
drop _m

egen ID = group(id)
sort ID date

* 剔除股灾区间的样本
egen dn=group(date) 
order stkcd date dn
drop if dn>=1321 & dn<=1371
drop if dn>=1457 & dn<=1461

bysort ID: gen date_num=_n
xtset ID date_num
by ID: gen target=date_num if date==FirstDeclareDate

* 部分公司公告日不是在交易日
gen  dateN=date(date,"YMD")
format dateN %td
gen  FirstDeclareDateN=date(FirstDeclareDate,"YMD")
format FirstDeclareDateN %td
gen week=dow(公告日期)
by ID: replace target=date_num if dateN==FirstDeclareDateN+2 & week==6
drop week
egen td=min(target), by(ID)

* 其他时间
gen temp=dateN-FirstDeclareDateN
replace temp=9999 if temp<0 
egen min_dif = min(temp) if td==. , by(ID)
drop if min_dif==9999
replace target=date_num if temp==min_dif
drop td temp min_dif
egen td=min(target), by(ID)
gen dif=date_num-td


* 事件窗口期
by ID: gen event_window=1 if dif>=-30 & dif<=30
by ID: gen estimation_window=1 if dif>=-230 & dif<-30
replace event_window=0 if event_window==.
replace estimation_window=0 if estimation_window==.
drop if event_window==0 & estimation_window==0

* 窗口期不够的删掉
by ID: gen N=_N
drop if N!=261

* 剔除st交易数据
drop if trdsta!=1
drop N
bysort ID: gen N=_N
drop if N!=261

* 分主板、创业板、中小板
gen stk=substr(stkcdN,1,3)

save temp.dta, replace 

***************************************************************************************


use temp.dta, clear
*keep if stk=="002" 
*keep if stk=="300"
keep if stk!="002" & stk!="300"  

* 股权激励方式
*keep if 激励标的物==3

drop ID
egen ID=group(id)

* 估计收益率
merge m:1 date using 无风险利率数据.dta
keep if _m==3
drop _m

merge m:1 dateN using 三因子数据.dta
keep if _m==3
drop _m


gen predicted_return=.
gen R_Rf=R-Rf

sum ID
global MaxID=r(max)
forvalues i=1(1)$MaxID {
	qui reg R_Rf Rm_Rf  SMB HML if ID==`i' & estimation_window==1 
	* qui reg R_Rf Rm_Rf   if ID==`i' & estimation_window==1 
	predict p if ID==`i'
	replace predicted_return=p if ID==`i' & event_window==1
	drop p
}

//异常收益率AR
keep if event_window==1
gen AR=R_Rf-predicted_return if event_window==1

//算出特定日期的累积异常收益率
sort ID dif
by ID: egen gap_1= sum(AR) if dif>=-1 & dif<=1
by ID: egen gap_2= sum(AR) if dif>=-2 & dif<=2
by ID: egen gap_3= sum(AR) if dif>=-5 & dif<=5
by ID: egen gap_4= sum(AR) if dif>=-10 & dif<=10
by ID: egen gap_5= sum(AR) if dif>=-10 & dif<=30
by ID: egen gap_6= sum(AR) if dif>=-30 & dif<=30
by ID: egen gap_7= sum(AR) if dif>=1 & dif<=5
by ID: egen gap_8= sum(AR) if dif>=-5 & dif<=-1
by ID: egen gap_9= sum(AR) if dif>=1 & dif<=10
by ID: egen gap_10= sum(AR) if dif>=-10 & dif<=-1
by ID: egen gap_11= sum(AR) if dif>=10 & dif<=20
by ID: egen gap_12= sum(AR) if dif>=-20 & dif<=-10



save 计算完.dta, replace



***************************** 计算AAR ************************************
use  计算完.dta, clear
//分组根据dif计算AAR

global xx=31
forvalues j=1/61{
	qui sum stkcd if dif==`j'-$xx
	local Ns=r(N)
	egen mean_`j'=mean(AR) if dif==`j'-$xx
	egen sd_`j'=sd(AR) if dif==`j'-$xx

	gen t_`j'=mean_`j'/(sd_`j'/sqrt(`Ns')) if dif==`j'-$xx
	gen p_`j'=ttail(`Ns', abs(t_`j'))*2
	drop sd_`j'
}

/*
egen mean=rowmean(mean_1 mean_2 mean_3 mean_4 mean_5 mean_6 mean_7 mean_8 mean_9 mean_10 mean_11 mean_12 mean_13 mean_14 mean_15 mean_16 mean_17 mean_18 mean_19 mean_20 mean_21 mean_22 mean_23 mean_24 mean_25 mean_26 mean_27 mean_28 mean_29 mean_30 mean_31 mean_32 mean_33 mean_34 mean_35 mean_36 mean_37 mean_38 mean_39 mean_40 mean_41 )
egen t=rowmean(t_1 t_2 t_3 t_4 t_5 t_6 t_7 t_8 t_9 t_10 t_11 t_12 t_13 t_14 t_15 t_16 t_17 t_18 t_19 t_20 t_21 t_22 t_23 t_24 t_25 t_26 t_27 t_28 t_29 t_30 t_31 t_32 t_33 t_34 t_35 t_36 t_37 t_38 t_39 t_40 t_41 )
egen p=rowmean(p_1 p_2 p_3 p_4 p_5 p_6 p_7 p_8 p_9 p_10 p_11 p_12 p_13 p_14 p_15 p_16 p_17 p_18 p_19 p_20 p_21 p_22 p_23 p_24 p_25 p_26 p_27 p_28 p_29 p_30 p_31 p_32 p_33 p_34 p_35 p_36 p_37 p_38 p_39 p_40 p_41 )

*/
egen mean=rowmean(mean_1 mean_2 mean_3 mean_4 mean_5 mean_6 mean_7 mean_8 mean_9 mean_10 mean_11 mean_12 mean_13 mean_14 mean_15 mean_16 mean_17 mean_18 mean_19 mean_20 mean_21 mean_22 mean_23 mean_24 mean_25 mean_26 mean_27 mean_28 mean_29 mean_30 mean_31 mean_32 mean_33 mean_34 mean_35 mean_36 mean_37 mean_38 mean_39 mean_40 mean_41 mean_42 mean_43 mean_44 mean_45 mean_46 mean_47 mean_48 mean_49 mean_50 mean_51 mean_52 mean_53 mean_54 mean_55 mean_56 mean_57 mean_58 mean_59 mean_60 mean_61)
egen t=rowmean(t_1 t_2 t_3 t_4 t_5 t_6 t_7 t_8 t_9 t_10 t_11 t_12 t_13 t_14 t_15 t_16 t_17 t_18 t_19 t_20 t_21 t_22 t_23 t_24 t_25 t_26 t_27 t_28 t_29 t_30 t_31 t_32 t_33 t_34 t_35 t_36 t_37 t_38 t_39 t_40 t_41 t_42 t_43 t_44 t_45 t_46 t_47 t_48 t_49 t_50 t_51 t_52 t_53 t_54 t_55 t_56 t_57 t_58 t_59 t_60 t_61)
egen p=rowmean(p_1 p_2 p_3 p_4 p_5 p_6 p_7 p_8 p_9 p_10 p_11 p_12 p_13 p_14 p_15 p_16 p_17 p_18 p_19 p_20 p_21 p_22 p_23 p_24 p_25 p_26 p_27 p_28 p_29 p_30 p_31 p_32 p_33 p_34 p_35 p_36 p_37 p_38 p_39 p_40 p_41 p_42 p_43 p_44 p_45 p_46 p_47 p_48 p_49 p_50 p_51 p_52 p_53 p_54 p_55 p_56 p_57 p_58 p_59 p_60 p_61)

keep if ID==1
gen sum=sum(mean)
order mean t p

*line sum dif

*line mean dif 
*export excel using 计算完数据.xlsx, firstrow(variables) replace

***************************** 计算ACAR ************************************

use 计算完.dta, clear
//分组根据dif计算ACAR
forvalues j=1/61{
by ID: egen CAR_`j'= sum(AR) if dif<=`j'-$xx
}

keep if dif==-30

forvalues j=1/61{
	qui sum stkcd
	local Ns=r(N)
	egen a_mean_`j'=mean(CAR_`j')
	egen a_sd_`j'=sd(CAR_`j') 
	gen a_t_`j'=a_mean_`j'/(a_sd_`j'/sqrt(`Ns')) 
	gen a_p_`j'=ttail(`Ns', abs(a_t_`j'))*2
	drop a_sd_`j'
}

keep a_*



use 计算完.dta, clear

forvalues i=1/12{
	bysort stkcd: egen agap_`i'=mean(gap_`i') 
}
keep if dif==-30

* 计算t统计量和p值

sum stkcd 
local Ns=r(N)
forvalues i=1/12{
	egen mean_gap_`i'=mean(agap_`i') 
	egen sd_gap_`i'=sd(agap_`i') 
	gen t_gap_`i'=mean_gap_`i'/(sd_gap_`i'/sqrt(`Ns'))
	gen p_gap_`i'=ttail(`Ns', abs(t_gap_`i'))*2
	drop sd_gap_`i'
}

keep mean_gap_* t_gap_* p_gap_*












