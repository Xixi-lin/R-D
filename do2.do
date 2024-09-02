import excel "C:\Users\15253\Desktop\毕业论文\数据\回归7.xlsx", sheet("Sheet1") firstrow clear
save data01.dta, replace

//数据处理
gen did = 1 if Year>2015 & industy == 0
replace did = 0 if did == .
gen jiaohu = did*resp
gen jiaohu1 = did*resp1
gen jiaohu2 = did*resp2
gen size=ln(size1)
gen income=ln(income1+1)
gen sign_cash = sign(cash1)  
replace cash1 = abs(cash1) +1
gen cash = sign_cash * ln(cash1)
gen profit = ln(profit1)
//gen sign_tax = sign(tax1)  
//replace tax1 = abs(tax1) +1
//gen tax = sign_tax * ln(tax1)
gen invia=Invia+Umia
gen pInvia= Invia/number
encode Scode,gen(id)
encode industycode,gen(indcode)
encode city,gen(city1)
// gen policy = cond(Year > 2015, 2016, 2100)
gen policy = 2016 if  industy == 0
replace policy = 2100 if policy == .
//gen policy = 2016
//replace policy = 2100 if policy == .
//gen gdzc=ln(gdzc1)
save data01.dta,replace


//描述性统计
logout, save(logout-sum) word dec(4) replace :sum Invia invia jiaohu jiaohu1 jiaohu2 ///
lev Top income size number roa groincome TBq wct tax industy gaoxin 





//基准回归
///reghdfe per_invia jiaohu cash lev Top income tax size,absorb(Scode Year)
cd C:\Users\15253\Desktop\毕业论文\数据
use data01.dta,clear
reghdfe Invia jiaohu if industy==0 ,absorb(Scode Year)
est sto m1
reghdfe invia jiaohu if industy==0 ,absorb(Scode Year)
est sto m2
reghdfe Invia jiaohu lev Top income size number roa groincome TBq wct tax ,absorb(Scode Year)
est sto m3
reghdfe invia jiaohu lev Top income size number roa groincome TBq wct tax ,absorb(Scode Year)
est sto m4
esttab m1 m2 m3 m4, b(%6.4f) se(%6.4f)  star(* 0.1 ** 0.05 *** 0.01)  scalar(N r2) compress nogaps
outreg2 [m1 m2 m3 m4] using 基准回归01.xls, replace  dec(4) addtext(Company FE, YES,Year FE, YES)
//outreg2 using regress1.doc,replace dec(4)


// logout, save(logout-sum) word dec(4) replace :sum 
//use data.dta,clear
//per_invia jiaohu cash lev Top income Year 

//reghdfe per_invia jiaohu,absorb(Scode Year)

//平行趋势检验
use data01.dta,clear
xtset id Year
global conval lev Top income size number roa groincome TBq wct tax

gen policy_multi = Year - policy
replace policy_multi = -5 if policy_multi <= -5
replace policy_multi = 5 if policy_multi >= 5

forvalues i=5(-1)1{
  gen pre_multi`i'=(policy_multi==-`i')
}
gen current_multi= (policy_multi==0)
forvalues i=1(1)5{
  gen post_multi`i'=(policy_multi==`i')
}

drop pre_multi5


reghdfe Invia pre_multi* current_multi post_multi*  $conval ,absorb(id Year)
coefplot, baselevels ///
keep(pre_multi* current_multi post_multi*) ///
coeflabels(pre_multi4 = "-1" ///
pre_multi3 = "-3" ///
pre_multi2 = "-2" ///
pre_multi1 = "-1" ///
current_multi = "0" ///
post_multi1 = "1" ///
post_multi2 = "2" ///
post_multi3 = "3" ///
post_multi4 = "4" ///
post_multi5 = "5" ///
) ///
vertical ///
yline(0,lcolor(edkblue*0.8)) ///
xline(5, lwidth(vthin) lpattern(dash) lcolor(teal)) ///
ylabel(,labsize(*0.75)) ///
xlabel(,labsize(*0.75)) ///
ytitle("政策动态效应", size(small)) ///
xtitle("政策时点", size(small)) ///
addplot(line @b @at) ///
ciopts(lpattern(dash) recast(rcap) msize(medium)) ///
msymbol(circle_hollow) ///
scheme(s1mono)

// local titles = "平行趋势检验" + "`y'"
graph export 平行趋势检验7-01.tif, replace

use data01.dta,clear
xtset id Year
global conval lev Top income size number roa groincome TBq wct tax

gen policy_multi = Year - policy
replace policy_multi = -5 if policy_multi <= -5
replace policy_multi = 5 if policy_multi >= 5

forvalues i=5(-1)1{
  gen pre_multi`i'=(policy_multi==-`i')
}
gen current_multi= (policy_multi==0)
forvalues i=1(1)5{
  gen post_multi`i'=(policy_multi==`i')
}

drop pre_multi4


reghdfe invia pre_multi* current_multi post_multi*  $conval ,absorb(id Year)
coefplot, baselevels ///
keep(pre_multi* current_multi post_multi*) ///
coeflabels(pre_multi4 = "-1" ///
pre_multi3 = "-3" ///
pre_multi2 = "-2" ///
pre_multi1 = "-1" ///
current_multi = "0" ///
post_multi1 = "1" ///
post_multi2 = "2" ///
post_multi3 = "3" ///
post_multi4 = "4" ///
post_multi5 = "5" ///
) ///
vertical ///
yline(0,lcolor(edkblue*0.8)) ///
xline(5, lwidth(vthin) lpattern(dash) lcolor(teal)) ///
ylabel(,labsize(*0.75)) ///
xlabel(,labsize(*0.75)) ///
ytitle("政策动态效应", size(small)) ///
xtitle("政策时点", size(small)) ///
addplot(line @b @at) ///
ciopts(lpattern(dash) recast(rcap) msize(medium)) ///
msymbol(circle_hollow) ///
scheme(s1mono)

// local titles = "平行趋势检验" + "`y'"
graph export 平行趋势检验7-02.tif, replace



cd C:\Users\15253\Desktop\毕业论文\数据
use data01.dta,clear
//稳健性检验1 被解释变量
//reghdfe invia jiaohu lev Top income size number roa groincome TBq wct tax,absorb(Scode Year)
//est sto m1

reghdfe invia jiaohu lev Top income size number roa groincome TBq wct tax
est sto m1
reghdfe invia jiaohu lev Top income size number roa groincome TBq wct tax ,absorb(Scode) 
est sto m2
reghdfe invia jiaohu lev Top income size number roa groincome TBq wct tax ,absorb(Year)
est sto m3
reghdfe invia jiaohu lev Top income size number roa groincome TBq wct tax ,absorb(Scode Year)
est sto m4
esttab m1 m2 m3 m4, b(%6.4f) se(%6.4f)  star(* 0.1 ** 0.05 *** 0.01)  scalar(N r2) compress nogaps
outreg2 [m1 m2 m3 m4] using 稳健性检验7-1.xls, replace  dec(4) addtext(Company FE, YES,Year FE, YES)

//稳健性检验2 核心解释变量
reghdfe Invia jiaohu1,absorb(Scode Year)
est sto m1
reghdfe invia jiaohu1,absorb(Scode Year)
est sto m2
reghdfe Invia jiaohu1 lev Top income size number roa groincome TBq wct tax,absorb(Scode Year)
est sto m3
reghdfe invia jiaohu1 lev Top income size number roa groincome TBq wct tax,absorb(Scode Year)
est sto m4
reghdfe Invia jiaohu2,absorb(Scode Year)
est sto m5
reghdfe invia jiaohu2,absorb(Scode Year)
est sto m6
reghdfe Invia jiaohu2 lev Top income size number roa groincome TBq wct tax,absorb(Scode Year)
est sto m7
reghdfe Invia jiaohu2 lev Top income size number roa groincome TBq wct tax,absorb(Scode Year)
est sto m8
esttab m1 m2 m3 m4 m5 m6 m7 m8, b(%6.4f) se(%6.4f)  star(* 0.1 ** 0.05 *** 0.01)  scalar(N r2) compress nogaps
outreg2 [m1 m2 m3 m4 m5 m6 m7 m8] using 稳健性检验7-02.xls, replace  dec(3) addtext(Company FE, YES,Year FE, YES)


//稳健性检验3 加入城市固定效应


reghdfe Invia jiaohu lev Top income size number roa groincome TBq wct tax,absorb(Scode Year Year#city1)
est sto m1
reghdfe invia jiaohu lev Top income size number roa groincome TBq wct tax,absorb(Scode Year Year#city1)
est sto m2

reghdfe Invia jiaohu lev Top income size number roa groincome TBq wct tax,absorb(Scode Year Year#indcode)
est sto m3
reghdfe invia jiaohu lev Top income size number roa groincome TBq wct tax,absorb(Scode Year Year#indcode)
est sto m4
esttab m1 m2 m3 m4, b(%6.4f) se(%6.4f)  star(* 0.1 ** 0.05 *** 0.01)  scalar(N r2) compress nogaps
outreg2 [m1 m2 m3 m4] using 稳健性检验7-03.xls, replace  dec(4) addtext(Company FE, YES,Year FE, YES)


//稳健性检验4 减少样本
//reghdfe invia jiaohu lev Top income size number roa groincome TBq wct tax,absorb(Scode Year)
//est sto m1

reghdfe Invia jiaohu lev Top income size number roa groincome TBq wct tax if Year<2019
est sto m1
reghdfe invia jiaohu lev Top income size number roa groincome TBq wct tax if Year<2019
est sto m2
reghdfe Invia jiaohu lev Top income size number roa groincome TBq wct tax if Year<2019 ,absorb(Scode Year)
est sto m3
reghdfe invia jiaohu lev Top income size number roa groincome TBq wct tax if Year<2019 ,absorb(Scode Year)
est sto m4
esttab m1 m2 m3 m4, b(%6.4f) se(%6.4f)  star(* 0.1 ** 0.05 *** 0.01)  scalar(N r2) compress nogaps
outreg2 [m1 m2 m3 m4] using 稳健性检验7-04.xls, replace  dec(4) addtext(Company FE, YES,Year FE, YES)



//安慰剂检验2个体
cd C:\Users\15253\Desktop\毕业论文\数据
use data01.dta,clear
global yvar Invia 
foreach y in $yvar{

mat b = J(500,1,0) 
mat se = J(500,1,0) 
mat p = J(500,1,0) 



forvalues i=1/500{
 use data01.dta, clear
 qui xtset id Year  
 keep if Year==2016
 sample 320, count  
 keep id  
 save match_id.dta, replace  
 qui merge 1:m id using  data01.dta 
 gen treat = (_merge == 3)  
 gen period = (Year >= 2016)  
 gen didd = treat*period
 qui reghdfe `y' didd lev Top income size number roa groincome TBq wct tax,absorb(id Year) 
  

 mat b[`i',1] = _b[did]
 mat se[`i',1] = _se[did]
 mat p[`i',1] = 2*ttail(e(df_r), abs(_b[did]/_se[did]))
}


svmat b, names(coef)
svmat se, names(se)
svmat p, names(pvalue)


drop if pvalue1 == .
label var pvalue1 
label var coef1 
keep coef1 se1 pvalue1 



twoway (kdensity coef1,yaxis(1)) (scatter pvalue1 coef1, yaxis(2) msymbol(smcircle_hollow) mcolor(blue)), ///
xlabel(-20(4)20) ylabel(,angle(0)) ///
xline(2.048841, lwidth(vthin) lp(shortdash)) xtitle("系数") ///
yline(0.1,lwidth(vthin) lp(dash) axis(2)) ytitle("密度") ///
ytitle("p值",axis(2)) ///
legend(label(1 "核密度曲线") label( 2 "p值")) ///
plotregion(style(none)) ///
graphregion(color(white))  
local titles = "安慰剂检验" + "`y'"
graph export `titles'.tif,replace
}

//安慰剂检验2个体
cd C:\Users\15253\Desktop\毕业论文\数据
use data01.dta,clear
global yvar invia 
foreach y in $yvar{

mat b = J(500,1,0) 
mat se = J(500,1,0) 
mat p = J(500,1,0) 



forvalues i=1/500{
 use data01.dta, clear
 qui xtset id Year  
 keep if Year==2016
 sample 320, count  
 keep id  
 save match_id.dta, replace  
 qui merge 1:m id using  data01.dta 
 gen treat = (_merge == 3)  
 gen period = (Year >= 2016)  
 gen didd = treat*period
 qui reghdfe `y' didd lev Top income size number roa groincome TBq wct tax,absorb(id Year) 
  

 mat b[`i',1] = _b[did]
 mat se[`i',1] = _se[did]
 mat p[`i',1] = 2*ttail(e(df_r), abs(_b[did]/_se[did]))
}


svmat b, names(coef)
svmat se, names(se)
svmat p, names(pvalue)


drop if pvalue1 == .
label var pvalue1 
label var coef1 
keep coef1 se1 pvalue1 



twoway (kdensity coef1,yaxis(1)) (scatter pvalue1 coef1, yaxis(2) msymbol(smcircle_hollow) mcolor(blue)), ///
xlabel(-20(4)20) ylabel(,angle(0)) ///
xline(2.048841, lwidth(vthin) lp(shortdash)) xtitle("系数") ///
yline(0.1,lwidth(vthin) lp(dash) axis(2)) ytitle("密度") ///
ytitle("p值",axis(2)) ///
legend(label(1 "核密度曲线") label( 2 "p值")) ///
plotregion(style(none)) ///
graphregion(color(white))  
local titles = "安慰剂检验" + "`y'"
graph export `titles'.tif,replace
}
//安慰剂检验时间
cd C:\Users\15253\Desktop\毕业论文\数据
use data01.dta,clear
forvalues i=1/5{
gen did`i' = 1 if Year>2015-`i' & industy == 0
replace did`i' = 0 if did`i' == .
//drop if missing(resp3)
gen jiaohu_time`i'=did`i'*resp
reghdfe Invia jiaohu_time`i' lev Top income size number roa groincome TBq wct tax,absorb(id Year province) cluster(Scode province)
est sto m`i'
}
esttab m1 m2 m3 m4 m5, b(%6.4f) se(%6.4f)  star(* 0.1 ** 0.05 *** 0.01)  scalar(N r2) compress nogaps
outreg2 [m1 m2 m3 m4 m5] using 稳健性检验7-04时间.xls, replace  dec(4) addtext(Company FE, YES,Year FE, YES)


//安慰剂检验时间
cd C:\Users\15253\Desktop\毕业论文\数据
use data01.dta,clear
forvalues i=1/5{
gen did`i' = 1 if Year>2015-`i' & industy == 0
replace did`i' = 0 if did`i' == .
//drop if missing(resp3)
gen jiaohu_time`i'=did`i'*resp
reghdfe invia jiaohu_time`i' lev Top income size number roa groincome TBq wct tax,absorb(id Year province) cluster(Scode province)
est sto m`i'
}
esttab m1 m2 m3 m4 m5, b(%6.4f) se(%6.4f)  star(* 0.1 ** 0.05 *** 0.01)  scalar(N r2) compress nogaps
outreg2 [m1 m2 m3 m4 m5] using 稳健性检验7-004时间.xls, replace  dec(4) addtext(Company FE, YES,Year FE, YES)

****************************************
* PSM-DID
	* 注意，在最开始将面板数据拆分成截面数据分别进行psm时，循环中不能包含暂元。为什么？
	* forvalue forval forvalues有什么区别
	* 一直出错的原因，psmatch2后面的变量必须是01变量，所以不能是did，因为did只是特定个体在特定的时间取1
	* 如在2016年，did取值全部都是0（因为绿色金融试点政策是在2017年之后推行的）
	* 此时did变量不是01变量，没有取值为1
	* 所以，psmatch2后面应当是时点城市
*****************************************


*******************************************
* PSM-DID分截面数据
	* preserve。把当前数据集的状态保存到内存中 
	* set seed 。设定种子是为了让结果可重复
	* 使用append命令将2008.dta~2019.dta首尾相连拼接到2007.dta后面，生成新的数据文件ybydata.dta
*********************************************
cd C:\Users\15253\Desktop\毕业论文\数据
use data01.dta,clear
xtset id Year
global conval lev Top income size number roa groincome TBq wct tax
gen treated=1 if industy==0
replace treated = 0 if treated == .


forvalue i = 2007/2022{
preserve 
capture {
	keep if Year == `i'
	set seed 0000	
	gen  norvar_2 = rnormal()
	sort norvar_2
	psmatch2 treated $conval , outcome(Invia) logit neighbor(2) ties common ate caliper(0.05)
	save `i'.dta, replace
		}
restore
}

clear all


use 2007.dta, clear
forvalue k =2008/2022 {
      capture {
          append using `k'.dta
          }
      }

save ybydata.dta, replace


***********************************
* PSM-DID
	* 近邻匹配。带卡尺的1:2近邻匹配
	* 核半径匹配
************************************
cd C:\Users\15253\Desktop\毕业论文\数据
use ybydata.dta,clear
global conval lev Top income size number roa groincome TBq wct tax

gen common=_support	
psgraph
reghdfe Invia jiaohu $conval if common==1
est sto m1
reghdfe invia jiaohu $conval if common==1
est sto m2
reghdfe Invia jiaohu $conval if common==1 , absorb(id Year)  
est sto m3
reghdfe invia jiaohu $conval if common==1 , absorb(id Year) 
est sto m4
esttab m1 m2 m3 m4, b(%6.4f) se(%6.4f)  star(* 0.1 ** 0.05 *** 0.01)  scalar(N r2) compress nogaps
outreg2 [m1 m2 m3 m4] using 稳健性检验7-05PSM-DID.xls, replace  dec(4) addtext(Company FE, YES,Year FE, YES)



//稳健性检验8前置一期
cd C:\Users\15253\Desktop\毕业论文\数据
use data01.dta,clear
sort id Year
gen Invia1 = .
by id: replace Invia1 = Invia[_n+1] if Year != Year[_n+1] & id == id[_n+1]
gen invia1 = .
by id: replace invia1 = invia[_n+1] if Year != Year[_n+1] & id == id[_n+1]

reghdfe Invia1 jiaohu lev Top income size number roa groincome TBq wct tax
est sto m1
reghdfe invia1 jiaohu lev Top income size number roa groincome TBq wct tax
est sto m2
reghdfe Invia1 jiaohu lev Top income size number roa groincome TBq wct tax ,absorb(Scode Year)
est sto m3
reghdfe invia1 jiaohu lev Top income size number roa groincome TBq wct tax ,absorb(Scode Year)
est sto m4
esttab m1 m2 m3 m4, b(%6.4f) se(%6.4f)  star(* 0.1 ** 0.05 *** 0.01)  scalar(N r2) compress nogaps
outreg2 [m1 m2 m3 m4] using 稳健性检验08.xls, replace  dec(4) addtext(Company FE, YES,Year FE, YES)











//psmatch2 treated $conval,kernel out(Invia) comm
//teffects psmatch (Invia) ($conval), method(hungarian)
// psmatch2 treated $conval,kernel out(Invia) comm
//gen    weight = _weight * 2
//replace weight = 1 if treated == 1 & _weight != .
// reghdfe Invia jiaohu $conval [pweight = weight], absorb(id Year) 
// est sto m2
// esttab m1 m2, b(%6.4f) se(%6.4f)  star(* 0.1 ** 0.05 *** 0.01)  scalar(N r2) compress nogaps
// outreg2 [m1 m2] using 稳健性检验_PSM-DID核半径匹配.xls, replace  dec(4) addtext(Company FE, YES,Year FE, YES)







cd C:\Users\15253\Desktop\毕业论文\数据
use data01.dta,clear
//异质性分析1东中西部
gen region = .  
replace region = 1 if inlist(province, "北京市", "天津市", "河北省", "山东省", "江苏省", "上海市", "浙江省") 
replace region = 1 if inlist(province, "福建省", "广东省", "海南省", "辽宁省" ) 
replace region = 2 if inlist(province, "山西省", "安徽省", "江西省", "河南省", "湖北省", "湖南省", "吉林省", "黑龙江省")  
replace region = 3 if region== .

reghdfe Invia jiaohu lev Top income size number roa groincome TBq wct tax if region==1 , absorb(Scode Year)
est sto m1
reghdfe invia jiaohu lev Top income size number roa groincome TBq wct tax if region==1 , absorb(Scode Year) 
est sto m2
reghdfe Invia jiaohu lev Top income size number roa groincome TBq wct tax if region==2 , absorb(Scode Year) 
est sto m3
reghdfe invia jiaohu lev Top income size number roa groincome TBq wct tax if region==2 , absorb(Scode Year) 
est sto m4
reghdfe Invia jiaohu lev Top income size number roa groincome TBq wct tax if region==3 , absorb(Scode Year) 
est sto m5
reghdfe invia jiaohu lev Top income size number roa groincome TBq wct tax if region==3 , absorb(Scode Year) 
est sto m6
esttab m1 m2 m3 m4 m5 m6, b(%6.4f) se(%6.4f)  star(* 0.1 ** 0.05 *** 0.01)  scalar(N r2) compress nogaps
outreg2 [m1 m2 m3 m4 m5 m6] using 异质性分析01.xls, replace  dec(4) addtext(Company FE, YES,Year FE, YES)

cd C:\Users\15253\Desktop\毕业论文\数据
use data01.dta,clear
//异质性分析2国企
reghdfe Invia jiaohu lev Top income size number roa groincome TBq wct tax if soe==1 , absorb(Scode Year)
est sto m1
reghdfe invia jiaohu lev Top income size number roa groincome TBq wct tax if soe==1 , absorb(Scode Year) 
est sto m2
reghdfe Invia jiaohu lev Top income size number roa groincome TBq wct tax if soe==0 , absorb(Scode Year)
est sto m3
reghdfe invia jiaohu lev Top income size number roa groincome TBq wct tax if soe==0 , absorb(Scode Year) 
est sto m4
esttab m1 m2 m3 m4, b(%6.4f) se(%6.4f)  star(* 0.1 ** 0.05 *** 0.01)  scalar(N r2) compress nogaps
outreg2 [m1 m2 m3 m4] using 异质性分析02.xls, replace  dec(4) addtext(Company FE, YES,Year FE, YES)

//异质性分析3高新

reghdfe Invia jiaohu lev Top income size number roa groincome TBq wct tax if gaoxin==0 , absorb(Scode Year)
est sto m1
reghdfe invia jiaohu lev Top income size number roa groincome TBq wct tax if gaoxin==0 , absorb(Scode Year) 
est sto m2
reghdfe Invia jiaohu lev Top income size number roa groincome TBq wct tax if gaoxin==1 , absorb(Scode Year)
est sto m3
reghdfe invia jiaohu lev Top income size number roa groincome TBq wct tax if gaoxin==1 , absorb(Scode Year) 
est sto m4
esttab m1 m2 m3 m4, b(%6.4f) se(%6.4f)  star(* 0.1 ** 0.05 *** 0.01)  scalar(N r2) compress nogaps
outreg2 [m1 m2 m3 m4] using 异质性分析03.xls, replace  dec(4) addtext(Company FE, YES,Year FE, YES)

//异质性分析4 投入水平
//summarize RDRatio, detail
//local median = r(p50)
gen group1 = .
gen group2 = .
gen aaa=5
gen bbb=10 
replace group1 = 1 if RDRatio <= aaa
replace group1 = 2 if RDRatio > aaa
replace group2 = 1 if RDRatio <= bbb
replace group2 = 2 if RDRatio > bbb
reghdfe Invia jiaohu lev Top income size number roa groincome TBq wct tax if group1==1 , absorb(Scode Year) 
est sto m1
reghdfe invia jiaohu lev Top income size number roa groincome TBq wct tax if group1==1 , absorb(Scode Year) 
est sto m2
reghdfe Invia jiaohu lev Top income size number roa groincome TBq wct tax if group1==2 , absorb(Scode Year) 
est sto m3
reghdfe invia jiaohu lev Top income size number roa groincome TBq wct tax if group1==2 , absorb(Scode Year) 
est sto m4
reghdfe Invia jiaohu lev Top income size number roa groincome TBq wct tax if group2==1 , absorb(Scode Year) 
est sto m5
reghdfe invia jiaohu lev Top income size number roa groincome TBq wct tax if group2==1 , absorb(Scode Year) 
est sto m6
reghdfe Invia jiaohu lev Top income size number roa groincome TBq wct tax if group2==2 , absorb(Scode Year) 
est sto m7
reghdfe invia jiaohu lev Top income size number roa groincome TBq wct tax if group2==2 , absorb(Scode Year) 
est sto m8
esttab m1 m2 m3 m4 m5 m6 m7 m8, b(%6.4f) se(%6.4f)  star(* 0.1 ** 0.05 *** 0.01)  scalar(N r2) compress nogaps
outreg2 [m1 m2 m3 m4 m5 m6 m7 m8] using 异质性分析04.xls, replace  dec(2) addtext(Company FE, YES,Year FE, YES)



//机制分析
import excel "C:\Users\15253\Desktop\毕业论文\数据\回归8.xlsx", sheet("Sheet1") firstrow clear
save data8.dta, replace

cd C:\Users\15253\Desktop\毕业论文\数据
use data8.dta,clear

gen did = 1 if Year>2015 & industy == 0
replace did = 0 if did == .
gen jiaohu = did*resp
// gen jiaohu1 = did*resp1
// gen jiaohu2 = did*resp2
gen size=ln(size1)
gen income=ln(income1+1)
// gen sign_cash = sign(cash1)  
// replace cash1 = abs(cash1) +1
// gen cash = sign_cash * ln(cash1)
gen profit = ln(profit1)
gen RDSS=ln(RDSpendSum+1)
//gen sign_tax = sign(tax1)  
//replace tax1 = abs(tax1) +1
//gen tax = sign_tax * ln(tax1)
gen invia=Invia+Umia
gen gdzc=ln(gdzc1)
encode Scode,gen(id)
// encode industycode,gen(indcode)
encode city,gen(city1)
// gen policy = cond(Year > 2015, 2016, 2100)
// gen policy = 2016 if  industy == 0
// replace policy = 2100 if policy == .
//gen policy = 2016
//replace policy = 2100 if policy == .
//gen gdzc=ln(gdzc1)
save data8.dta,replace


reghdfe RDPersonRatio jiaohu lev Top income number roa groincome TBq wct tax,absorb(Scode Year)
est sto m1
reghdfe RDPerson jiaohu lev Top income number roa groincome TBq wct tax,absorb(Scode Year)
est sto m2
reghdfe gdzc jiaohu lev Top income number roa groincome TBq wct tax,absorb(Scode Year)
est sto m3
reghdfe RDSS jiaohu lev Top income number roa groincome TBq wct tax,absorb(Scode Year)
est sto m4

esttab m1 m2 m3 m4, b(%6.4f) se(%6.4f)  star(* 0.1 ** 0.05 *** 0.01)  scalar(N r2) compress nogaps
outreg2 [m1 m2 m3 m4] using 机制分析前01.xls, replace  dec(4) addtext(Company FE, YES,Year FE, YES)

//reghdfe Invia jiaohu RDPerson lev Top income size number roa groincome TBq wct tax,absorb(Scode Year)
//reghdfe Invia jiaohu RDPersonRatio RDPerson lev Top income size number roa groincome TBq wct tax,absorb(Scode Year)
reghdfe Invia jiaohu RDPersonRatio lev Top income size number roa groincome TBq wct tax,absorb(Scode Year)
est sto m1
reghdfe Invia jiaohu RDPerson lev Top income size number roa groincome TBq wct tax,absorb(Scode Year)
est sto m2
reghdfe Invia jiaohu gdzc lev Top income size number roa groincome TBq wct tax,absorb(Scode Year)
est sto m3
reghdfe Invia jiaohu RDSS lev Top income size number roa groincome TBq wct tax,absorb(Scode Year)
est sto m4
reghdfe Invia jiaohu RDPersonRatio RDPerson gdzc RDSS lev Top income size number roa groincome TBq wct tax,absorb(Scode Year)
est sto m5


esttab m1 m2 m3 m4 m5, b(%6.4f) se(%6.4f)  star(* 0.1 ** 0.05 *** 0.01)  scalar(N r2) compress nogaps
outreg2 [m1 m2 m3 m4 m5] using 机制分析后01.xls, replace  dec(4) addtext(Company FE, YES,Year FE, YES)

reghdfe invia jiaohu RDPersonRatio lev Top income size number roa groincome TBq wct tax,absorb(Scode Year)
est sto m1
reghdfe invia jiaohu RDPerson lev Top income size number roa groincome TBq wct tax,absorb(Scode Year)
est sto m2
reghdfe invia jiaohu gdzc lev Top income size number roa groincome TBq wct tax,absorb(Scode Year)
est sto m3
reghdfe invia jiaohu RDSS lev Top income size number roa groincome TBq wct tax,absorb(Scode Year)
est sto m4
reghdfe invia jiaohu RDPersonRatio RDPerson gdzc RDSS lev Top income size number roa groincome TBq wct tax,absorb(Scode Year)
est sto m5


esttab m1 m2 m3 m4 m5, b(%6.4f) se(%6.4f)  star(* 0.1 ** 0.05 *** 0.01)  scalar(N r2) compress nogaps
outreg2 [m1 m2 m3 m4 m5] using 机制分析后02.xls, replace  dec(4) addtext(Company FE, YES,Year FE, YES)





