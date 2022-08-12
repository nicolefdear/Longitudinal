/***********************************************************
Program: RV348B TGW code
Date: 11 June 2020
Author: Nicole Dear
Data in: RV348_merged_data_24FEB20
Data out: table1, model output
***********************************************************/

/*
1)	Table1: descriptive statistics on questions specific to TGW, e.g. duration of hormone use
2)	Table 2: bivariate analysis - comparisons of significance between TGW vs. MSM, e.g. number of partners, frequency of condom use, etc.
3)	Please report on total TGW enrolled, retained, and HIV-infected (by visit) broken down by site
*/

cd "C:\Users\ndear\Box Sync\NDear\RV348\data"
use RV348_merged_data_24FEB20.dta, clear

*drop visits that were not completed
drop if cd_visdat_nd=="Not Applicable"|cd_visdat_nd=="Not Done"

gen enrolled=1 if cd_ieyn_c=="Y"
gsort subjectnumber cd_visdat
bysort subjectnumber: carryforward enrolled, replace
gsort subjectnumber -cd_visdat
bysort subjectnumber: carryforward enrolled, replace
keep if enrolled==1

*define TGW at screening: answered A1 with “1-male” AND answered A2 with “2-female,” or answered A2 with “3-transgender woman”
gsort subjectnumber cd_visdat
gen tgw=0 if cd_sex_c=="M" & risk_a2==1
replace tgw=1 if cd_sex_c=="M" & (risk_a2==2|risk_a2==3)
replace tgw=. if visitnumber!="S"
bysort subjectnumber: carryforward tgw, replace
label def tgw 0 "cisgender MSM" 1 "transgender MSM"
label val tgw tgw
label var tgw "Gender identity"
tab tgw if visitnumber=="S", missing

/*
*gender fluidity
gsort subjectnumber cd_visdat
gen tgw=0 if cd_sex_c=="M" & risk_a2==1
replace tgw=1 if cd_sex_c=="M" & (risk_a2==2|risk_a2==3)
label def tgw 0 "cisgender MSM" 1 "transgender MSM"
label val tgw tgw

gen visit=0 if visitnumber=="S"
replace visit=1 if visitnumber=="V1"
replace visit=2 if visitnumber=="V2"
replace visit=3 if visitnumber=="V3"
replace visit=4 if visitnumber=="V4"
replace visit=5 if visitnumber=="V5"
replace visit=99 if visitnumber=="Unscheduled/Addition"

drop if visit==99
keep subjectnumber visit tgw
reshape wide tgw, i(subjectnumber) j(visit)
drop tgw1

gen fluid=1
replace fluid=0 if ((tgw0==0|tgw0==.)&(tgw2==0|tgw2==.)&(tgw3==0|tgw3==.)&(tgw4==0|tgw4==.)&(tgw5==0|tgw5==.))|((tgw0==1|tgw0==.)&(tgw2==1|tgw2==.)&(tgw3==1|tgw3==.)&(tgw4==1|tgw4==.)&(tgw5==1 | tgw5==.))
replace fluid=. if tgw0==.&tgw2==.&tgw3==.&tgw4==.&tgw5==.

tab fluid, missing
tab tgw0 fluid if tgw0!=., col missing chi2

gen v0_nomiss=1 if tgw0!=.
gen v2_nomiss=1 if tgw2!=.
gen v3_nomiss=1 if tgw3!=.
gen v4_nomiss=1 if tgw4!=.
gen v5_nomiss=1 if tgw5!=.

egen visitsum=rowtotal(v0_nomiss v2_nomiss v3_nomiss v4_nomiss v5_nomiss)
egen tgwsum=rowtotal(tgw0 tgw2 tgw3 tgw4 tgw5)
*/

/*
*hormone use/type
keep if visitnumber=="S"
keep if tgw==1
merge 1:m subjectnumber using RV348b_cm.dta
keep if _merge==3

gen horm1=1 if cd_cmatc3=="ANTIANDROGENS"
gen horm2=1 if cd_cmatc3=="ESTROGENS"
gen horm3=1 if cd_cmatc3=="PROGESTOGENS"
gen horm4=1 if cd_cmatc3=="PROGESTOGENS AND ESTROGENS IN COMBINATION"
gen horm5=1 if cd_cmatc3=="HORMONAL CONTRACEPTIVES FOR SYSTEMIC USE"|cd_cmatc3=="OTHER SEX HORMONES AND MODULATORS OF THE GENITAL SYSTEM"

bysort subjectnumber: gen n=_n
gsort subjectnumber -n
bysort subjectnumber: carryforward horm1 horm2 horm3 horm4 horm5, replace

keep if n==1

replace horm4=1 if horm2==1&horm3==1
replace horm2=. if horm4==1
replace horm3=. if horm4==1

gen horm1c="1" if horm1==1
gen horm2c="2" if horm2==1
gen horm3c="3" if horm3==1
gen horm4c="4" if horm4==1
gen horm5c="5" if horm5==1

gen horm_concat=horm1c+horm2c+horm3c+horm4c+horm5c
tab horm_concat
*/

*destring numeric variables saved as character
destring(risk_b4a_female_age), replace
destring(risk_b4b_male_age), replace
destring(risk_b4c_tran_age), replace
destring(risk_b6), replace

*replace missing with zero
replace risk_a2r1=0 if risk_a2r1==.
replace risk_a2r2=0 if risk_a2r2==.
replace risk_a2r3=0 if risk_a2r3==.
replace risk_a2r4=0 if risk_a2r4==.
replace risk_a2r5=0 if risk_a2r5==.
replace risk_a2r7=0 if risk_a2r7==.
replace risk_a2r8=0 if risk_a2r8==.
replace risk_b3r1=0 if risk_b3r1==.
replace risk_b3r2=0 if risk_b3r2==.
replace risk_b3r3=0 if risk_b3r3==.
replace risk_b5r1=0 if risk_b5r1==.
replace risk_b5r2=0 if risk_b5r2==.
replace risk_b5r3=0 if risk_b5r3==.
replace risk_b7r1=0 if risk_b7r1==.
replace risk_b7r2=0 if risk_b7r2==. 
replace risk_b7r3=0 if risk_b7r3==.
replace risk_b7r4=0 if risk_b7r4==.
replace risk_b7r5=0 if risk_b7r5==.
replace risk_b7r6=0 if risk_b7r6==. 
replace risk_b8r1=0 if risk_b8r1==.
replace risk_b8r2=0 if risk_b8r2==.
replace risk_b8r3=0 if risk_b8r3==.
replace risk_b8r4=0 if risk_b8r4==.
replace risk_b8r5=0 if risk_b8r5==.
replace risk_b8r6=0 if risk_b8r6==.
replace risk_b29r1=0 if risk_b29r1==.
replace risk_b29r2=0 if risk_b29r2==.
replace risk_b29r3=0 if risk_b29r3==.
replace risk_c3ar1=0 if risk_c3ar1==.
replace risk_c3ar2=0 if risk_c3ar2==.
replace risk_c3ar3=0 if risk_c3ar3==.
replace risk_c3ar4=0 if risk_c3ar4==.
replace risk_c5r1=0 if risk_c5r1==.
replace risk_c5r2=0 if risk_c5r2==.
replace risk_c5r3=0 if risk_c5r3==.
replace risk_c5r4=0 if risk_c5r4==.
replace risk_c5r5=0 if risk_c5r5==.

*code skip pattern missingness
replace risk_a2r1=. if tgw!=1
replace risk_a2r2=. if tgw!=1
replace risk_a2r3=. if tgw!=1
replace risk_a2r4=. if tgw!=1
replace risk_a2r5=. if tgw!=1
replace risk_a2r7=. if tgw!=1
replace risk_a2r8=. if tgw!=1
replace risk_a2b=. if risk_a2r3!=1
replace risk_a2c=. if risk_a2r3!=1
replace risk_a2d=. if risk_a2r3!=1
replace risk_b13=. if risk_b12!=1
replace risk_b14r1=. if risk_b14==0
replace risk_b14r2=. if risk_b14==0
replace risk_b14r3=. if risk_b14==0
replace risk_b14r4=. if risk_b14==0
replace risk_b14r5=. if risk_b14==0
replace risk_b14r6=. if risk_b14==0
replace risk_b14r7=. if risk_b14==0
replace risk_b14r8=. if risk_b14==0
replace risk_b14r8=. if risk_b14==0
replace risk_b15a=. if risk_b14r1!=1
replace risk_b16a=. if risk_b14r1!=1
replace risk_b15b=. if risk_b14r2!=1
replace risk_b16b=. if risk_b14r2!=1
replace risk_b15c=. if risk_b14r3!=1
replace risk_b16c=. if risk_b14r3!=1
replace risk_b15d=. if risk_b14r4!=1
replace risk_b16d=. if risk_b14r4!=1
replace risk_b15e=. if risk_b14r5!=1
replace risk_b16e=. if risk_b14r5!=1
replace risk_b15f=. if risk_b14r6!=1
replace risk_b16f=. if risk_b14r6!=1
replace risk_b15g=. if risk_b14r7!=1
replace risk_b16g=. if risk_b14r7!=1
replace risk_b15h=. if risk_b14r8!=1
replace risk_b16h=. if risk_b14r8!=1
replace risk_b15i=. if risk_b14r9!=1
replace risk_b16i=. if risk_b14r9!=1
replace risk_b19ar1=. if risk_b19==0
replace risk_b19ar2=. if risk_b19==0
replace risk_b19ar3=. if risk_b19==0
replace risk_b19ar4=. if risk_b19==0
replace risk_b19ar5=. if risk_b19==0
replace risk_b19ar6=. if risk_b19==0
replace risk_b19ar7=. if risk_b19==0
replace risk_b19ar8=. if risk_b19==0
replace risk_b19ar9=. if risk_b19==0
replace risk_b20a=. if risk_b20!=1
replace risk_b21a=. if risk_b19ar1!=1
replace risk_b22a=. if risk_b19ar1!=1
replace risk_b21b=. if risk_b19ar2!=1
replace risk_b22b=. if risk_b19ar2!=1
replace risk_b21c=. if risk_b19ar3!=1
replace risk_b22c=. if risk_b19ar3!=1
replace risk_b21d=. if risk_b19ar4!=1
replace risk_b22d=. if risk_b19ar4!=1
replace risk_b21e=. if risk_b19ar5!=1
replace risk_b22e=. if risk_b19ar5!=1
replace risk_b21f=. if risk_b19ar6!=1
replace risk_b22f=. if risk_b19ar6!=1
replace risk_b21g=. if risk_b19ar7!=1
replace risk_b22g=. if risk_b19ar7!=1
replace risk_b21h=. if risk_b19ar8!=1
replace risk_b22h=. if risk_b19ar8!=1
replace risk_b21i=. if risk_b19ar9!=1
replace risk_b22i=. if risk_b19ar9!=1
replace risk_b24a=. if risk_b23r1!=1
replace risk_b25a=. if risk_b23r1!=1
replace risk_b24b=. if risk_b23r2!=1
replace risk_b25b=. if risk_b23r2!=1
replace risk_b24c=. if risk_b23r3!=1
replace risk_b25c=. if risk_b23r3!=1
replace risk_b24d=. if risk_b23r4!=1
replace risk_b25d=. if risk_b23r4!=1
replace risk_b24e=. if risk_b23r5!=1
replace risk_b25e=. if risk_b23r5!=1
replace risk_b24f=. if risk_b23r6!=1
replace risk_b25f=. if risk_b23r6!=1
replace risk_b24g=. if risk_b23r7!=1
replace risk_b25g=. if risk_b23r7!=1
replace risk_b24h=. if risk_b23r8!=1
replace risk_b25h=. if risk_b23r8!=1
replace risk_b24i=. if risk_b23r9!=1
replace risk_b25i=. if risk_b23r9!=1
replace risk_b27=. if risk_b26!=1
replace risk_b29r1=. if risk_b28!=1
replace risk_b29r2=. if risk_b28!=1
replace risk_b29r3=. if risk_b28!=1
replace risk_b29r4=. if risk_b28!=1
replace risk_b29r5=. if risk_b28!=1
replace risk_c8a=. if risk_c8!=1
replace risk_c8b=. if risk_c8!=1
replace risk_c8c=. if risk_c8!=1
replace risk_c8d=. if risk_c8!=1
replace risk_d2=. if risk_d1!=1
replace risk_d2a=. if risk_d1!=1
replace risk_d2b=. if risk_d1!=1
replace risk_d2c=. if risk_d1!=1
replace risk_d2a=. if risk_d2!=1
replace risk_d2b=. if risk_d2!=1
replace risk_d2c=. if risk_d2!=1
replace risk_d3a=. if risk_d3!=1
replace risk_d3b=. if risk_d3!=1
replace risk_d3b=. if risk_d3a!=1
replace risk_d4a=. if risk_d4!=1
replace risk_d4b=. if risk_d4!=1
replace risk_d4b=. if risk_d4a!=1
replace risk_d5a=. if risk_d5!=1
replace risk_d5b=. if risk_d5!=1
replace risk_d5c=. if risk_d5!=1
replace risk_d5b=. if risk_d5a!=1
replace risk_d5c=. if risk_d5a!=1
replace risk_d6a=. if risk_d6!=1
replace risk_d6b=. if risk_d6!=1
replace risk_d6b=. if risk_d6a!=1
replace risk_d7a=. if risk_d7!=1
replace risk_d7b=. if risk_d7!=1
replace risk_d7b=. if risk_d7a!=1

*labels
gen age=(cd_visdat-cd_brthdat)/365.25

label def risk_a1 1 "male" 2 "female"
label val risk_a1 risk_a1

label def yn 1 "yes" 0 "no"
label val risk_a2r1 yn
label val risk_a2r2 yn
label val risk_a2r3 yn
label val risk_a2r4 yn
label val risk_a2r5 yn
/* label val risk_a2r6 yn */
label val risk_a2r7 yn
label val risk_a2r8 yn

label def risk_a2b 1 "one month or less" 2 "1-3 months" 3 "3-6 months" 4 "6 months - 1 year" 5 "1 year - 18 months" 6 "2+ years"
label val risk_a2b risk_a2b

label def risk_a2c 1 "pharmacist" 2 "doctor or nurse" 3 "private clinic" 4 "friends" 5 "online"
label val risk_a2c risk_a2c

label def risk_a2d 1 "pharmacy" 2 "clinic" 3 "friends" 4 "online"
label val risk_a2d risk_a2d

label def risk_b1 1 "attracted mostly or only to men" 2 "attracted to men and women" 3 "attracted to mostly or only to women" 8 "refuse to answer" 9 "don't know"
label val risk_b1 risk_b1

label def risk_b2 1 "heterosexual" 2 "bisexual" 3 "homosexual" 4 "TGW" 8 "refuse to answer" 9 "don't know"
label val risk_b2 risk_b2

label val risk_b3r1 yn
label val risk_b3r2 yn
label val risk_b3r3 yn
label val risk_b5r1 yn
label val risk_b5r2 yn
label val risk_b5r3 yn
label val risk_b7r1 yn
label val risk_b7r2 yn
label val risk_b7r3 yn
label val risk_b7r4 yn
label val risk_b7r5 yn
label val risk_b7r6 yn
label val risk_b8r1 yn
label val risk_b8r2 yn
label val risk_b8r3 yn
label val risk_b8r4 yn
label val risk_b8r5 yn
label val risk_b8r6 yn
label val risk_b9 yn
label val risk_b10 yn
label val risk_b11 yn
label val risk_b12 yn

label def condomfreq 1 "always" 2 "nearly always" 3 "sometimes" 4 "rarely" 5 "never" 8 "refuse to answer" 9 "don't know"
label val risk_b13 condomfreq

label val risk_b14 yn
label val risk_b14r1 yn
label val risk_b14r2 yn
label val risk_b14r3 yn
label val risk_b14r4 yn
label val risk_b14r5 yn
label val risk_b14r6 yn
label val risk_b14r7 yn
label val risk_b14r8 yn
label val risk_b14r9 yn
label val risk_b15a condomfreq
label val risk_b15b condomfreq
label val risk_b15c condomfreq
label val risk_b15d condomfreq
label val risk_b15e condomfreq
label val risk_b15f condomfreq
label val risk_b15g condomfreq
label val risk_b15h condomfreq
label val risk_b15i condomfreq
label val risk_b16a yn
label val risk_b16b yn
label val risk_b16c yn
label val risk_b16d yn
label val risk_b16e yn
label val risk_b16f yn
label val risk_b16g yn
label val risk_b16h yn
label val risk_b16i yn

label val risk_b18 yn
label val risk_b19 yn
label val risk_b19ar1 yn
label val risk_b19ar2 yn
label val risk_b19ar3 yn
label val risk_b19ar4 yn
label val risk_b19ar5 yn
label val risk_b19ar6 yn
label val risk_b19ar7 yn
label val risk_b19ar8 yn
label val risk_b19ar9 yn
label val risk_b20 yn

label def risk_b20a 1 "partner was HIV-" 2 "partner was HIV+" 3 "did not know status of partner"
label val risk_b20a risk_b20a

label val risk_b21a condomfreq
label val risk_b21b condomfreq
label val risk_b21c condomfreq
label val risk_b21d condomfreq
label val risk_b21e condomfreq
label val risk_b21f condomfreq
label val risk_b21g condomfreq
label val risk_b21h condomfreq
label val risk_b21i condomfreq
label val risk_b22a yn
label val risk_b22b yn
label val risk_b22c yn
label val risk_b22d yn
label val risk_b22e yn
label val risk_b22f yn
label val risk_b22g yn
label val risk_b22h yn
label val risk_b22i yn

label val risk_b21a condomfreq
label val risk_b21b condomfreq
label val risk_b21c condomfreq
label val risk_b21d condomfreq
label val risk_b21e condomfreq
label val risk_b21f condomfreq
label val risk_b21g condomfreq
label val risk_b21h condomfreq
label val risk_b21i condomfreq
label val risk_b22a yn
label val risk_b22b yn
label val risk_b22c yn
label val risk_b22d yn
label val risk_b22e yn
label val risk_b22f yn
label val risk_b22g yn
label val risk_b22h yn
label val risk_b22i yn

label val risk_b23r1 yn
label val risk_b23r2 yn
label val risk_b23r3 yn
label val risk_b23r4 yn
label val risk_b23r5 yn
label val risk_b23r6 yn
label val risk_b23r7 yn
label val risk_b23r8 yn
label val risk_b23r9 yn

label val risk_b24a condomfreq
label val risk_b24b condomfreq
label val risk_b24c condomfreq
label val risk_b24d condomfreq
label val risk_b24e condomfreq
label val risk_b24f condomfreq
label val risk_b24g condomfreq
label val risk_b24h condomfreq
label val risk_b24i condomfreq
label val risk_b25a yn
label val risk_b25b yn
label val risk_b25c yn
label val risk_b25d yn
label val risk_b25e yn
label val risk_b25f yn
label val risk_b25g yn
label val risk_b25h yn
label val risk_b25i yn

*recode 8=refuse, 9=don't know, missing
// replace risk_b26=0 if risk_b26==8|risk_b26==9|risk_b26==.
// replace risk_b27=5 if risk_b27==8|risk_b27==9|risk_b27==.
//
// gen lube_freq=5 if risk_b26==0
// replace lube_freq=risk_b27 if risk_b26==1
// label val lube_freq condomfreq

label def ynrdk 1 "yes" 0 "no" 8 "refuse to answer" 9 "don't know"
label val risk_b26 ynrdk
label val risk_b27 condomfreq

label def risk_b28 0 "no" 1 "yes" 2 "never heard of female condoms" 8 "refuse to answer" 9 "don't know"
label val risk_b28 risk_b28

label val risk_b29r1 yn
label val risk_b29r2 yn
label val risk_b29r3 yn

label val risk_c1 yn
label val risk_c2 yn
label val risk_c3 yn
label val risk_c3ar1 yn
label val risk_c3ar2 yn
label val risk_c3ar3 yn
label val risk_c3ar4 yn

label def risk_c3b 1 "within the past 3 months" 2 "within the past 6 months" 3 "within the past year" 4 "over a year ago"
label val risk_c3b risk_c3b

label def risk_c4 1 "no" 2 "yes, every 1-3 months" 3 "yes, every 6-11 months" 4 "yes, every year"
label val risk_c4 risk_c4

label val risk_c5r1 yn
label val risk_c5r2 yn
label val risk_c5r3 yn
label val risk_c5r4 yn
label val risk_c5r5 yn
label val risk_c5r6 yn
label val risk_c5r7 yn
label val risk_c5r8 yn

label val risk_c6 ynrdk
label val risk_c7 yn
label val risk_c8 yn

label def risk_c8a 1 "clinic by doctor or nurse" 2 "pharmacy" 3 "at my own home" 4 "at friend's house" 5 "other"
label val risk_c8a risk_c8a

label def risk_c8b 1 "always" 2 "nearly always" 3 "sometimes" 4 "nearly never" 5 "never" 8 "refuse to answer" 9 "don't know"
label val risk_c8b risk_c8b

label def risk_c8c 1 "every week" 2 "every 2 weeks" 3 "once a month" 4 "once every few months" 5 "once a year"
label val risk_c8c risk_c8c

label def risk_c8d 1 "in the past week" 2 "in the past month" 3 "in the past 6 months" 4 "in the past year"
label val risk_c8d risk_c8d

label def risk_c9 0 "no" 1 "yes" 8 "refuse to answer" 9 "don't know"
label val risk_c9 risk_c9

label val risk_d1 ynrdk
label val risk_d2 ynrdk

label def freqdrink 1 "once a month or less" 2 "2-4 times a month" 3 "a few times a week" 4 "daily/almost every day" 8 "refuse to answer" 9 "don't know"
label val risk_d2a freqdrink
label val risk_d2b freqdrink

label def concerndrink 1 "yes, in last year" 2 "yes, but not in last year" 3 "no"
label val risk_d2c concerndrink

label val risk_d3 ynrdk
label val risk_d3a ynrdk
label val risk_d3b freqdrink
label val risk_d4 ynrdk
label val risk_d4a ynrdk
label val risk_d4b freqdrink
label val risk_d5 ynrdk
label val risk_d5a ynrdk
label val risk_d5b freqdrink
label val risk_d5c risk_c8b
label val risk_d6 ynrdk
label val risk_d6a ynrdk
label val risk_d6b freqdrink
label val risk_d7 ynrdk
label val risk_d7a ynrdk
label val risk_d7b freqdrink
label val risk_d8 condomfreq
label val risk_d9 condomfreq

*condom use at last sex by partner type
replace risk_b14r1=0 if risk_b14r1==.
replace risk_b14r2=0 if risk_b14r2==.
replace risk_b14r3=0 if risk_b14r3==.
replace risk_b14r4=0 if risk_b14r4==.
replace risk_b14r5=0 if risk_b14r5==.
replace risk_b14r6=0 if risk_b14r6==.
replace risk_b14r7=0 if risk_b14r7==.
replace risk_b14r8=0 if risk_b14r8==.
replace risk_b14r9=0 if risk_b14r9==.

replace risk_b19ar1=0 if risk_b19ar1==.
replace risk_b19ar2=0 if risk_b19ar2==.
replace risk_b19ar3=0 if risk_b19ar3==.
replace risk_b19ar4=0 if risk_b19ar4==.
replace risk_b19ar5=0 if risk_b19ar5==.
replace risk_b19ar6=0 if risk_b19ar6==.
replace risk_b19ar7=0 if risk_b19ar7==.
replace risk_b19ar8=0 if risk_b19ar8==.
replace risk_b19ar9=0 if risk_b19ar9==.

replace risk_b23r1=0 if risk_b23r1==.
replace risk_b23r2=0 if risk_b23r2==.
replace risk_b23r3=0 if risk_b23r3==.
replace risk_b23r4=0 if risk_b23r4==.
replace risk_b23r5=0 if risk_b23r5==.
replace risk_b23r6=0 if risk_b23r6==.
replace risk_b23r7=0 if risk_b23r7==.
replace risk_b23r8=0 if risk_b23r8==.
replace risk_b23r9=0 if risk_b23r9==.

*condom use per partner type
egen numpart_vaginal = rowtotal(risk_b14r1 risk_b14r2 risk_b14r3 risk_b14r4 risk_b14r5 risk_b14r6 risk_b14r7 risk_b14r8 risk_b14r9), missing
egen numpart_vaginal_condoms = rowtotal(risk_b16a risk_b16b risk_b16c risk_b16d risk_b16e risk_b16f risk_b16g risk_b16h risk_b16i), missing
gen prop_vaginal_condoms = numpart_vaginal_condoms/numpart_vaginal

gen condom_vaginal=0 if numpart_vaginal==0
replace condom_vaginal=1 if prop_vaginal_condoms==1
replace condom_vaginal=2 if prop_vaginal_condoms>0 & prop_vaginal_condoms<1
replace condom_vaginal=3 if prop_vaginal_condoms==0

egen numpart_insertive = rowtotal(risk_b19ar1 risk_b19ar2 risk_b19ar3 risk_b19ar4 risk_b19ar5 risk_b19ar6 risk_b19ar7 risk_b19ar8 risk_b19ar9), missing
egen numpart_insertive_condoms = rowtotal(risk_b22a risk_b22b risk_b22c risk_b22d risk_b22e risk_b22f risk_b22g risk_b22h risk_b22i), missing
gen prop_insertive_condoms = numpart_insertive_condoms/numpart_insertive

gen condom_insertive=0 if numpart_insertive==0
replace condom_insertive=1 if prop_insertive_condoms==1
replace condom_insertive=2 if prop_insertive_condoms>0 & prop_insertive_condoms<1
replace condom_insertive=3 if prop_insertive_condoms==0

egen numpart_receptive = rowtotal(risk_b23r1 risk_b23r2 risk_b23r3 risk_b23r4 risk_b23r5 risk_b23r6 risk_b23r7 risk_b23r8 risk_b23r9), missing
egen numpart_receptive_condoms = rowtotal(risk_b25a risk_b25b risk_b25c risk_b25d risk_b25e risk_b25f risk_b25g risk_b25h risk_b25i), missing
gen prop_receptive_condoms = numpart_receptive_condoms/numpart_receptive

gen condom_receptive=0 if numpart_receptive==0
replace condom_receptive=1 if prop_receptive_condoms==1
replace condom_receptive=2 if prop_receptive_condoms>0 & prop_receptive_condoms<1
replace condom_receptive=3 if prop_receptive_condoms==0

label define condom 0 "no sex of this type with any partners" 1 "used condom at last sex with all partners" /*
*/ 2 "used condoms at last sex with some partners" 3 "did not use condoms at last sex with any partners"
label val condom_vaginal condom
label val condom_insertive condom
label val condom_receptive condom

/*Table 3: retention
use RV348_merged_data_24FEB20.dta, clear

*drop visits that were not completed
drop if cd_visdat_nd=="Not Applicable"|cd_visdat_nd=="Not Done"

gen enrolled=1 if cd_ieyn_c=="Y"
gsort subjectnumber cd_visdat
bysort subjectnumber: carryforward enrolled, replace
gsort subjectnumber -cd_visdat
bysort subjectnumber: carryforward enrolled, replace
keep if enrolled==1

*define TGW at screening: answered A1 with “1-male” AND answered A2 with “2-female,” or answered A2 with “3-transgender woman”
gsort subjectnumber cd_visdat
gen tgw=0 if cd_sex_c=="M" & risk_a2==1
replace tgw=1 if cd_sex_c=="M" & (risk_a2==2|risk_a2==3)
replace tgw=. if visitnumber!="S"
bysort subjectnumber: carryforward tgw, replace
label def tgw 0 "cisgender MSM" 1 "transgender MSM"
label val tgw tgw
label var tgw "Gender identity"

*number retained at each visit
tab sitename visitnumber
tab tgw visitnumber
bysort sitename: tab tgw visitnumber

// *seroconversion at unscheduled visit: If someone seroconverted at an unscheduled visit between visits 2 and 3, then visit 2 would have been their last expected study visit. They should be removed from the denominator of expected study visits for visits 3 onward.
// keep subjectnumber visitnumber cd_visdat sitename tgw cd_hivfinal_lborres_c enrolled
// keep if enrolled==1 & cd_hivfinal_lborres_c=="Positive/Reactive" & visitnumber=="Unscheduled/Addition"
// keep if subjectnumber=="1043"|subjectnumber=="2131"|subjectnumber=="1347"|subjectnumber=="2324"|subjectnumber=="2493"

*1043 last expected study visit=V2 (VTC/TGW)
*1347 last expected study visit=V4 (VTC/TGW)
*2131 last expected study visit=V5 (RTA/TGW)
*2324 last expected study visit=V4 (RTA/cisgender MSM)
*2493 last expected study visit=V3 (RTA/cisgender MSM)

tab sitename visitnumber if cd_hivfinal_lborres_c=="Positive/Reactive"
tab tgw visitnumber if cd_hivfinal_lborres_c=="Positive/Reactive"
bysort sitename: tab tgw visitnumber if cd_hivfinal_lborres_c=="Positive/Reactive"
*/

*Categorization for table 1
gen agec=0 if age>=18 & age<=20
replace agec=1 if age>20 & age<=23
replace agec=2 if age>23 & age!=.
label def agec 0 "18-20" 1 "21-23" 2 ">23"
label val agec agec
label var agec "Age"

gen sexattract=1 if risk_b1==1
replace sexattract=2 if risk_b1==2
replace sexattract=3 if risk_b1==3
replace sexattract=4 if risk_b1==8|risk_b1==9|risk_b1==.
label def sexattract 1 "men only" 2 "both men and women" 3 "women only" 4 "unknown/missing"
label val sexattract sexattract

gen marital=1 if strpos(cd_marital_c,"01")
replace marital=2 if strpos(cd_marital_c,"02")
replace marital=3 if strpos(cd_marital_c,"03")
replace marital=4 if strpos(cd_marital_c,"04")
label def marital 1 "Never married or cohabited" 2 "Married" 3 "Cohabitating (living with partner)" 4 "Ever married (separated, divorced, widowed)"
label val marital marital
label var marital "Marital Status"

gen educat=0 if strpos(cd_edlevel,"No formal education")|strpos(cd_edlevel,"Primary")
replace educat=1 if strpos(cd_edlevel,"secondary")|strpos(cd_edlevel,"Secondary")
replace educat=2 if strpos(cd_edlevel,"Vocational")
replace educat=3 if strpos(cd_edlevel,"Some university")
replace educat=4 if strpos(cd_edlevel,"Bachelor's Degree")|strpos(cd_edlevel,"degree beyond bachelor’s")
label def educat 0 "none/some primary" 1 "secondary" 2 "vocational" 3 "some university" 4 "completed university/more"
label val educat educat

gen income=0 if risk_a6==.
replace income=1 if risk_a6==1
replace income=2 if risk_a6>=2&risk_a6!=.
label def income 0 "unknown/missing" 1 "<15,000 Baht" 2 ">=15,000 Baht"
label val income income

gen reside=0 if risk_a4==0|risk_a4==1
replace reside=1 if risk_a4==2
replace reside=2 if risk_a4==3|risk_a4==4
replace reside=3 if risk_a4==8|risk_a4==9|risk_a4==.
label def reside 0 "<1 year" 1 "1-2 years" 2 ">2 years" 3 "unknown/missing"
label val reside reside

gen ever_idu=0 if risk_d5==0
replace ever_idu=1 if risk_d5==1
replace ever_idu=2 if risk_d5==8|risk_d5==9|risk_d5==.
label def ever_idu 0 "no" 1 "yes" 2 "unknown/missing"
label val ever_idu ever_idu

gen ever_meth=0 if risk_d7==0
replace ever_meth=1 if risk_d7==1
replace ever_meth=2 if risk_d7==8|risk_d7==9|risk_d7==.
label def ever_meth 0 "no" 1 "yes" 2 "unknown/missing"
label val ever_meth ever_meth

gen site_n=0 if sitename=="RTA"
replace site_n=1 if sitename=="VTC"
label def site_n 0 "RTA" 1 "VTC"
label val site_n site_n

gen sexpartner=1 if risk_b6>=1 & risk_b6<4
replace sexpartner=2 if risk_b6>=4 & risk_b6<8
replace sexpartner=3 if risk_b6>=8 & risk_b6!=.
replace sexpartner=4 if risk_b6==0|risk_b6==.
label def sexpartner 1 "1-3" 2 "4-7" 3 "8 or more" 4 "missing"
label val sexpartner sexpartner

label val risk_b18 yn
label var risk_b18 "sex work in past 6 months"

label val risk_b9 yn
label var risk_b9 "forced sex"

gen hivtest=0 if risk_c1==0
replace hivtest=1 if risk_c1==1
replace hivtest=2 if risk_c1==.
label def hivtest 0 "no" 1 "yes" 2 "missing"
label val hivtest hivtest

*Table 1
table1 if visitnumber=="S" & tgw!=., by(tgw) vars(agec cate\sexattract cate\educat cate\risk_a6 cate\income cate\reside cate\ever_idu cate\ever_meth cate\sitename cate\sexpartner cate\risk_b18 cate\risk_b9 cate\hivtest cate) onecol sav("C:\Users\ndear\Box Sync\NDear\RV348\TGW\table1.xlsx")

*Figure 1
table1 if visitnumber=="S" & tgw!=., by(tgw)  vars(risk_b7r1 cat\risk_b7r2 cat\risk_b7r3 cat\risk_b7r4 cat\risk_b7r5 cat\risk_b7r6 cat) onecol missing sav("fig1.xlsx")

*Categorization for tables 2 and 3
gen agec=0 if age>=18 & age<=20
replace agec=1 if age>20 & age<=23
replace agec=2 if age>23 & age!=.
label def agec 0 "18-20" 1 "21-23" 2 ">23"
label val agec agec
label var agec "Age"

gen sexattract=1 if risk_b1==1
replace sexattract=2 if risk_b1==2
replace sexattract=3 if risk_b1==3|risk_b1==8|risk_b1==9|risk_b1==.
label def sexattract 1 "men only" 2 "both men and women" 3 "women only/unknown"
label val sexattract sexattract

gen educat=0 if strpos(cd_edlevel,"No formal education")|strpos(cd_edlevel,"Primary")
replace educat=1 if strpos(cd_edlevel,"secondary")|strpos(cd_edlevel,"Secondary")
replace educat=2 if strpos(cd_edlevel,"Vocational")
replace educat=3 if strpos(cd_edlevel,"Some university")
replace educat=4 if strpos(cd_edlevel,"Bachelor's Degree")|strpos(cd_edlevel,"degree beyond bachelor’s")
label def educat 0 "none/some primary" 1 "secondary" 2 "vocational" 3 "some university" 4 "completed university/more"
label val educat educat

gen income=0 if risk_a6==.|risk_a6==1
replace income=1 if risk_a6>=2&risk_a6!=.
label def income 0 "<15,000 Baht/unknown" 1 ">=15,000 Baht"
label val income income

gen reside=0 if risk_a4==0|risk_a4==1
replace reside=1 if risk_a4==2
replace reside=2 if risk_a4==3|risk_a4==4|risk_a4==8|risk_a4==9|risk_a4==.
label def reside 0 "<1 year" 1 "1-2 years" 2 ">2 years/unknown"
label val reside reside

gen ever_idu=0 if risk_d5==0|risk_d5==8|risk_d5==9|risk_d5==.
replace ever_idu=1 if risk_d5==1
label def ever_idu 0 "no/unknown" 1 "yes"
label val ever_idu ever_idu

gen ever_meth=0 if risk_d7==0|risk_d7==8|risk_d7==9|risk_d7==.
replace ever_meth=1 if risk_d7==1
label def ever_meth 0 "no/unknown" 1 "yes"
label val ever_meth ever_meth

gen site_n=0 if sitename=="RTA"
replace site_n=1 if sitename=="VTC"
label def site_n 0 "RTA" 1 "VTC"
label val site_n site_n

gen sexpartner=1 if risk_b6>=1 & risk_b6<4
replace sexpartner=2 if risk_b6>=4 & risk_b6<8
replace sexpartner=3 if risk_b6>=8|risk_b6==0|risk_b6==.
label def sexpartner 1 "1-3" 2 "4-7" 3 "8 or more/missing"
label val sexpartner sexpartner

gen sexwork=0 if risk_b18==0|risk_b18==.
replace sexwork=1 if risk_b18==1
label val sexwork yn
label var sexwork "sex work in past 6 months"

gen forcedsex=0 if risk_b9==0|risk_b9==.
replace forcedsex=1 if risk_b9==1
label val forcedsex yn
label var forcedsex "forced sex in past 6 months"

gen hivtest=0 if risk_c1==0|risk_c1==.
replace hivtest=1 if risk_c1==1
label def hivtest 0 "no/missing" 1 "yes"
label val hivtest hivtest

*Table 2
keep if visitnumber=="S" & tgw!=.
glm tgw ib2.agec, family(poisson) link(log) nolog robust eform
glm tgw ib3.sexattract, family(poisson) link(log) nolog robust eform
glm tgw ib0.educat, family(poisson) link(log) nolog robust eform
glm tgw i.income, family(poisson) link(log) nolog robust eform
glm tgw ib2.reside, family(poisson) link(log) nolog robust eform
glm tgw ib0.ever_idu, family(poisson) link(log) nolog robust eform
glm tgw ib0.ever_meth, family(poisson) link(log) nolog robust eform
glm tgw i.site_n, family(poisson) link(log) nolog robust eform
glm tgw i.sexpartner, family(poisson) link(log) nolog robust eform
glm tgw i.sexwork, family(poisson) link(log) nolog robust eform
glm tgw i.forcedsex, family(poisson) link(log) nolog robust eform
glm tgw i.hivtest, family(poisson) link(log) nolog robust eform

glm tgw ib2.agec ib3.sexattract ib0.educat i.income ib2.reside ib0.ever_idu ib0.ever_meth i.site_n i.sexpartner i.sexwork i.forcedsex i.hivtest, family(poisson) link(log) nolog robust eform

**check for multicollinearity
*run collin.ado from C:\Users\ndear\Box Sync\NDear\RV432\stata code\collin
*The condition number is a commonly used index of the global instability of the regression coefficients — a large condition number, 10 or more, is an indication of instability
collin agec sexattract educat income reside ever_idu ever_meth site_n sexpartner sexwork forcedsex hivtest

*Table 3
keep if tgw==1
drop if visitnumber=="Unscheduled/Addition"|visitnumber=="V1"
egen pin = group(subjectnumber), label

*taking hormones
xtgee risk_a2r3 ib2.agec , i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee risk_a2r3 ib3.sexattract , i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee risk_a2r3 ib0.educat , i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee risk_a2r3 i.income , i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee risk_a2r3 ib2.reside , i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee risk_a2r3 ib0.ever_idu , i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee risk_a2r3 ib0.ever_meth , i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee risk_a2r3 i.site_n , i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee risk_a2r3 i.sexpartner , i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee risk_a2r3 i.sexwork , i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee risk_a2r3 i.forcedsex , i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee risk_a2r3 i.hivtest , i(pin) family(bin) link(logit) corr(exch) robust ef nolog

xtgee risk_a2r3 ib2.agec ib3.sexattract ib0.educat i.income ib2.reside ib0.ever_idu ib0.ever_meth i.site_n i.sexpartner i.sexwork i.forcedsex i.hivtest, i(pin) family(bin) link(logit) corr(exch) robust ef nolog

*gender affirmation surgery
xtgee risk_a2r5 ib2.agec , i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee risk_a2r5 ib3.sexattract , i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee risk_a2r5 ib0.educat , i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee risk_a2r5 i.income , i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee risk_a2r5 ib2.reside , i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee risk_a2r5 ib0.ever_idu , i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee risk_a2r5 ib0.ever_meth , i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee risk_a2r5 i.site_n , i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee risk_a2r5 i.sexpartner , i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee risk_a2r5 i.sexwork , i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee risk_a2r5 i.forcedsex , i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee risk_a2r5 i.hivtest , i(pin) family(bin) link(logit) corr(exch) robust ef nolog

xtgee risk_a2r5 ib2.agec ib3.sexattract ib0.educat i.income ib2.reside ib0.ever_idu ib0.ever_meth i.site_n i.sexpartner i.sexwork i.forcedsex i.hivtest, i(pin) family(bin) link(logit) corr(exch) robust ef nolog

collin agec sexattract educat income reside ever_idu ever_meth site_n sexpartner sexwork forcedsex hivtest

// xtlogit risk_a2r5 ib2.agec ib3.sexattract ib0.educat i.income ib2.reside ib0.ever_idu ib0.ever_meth i.site_n , i(pin) pa corr(exchangeable)

*Figure 2
keep if visitnumber=="S" & tgw!=.

tab condom_insertive tgw if numpart_insertive!=0, exact col
tab condom_receptive tgw if numpart_receptive!=0, exact col

tab condom_insertive risk_a2r3 if numpart_insertive!=0, exact col
tab condom_receptive risk_a2r3 if numpart_receptive!=0, exact col

tab condom_insertive risk_a2r5 if numpart_insertive!=0, exact col
tab condom_receptive risk_a2r5 if numpart_receptive!=0, exact col

*hormone use
keep if visitnumber=="S" & tgw!=.

tab risk_a2r3 if tgw==1, missing

gen hormonedur=0 if risk_a2b<=5
replace hormonedur=1 if risk_a2b==6
label def hormonedur 0 "<2 years" 1 "2+ years"
label val hormonedur hormonedur

tab hormonedur if tgw==1 & risk_a2r3==1, missing
tab risk_a2c if tgw==1 & risk_a2r3==1, missing
tab risk_a2d if tgw==1 & risk_a2r3==1, missing

*filler use
keep if visitnumber=="S" & tgw!=.

tab tgw risk_c8, row chi2 exact

gen filler_clinic=0 if risk_c8a==3|risk_c8a==4|risk_c8a==5
replace filler_clinic=1 if risk_c8a==1|risk_c8a==2

tab tgw filler_clinic if risk_c8==1, row chi2 exact

tab risk_c8a if tgw==1 & risk_c8==1, missing
tab risk_c8b if tgw==1 & risk_c8==1, missing
tab risk_c8c if tgw==1 & risk_c8==1, missing
tab risk_c8d if tgw==1 & risk_c8==1, missing

tab risk_c8b risk_c8a if tgw==1 & risk_c8==1, missing

gen neverreuse=0 if risk_c8b!=5 & risk_c8b!=.
replace neverreuse=1 if risk_c8b==5
label def neverreuse 0 "no" 1 "yes"
label val neverreuse neverreuse

tab risk_c8 risk_a2r3 if visitnumber=="S", exact col
tab neverreuse risk_a2r3 if visitnumber=="S", exact col
tab risk_c8d risk_a2r3 if visitnumber=="S", exact col

tab hormonedur risk_a2r5 if visitnumber=="S", exact col
tab risk_c8 risk_a2r5 if visitnumber=="S", exact col
tab neverreuse risk_a2r5 if visitnumber=="S", exact col
tab risk_c8d risk_a2r5 if visitnumber=="S", exact col

*prep
label def ynd 0 "No" 1 "Yes" 9 "Don't know"
label val prep_q7 ynd
label val prep_q8 ynd

label def interest 1 "Very interested" 2 "Somewhat interested" 3 "Neutral" 4 "Somewhat uninterested" 5 "Very uninterested" 98 "Don’t know"
label val prep_q11 interest

gen interest=0 if prep_q11==1|prep_q11==2
replace interest=1 if prep_q11==3
replace interest=2 if prep_q11==4|prep_q11==5
replace interest=3 if prep_q11==98

label def interest2 0 "very/somewhat interested" 1 "neutral" 2 "somewhat/very uninterested" 3 "don't know"
label val interest interest2

tab prep_q7 tgw if visitnumber=="S" & tgw!=., col chi2 exact
tab prep_q8 tgw if prep_q7==1 & visitnumber=="S" & tgw!=., col chi2 exact
tab prep_q11 tgw if visitnumber=="S" & tgw!=., col chi2 exact
tab interest tgw if visitnumber=="S" & tgw!=., col chi2 exact

*tgw cascade
gen transition=0 if risk_a2r2==0&risk_a2r3==0&risk_a2r4==0&risk_a2r5==0
replace transition=1 if risk_a2r2==1&risk_a2r3==0&risk_a2r4==0&risk_a2r5==0
replace transition=2 if risk_a2r2==0&risk_a2r3==1&risk_a2r4==0&risk_a2r5==0
replace transition=3 if risk_a2r2==0&risk_a2r3==0&risk_a2r4==1&risk_a2r5==0
replace transition=4 if risk_a2r2==0&risk_a2r3==0&risk_a2r4==0&risk_a2r5==1
replace transition=5 if risk_a2r2==1&risk_a2r3==1&risk_a2r4==0&risk_a2r5==0
replace transition=6 if risk_a2r2==1&risk_a2r3==0&risk_a2r4==1&risk_a2r5==0
replace transition=7 if risk_a2r2==1&risk_a2r3==0&risk_a2r4==0&risk_a2r5==1
replace transition=8 if risk_a2r2==0&risk_a2r3==1&risk_a2r4==1&risk_a2r5==0
replace transition=9 if risk_a2r2==0&risk_a2r3==1&risk_a2r4==0&risk_a2r5==1
replace transition=10 if risk_a2r2==0&risk_a2r3==0&risk_a2r4==1&risk_a2r5==1
replace transition=11 if risk_a2r2==1&risk_a2r3==1&risk_a2r4==1&risk_a2r5==0
replace transition=12 if risk_a2r2==1&risk_a2r3==1&risk_a2r4==0&risk_a2r5==1
replace transition=13 if risk_a2r2==0&risk_a2r3==1&risk_a2r4==1&risk_a2r5==1
replace transition=14 if risk_a2r2==1&risk_a2r3==0&risk_a2r4==1&risk_a2r5==1
replace transition=15 if risk_a2r2==1&risk_a2r3==1&risk_a2r4==1&risk_a2r5==1

label def transition 0 "none" 1 "cross-gender role" 2 "taking hormones" 3 "counseling" 4 "GAS" 5 "cross-gender role/taking hormones" 6 "cross-gender role/counseling" 7 "cross-gender role/GAS" 8 "hormones/counseling" 9 "hormones/GAS" 10 "counseling/GAS" 11 "cross-gender role/hormones/counseling" 12 "cross-gender role/hormones/GAS" 13 "hormones/counseling/GAS" 14 "cross-gender role/counseling/GAS" 15 "all"
label val transition transition

tab transition if visitnumber=="S"

*numbers for venn diagram
gen transition=0 if risk_a2r2==0 & risk_a2r3==0 & risk_a2r5==0
replace transition=1 if risk_a2r2==1 & risk_a2r3==0 & risk_a2r5==0
replace transition=2 if risk_a2r2==0 & risk_a2r3==1 & risk_a2r5==0
replace transition=3 if risk_a2r2==0 & risk_a2r3==0 & risk_a2r5==1
replace transition=4 if risk_a2r2==1 & risk_a2r3==1 & risk_a2r5==0
replace transition=5 if risk_a2r2==1 & risk_a2r3==0 & risk_a2r5==1
replace transition=6 if risk_a2r2==0 & risk_a2r3==1 & risk_a2r5==1
replace transition=7 if risk_a2r2==1 & risk_a2r3==1 & risk_a2r5==1

label def transition 0 "none" 1 "cross-gender role" 2 "taking hormones" 3 "GAS" 4 "cross-gender role/taking hormones" 5 "cross-gender role/GAS" 6 "hormones/GAS" 7 "cross-gender role/hormones/GAS"
label val transition transition

tab transition if visitnumber=="S"


