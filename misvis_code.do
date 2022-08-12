/***********************************************************
Program: Missed visit analysis
Date: 5 May 2020
Author: Nicole Dear
Data in: misvis_1mar2020
Data out:
References: https://www.stata.com/support/faqs/statistics/random-effects-versus-population-averaged/
***********************************************************/
*set working directory and load data
cd "C:\Users\ndear\Box Sync\Shared Files- Reed and Esber\Visit adherence"
sysuse misvis_1jun2021_v2.dta, clear

drop if misvis==1
drop misvis
keep if visitdt<=date("1mar2020","DMY") & visit<90

*recode misdiagnoses
replace hivflag=2 if subjid=="A01-0026"
replace hivflag=2 if subjid=="A01-0159"
replace hivflag=2 if subjid=="A01-0232"
replace hivflag=2 if subjid=="A01-0343"
replace hivflag=2 if subjid=="A01-0402"
replace hivflag=2 if subjid=="A01-0527"
replace hivflag=2 if subjid=="A01-0587"
replace hivflag=2 if subjid=="A01-0603"
replace hivflag=2 if subjid=="B04-0054"

*check for duplicates
duplicates tag subjid visit, gen(dup_id)
tab dup_id

*assign labels
replace progid=1 if strpos(subjid, "A")
replace progid=2 if strpos(subjid, "B")
replace progid=3 if strpos(subjid, "C")
replace progid=4 if strpos(subjid, "D")
replace progid=5 if strpos(subjid, "E")

label var progid "Study site"
label define progid 1 "Kayunga, Uganda" 2 "South Rift Valley, Kenya" 3 "Kisumu West, Kenya" 4 "Mbeya, Tanzania" 5 "Abuja & Lagos Nigeria"
label val progid progid
label var progid "Program site"

label def gender 1 "Male" 2 "Female"
label val gender gender
label var gender "Sex"

*CESD score categories (Reference: Center for Epidemiologic Studies Depression Scale (CES-D))
gen cesdcat=0 if cesd_score<10
replace cesdcat=1 if cesd_score>=10 & cesd_score<16
replace cesdcat=2 if cesd_score>=16 & cesd_score<25
replace cesdcat=3 if cesd_score>=25 & cesd_score!=.
label define cesdcat 0 "not depressed (score 0-9)" 1 "mildly depressed (score 10-15)" 2 "moderately depressed (score 16-24)" 3 "severely depressed (score 25+)"
label val cesdcat cesdcat
label var cesdcat "CESD score"

// *generating BMI categories
// gen bmic=0 if bmi<18.5
// replace bmic=1 if bmi>=18.5 & bmi<25
// replace bmic=2 if bmi>=25 & bmi!=.
// label def bmic 0 "underweight" 1 "normal" 2 "overweight/obese"
// label val bmic bmic

*satisfaction with services, recode missing
replace artsrv_a=. if artsrv_a==8
replace artsrv_b=. if artsrv_b==8
replace artsrv_c=. if artsrv_c==8
replace artsrv_d=. if artsrv_d==8
replace artsrv_e=. if artsrv_e==8
replace artsrv_f=. if artsrv_f==8
replace artsrv_g=. if artsrv_g==8

*recode no reponse
replace alcohol=. if alcohol==8
replace tb=0 if tb==.

*categorizing howfrkm
bysort subjid: carryforward howfrkm, gen(howfar)

gen distance25=0 if howfar<25
replace distance25=1 if howfar>=25 & howfar!=.
label def distance25 0 "<25 km" 1 "25+ km"
label val distance25 distance25 
label var distance25 "Distance from clinic, 25km"

gen distance10=0 if howfar<10
replace distance10=1 if howfar>=10 & howfar!=.
label def distance10 0 "<10 km" 1 "10+ km"
label val distance10 distance10 
label var distance10 "Distance from clinic, 10km"

xtile qdistance2 = howfar, n(4)

gen distancecat=1 if howfar<5
replace distancecat=2 if howfar>=5 & howfar<10
replace distancecat=3 if howfar>=10 & howfar<15
replace distancecat=4 if howfar>=15 & howfar!=.
label def distancecat 1 "<5 km" 2 "5-9 km" 3 "10-14 km" 4 ">=15 km"
label val distancecat distancecat 
label var distancecat "Distance from clinic (KM) categories"

gen timetoclinic = ((howlongh*60) + howlongm)/60
xtile timetoclinic_quart = timetoclinic, n(4)
tab timetoclinic timetoclinic_quart

gen timetoclinic_cat=0 if timetoclinic_quart==1
replace timetoclinic_cat=1 if timetoclinic_quart==2|timetoclinic_quart==3|timetoclinic_quart==4
label def timetoclinic_cat 0 "<=30 mins" 1 ">30 mins"
label val timetoclinic_cat timetoclinic_cat

*gen age cat at visit
bysort subjid: carryforward dobdtn, replace
replace agev=(visitdt-dobdtn)/365.25 if agev==.
gen agec=0 if agev<30
replace agec=1 if agev>=30 & agev<40
replace agec=2 if agev>=40 & agev<50
replace agec=3 if agev>=50 & agev!=.
label def agec 0 "18-29" 1 "30-39" 2 "40-49" 3 "50+"
label val agec agec
label var agec "Age at visit"
label var agev "Age at visit"

*recoding education
gen edu_cat=0 if educat==0 | educat==1
replace edu_cat=1 if educat==3 | educat==2
replace edu_cat=2 if educat>3 & educat!=.
label define edu_cat 0 "None or some primary" 1 "Primary or some secondary" 2 "Secondary and above"
label val edu_cat edu_cat
label var edu_cat "Education"

*recoding education BM definition
gen edu_cat2=0 if educat<=2
replace edu_cat2=1 if educat>2 & educat!=.
label define edu_cat2 0 "Primary or less" 1 "Secondary or above" 
label val edu_cat2 edu_cat2
label var edu_cat2 "Education"

*recoding marriage
gen married=1 if marital==2
replace married=0 if marital!=2 & marital!=.
label define married 1 "Married" 0 "Not married"
label val married married
label var married "Marital status"

*gen HIV stigma
gen stigma=0
replace stigma=1 if hvste_a==1 | hvste_b==1 | hvste_c==1 
label var stigma "Experienced HIV stigma"
label val stigma yn

*disclosed HIV status
gen disclose=0
replace disclose=1 if whiv_a==1 | whiv_b==1 | whiv_c==1 | whiv_d==1 | whiv_e==1 | whiv_f==1 | whiv_g==1 | whiv_h==1 | whiv_i==1 | whiv_j==1 
label val disclose yn
label var disclose "Disclosed HIV status"

*gen dissatification
gen dissat=0
replace dissat=1 if artsrv_a==2 | artsrv_b==2 | artsrv_c==2 | artsrv_d==2 | artsrv_f==2 | artsrv_g==2
labe val dissat yn

label def dissat 1 "Satisfied" 2 "Needs to improve"
label val artsrv_a dissat
label val artsrv_b dissat
label val artsrv_c dissat
label val artsrv_d dissat
label val artsrv_e dissat
label val artsrv_f dissat
label val artsrv_g dissat

// *support group
// gen suppgroup=0 if sghivfrq==0|sghivfrq==1|sghivfrq==2
// replace suppgroup=1 if sghivfrq==3|sghivfrq==4
//
// label def suppgroup 0 "once per month or less" 1 "more than once per month"
// label val suppgroup suppgroup

*on art
bysort subjid: carryforward diagdtn, replace
bysort subjid: carryforward art_sdtn, replace

replace art_sdtn=astartdtcn if art_sdtn==.
replace art_sdtn=startdtcn if art_sdtn==.
bysort subjid: carryforward art_sdtn, replace

replace dur_hiv=(visitdt-diagdtn)/365.25 if dur_hiv==.
replace dur_art=(visitdt-art_sdtn)/365.25 if dur_art==.

replace nanotart=1 if subjid=="D01-0172" & visit==2
replace takearv=1 if art_sdtn!=.
replace takearv=0 if art_sdtn==. & nanotart==1

*vl
gen vs=0 if vl>=1000 & vl!=. & dur_art>=0.5 & dur_art!=.
replace vs=1 if vl<1000 & dur_art>=0.5 & dur_art!=.
replace vs=2 if vl!=. & dur_art<0.5
label def vs 0 "Failing" 1 "Suppressed" 2 "On ART <6 months"
label val vs vs

gen vl1000=0 if vl<1000 & takearv==1
replace vl1000=1 if vl>=1000 & vl!=. & takearv==1
label def vl1000 0 "on ART, vl<1000" 1 "on ART, vl>=1000"
label val vl1000 vl1000

*CD4_cat
gen cd4_cat=0 if cd3_4_n<200
replace cd4_cat=1 if cd3_4_n>=200 & cd3_4_n!=.
label define cd4_cat 0"<200" 1 ">=200" 
label val cd4_cat cd4_cat
label var cd4_cat "CD4 count"

gen cd4_enroll=cd3_4_n if visit==1
bysort subjid: carryforward cd4_enroll, replace
gen cd4_enroll_cat=0 if cd4_enroll<200
replace cd4_enroll_cat=1 if cd4_enroll>=200 & cd4_enroll!=.
label val cd4_enroll_cat cd4_cat

*categorize missed arv doses
gen missdose=0 if missarv==0
replace missdose=1 if missarv==1
replace missdose=2 if missarv==2|missarv==3|missarv==4
label define missdose 0 "No days missed" 1 "1-2 days missed" 2 "3+ days missed"
label val missdose missdose
label var missdose "Missed doses ARV in past month"

label def missarv 0 "None" 1 "1-2 days" 2 "3-5 days" 3 "6-10 days" 4 ">10 days" 7 "Unknown"
label val missarv missarv 

*time from diagnosis to art start
gen artstart=(art_sdtn-diagdtn)/365.2422
gen dxart=0 if artstart==0
replace dxart=1 if artstart>0 & artstart<0.02
replace dxart=2 if artstart>=.02 & artstart<.5
replace dxart=3 if artstart>=.5 & artstart!=.
label def dxart 0 "0 days" 1 "<1 week" 2 "1 wk-6 months" 3 "6 months +"
label val dxart dxart
label var dxart "Time from dx to ART start"

*time since HIV dx, duration on ART
gen hivdur=0 if dur_hiv<1
replace hivdur=1 if dur_hiv>=1 & dur_hiv<=5
replace hivdur=2 if dur_hiv>5 &  dur_hiv!=.
label def hivdura 0 "<1 yr" 1 "1-5 yrs" 2 ">5 yrs" 
label val hivdur hivdura
label var hivdur "Time since HIV diagnosis"

gen artdurc=1 if dur_art<2
replace artdurc=2 if dur_art>=2 & dur_art<4
replace artdurc=3 if dur_art>=4 & dur_art!=.
label def artdurcd 1 "<2 yrs" 2 ">=2 yrs-<4 yrs" 3 ">=4 yrs"
label val artdurc artdurcd
label var artdurc "Duration on ART"

*year of dx and art start
gen yr_hivdx=year(diagdtn)
gen yr_artst=year(art_sdtn)

gen yr_hivdx_cat=1 if yr_hivdx<=2005
replace yr_hivdx_cat=2 if yr_hivdx>2005 & yr_hivdx<=2010
replace yr_hivdx_cat=3 if yr_hivdx>2010 & yr_hivdx<=2015
replace yr_hivdx_cat=4 if yr_hivdx>2015 & yr_hivdx!=.
label def yr_hivdx_cat 1 "1985-2005" 2 "2006-2010" 3 "2011-2015" 4 "2016-2019"
label val yr_hivdx_cat yr_hivdx_cat
label var yr_hivdx_cat "Year diagnosed with HIV"

gen yr_artst_cat=1 if yr_artst<=2005
replace yr_artst_cat=2 if yr_artst>2005 & yr_artst<=2010
replace yr_artst_cat=3 if yr_artst>2010 & yr_artst<=2015
replace yr_artst_cat=4 if yr_artst>2015 & yr_artst!=.
label def yr_artst_cat 1 "1999-2005" 2 "2006-2010" 3 "2011-2015" 4 "2016-2019"
label val yr_artst_cat yr_artst_cat
label var yr_artst_cat "Year started ART"

*enrollment year
gen yearenrolled=year(visitdt) if visit==1
bysort subjid: carryforward yearenrolled, replace
gen year_enrolled_c=1 if yearenrolled==2013|yearenrolled==2014
replace year_enrolled_c=2 if yearenrolled==2015|yearenrolled==2016
replace year_enrolled_c=3 if yearenrolled==2017|yearenrolled==2018
replace year_enrolled_c=4 if yearenrolled==2019|yearenrolled==2020
label def year_enrolled_c 1 "2013-2014" 2 "2015-2016" 3 "2017-2018" 4 "2019-2020"
label val year_enrolled_c year_enrolled_c
label var year_enrolled_c "Year enrolled"

gen everincarcerated=prison if visit==1
bysort subjid: carryforward everincarcerated, replace

gen prison_any=prison if prison==1
gsort subjid -visit
bysort subjid: carryforward prison_any, replace
tab visit prison_any

*other labels
label def yn 0 "No" 1 "Yes"
label val depress yn
label val employed yn
label val readwrit yn
label val hivfood yn
label val arvsupp yn
label val stigma yn
label val disclose yn
label val dissat yn
label val food yn
label val alcohol yn
label val drug yn
label val prison yn
label val everincarcerated yn
label val tb yn

*frequency of follow up clinic visits
label def hlthfu 1 "Monthly" 2 "Every 2 months" 3 "Every 3 months" 4 "Every 6 months" 5 "No routine f/u visits" 6 "Every 4 months" 7 "Every 5 months"  8 "Every week" 9 "Every 2 weeks" 10 "Every 3 weeks" 90 "Other/unknown"
label val hlthfu hlthfu

replace hlthfu=8 if strpos(hlthftxt,"AFTER ONE WEEK")|strpos(hlthftxt,"EVERY AFTER ONE WEEK")|strpos(hlthftxt,"EVERY WEEK")|strpos(hlthftxt,"ONE WEEK")|strpos(hlthftxt,"WEEKLY")
replace hlthfu=9 if strpos(hlthftxt,"2 WEEKLY")|strpos(hlthftxt,"2 WEEKS")|strpos(hlthftxt,"AFTER TWO WEEK'S")|strpos(hlthftxt,"AFTER TWO WEEKES")|strpos(hlthftxt,"AFTER TWO WEEKS")|strpos(hlthftxt,"EVERY 2 WEEKS")|strpos(hlthftxt,"EVERY AFTER TWO WEEKS")|strpos(hlthftxt,"EVERY OTHER WEEK")|strpos(hlthftxt,"EVERY TWO WEEK")|strpos(hlthftxt,"EVERY TWO WEEKS")|strpos(hlthftxt,"TWICE A MONTH")|strpos(hlthftxt,"TWICE AMONTH")|strpos(hlthftxt,"TWICE WEEKLY")|strpos(hlthftxt,"TWO WEEK'S")|strpos(hlthftxt,"TWO WEEKLY")|strpos(hlthftxt,"TWO WEEKS")|strpos(hlthftxt,"VERY TWO WEEKS")
replace hlthfu=10 if strpos(hlthftxt,"EVERY THREE WEEKS")|strpos(hlthftxt,"THREE WEEKS")
replace hlthfu=1 if strpos(hlthftxt,"EVERY MONTH")|strpos(hlthftxt,"ONE MONTH")
replace hlthfu=2 if strpos(hlthftxt,"2 MONTHS")|strpos(hlthftxt,"AFTER EVERY TWO MONTHS")|strpos(hlthftxt,"AFTER TWO MONTHS")|strpos(hlthftxt,"ENERY 2 MONTHS")|strpos(hlthftxt,"EVERY 2 MONTHS")|strpos(hlthftxt,"EVERY AFTER 02 MONTHS")|strpos(hlthftxt,"EVERY AFTER 2 MONTHS")|strpos(hlthftxt,"EVERY AFTER TWO  MONTHS")|strpos(hlthftxt,"EVERY AFTER TWO MIONTHS")|strpos(hlthftxt,"EVERY AFTER TWO MONTH")|strpos(hlthftxt,"EVERY AFTER TWO MONTHS")|strpos(hlthftxt,"EVERY TW0 MONTHS")|strpos(hlthftxt,"EVERY TWO MONTHS")|strpos(hlthftxt,"TWO MONTHLY")|strpos(hlthftxt,"TWO MONTHS")
replace hlthfu=3 if strpos(hlthftxt,"EVERY THREE MONTHS")
replace hlthfu=4 if strpos(hlthftxt,"EVERY 6 MONTHS")
replace hlthfu=6 if strpos(hlthftxt,"EVERY FOUR MONTH")|strpos(hlthftxt,"EVERY FOUR MONTHS")|strpos(hlthftxt,"FOUR - MONTHS")|strpos(hlthftxt,"FOUR MONTHS")
replace hlthfu=7 if strpos(hlthftxt,"FIVE MONTHS")
replace hlthfu=5 if strpos(hlthftxt,"CLIENT DOES NOT FREQUENT CLINIC")|strpos(hlthftxt,"CLIENT DOES NOT FREQUENT THE CLINIC")|strpos(hlthftxt,"DIAGNOSED RECENTLY")|strpos(hlthftxt,"DOES NOT APPLY")|strpos(hlthftxt,"DOEST APPL")|strpos(hlthftxt,"FIRST TIME")|strpos(hlthftxt,"FIRST VISIT TO THE CLINIC")|strpos(hlthftxt,"NA")|strpos(hlthftxt,"NEW CLIENT")|strpos(hlthftxt,"NEW ENROLLMENT IN CLINIC")|strpos(hlthftxt,"NEW IN CLINIC , LESS THAN ONE MONTH")|strpos(hlthftxt,"NEWLY DIAGNOSED")|strpos(hlthftxt,"NEWLY ENROLLED")|strpos(hlthftxt,"NEWLY ENROLLED TO CLINIC")|strpos(hlthftxt,"NEWLY REGISTERED")|strpos(hlthftxt,"NONE")|strpos(hlthftxt,"NOT APLICABLE")|strpos(hlthftxt,"NOT APPLICABLE")|strpos(hlthftxt,"NOT APPLIED")|strpos(hlthftxt,"NOT ON ART")|strpos(hlthftxt,"NOT ON ART HE ON CLASS SESSION")|strpos(hlthftxt,"THE PARTICIPANT DOES NOT FREQUENT THE")|strpos(hlthftxt,"HAIHUSIKI")|strpos(hlthftxt,"HAUSIKI")

gen freqvis=1 if hlthfu==8
replace freqvis=2 if hlthfu==9
replace freqvis=3 if hlthfu==10
replace freqvis=4 if hlthfu==1
replace freqvis=5 if hlthfu==2
replace freqvis=6 if hlthfu==3
replace freqvis=7 if hlthfu==6
replace freqvis=8 if hlthfu==7
replace freqvis=9 if hlthfu==4
replace freqvis=10 if hlthfu==5

label def freqvis 1 "Every week" 2 "Every 2 weeks" 3 "Every 3 weeks" 4 "Monthly" 5 "Every 2 months" 6 "Every 3 months" 7 "Every 4 months" 8 "Every 5 months" 9 "Every 6 months" 10 "No routine f/u visits"
label val freqvis freqvis

/*
*check for non-possible values
list subjid visit hlthfu freqvis hlthmnum if hlthmnum>10 & hlthmnum!=.

*recode non-possible values
replace hlthmnum=. if hlthmnum>=30
*/

*generating missed visits
gen misvis=0 if hlthmnum==0 & freqvis<10
replace misvis=1 if hlthmnum>0 & hlthmnum!=. & freqvis<10
label def misvis 0 "No missed visits" 1 "Missed visits"
label var misvis "Missed visits in last 6 months"
label val misvis misvis

*cross tab frequency of visits with any missed visits
tab freqvis misvis if freqvis<10 & hivflag==1 & visit==1, col chi2
tab freqvis misvis if freqvis<10 & hivflag==1, col chi2

*generating expected number 6-monthly visits
gen expected=24 if freqvis==1
replace expected=12 if freqvis==2
replace expected=8 if freqvis==3
replace expected=6 if freqvis==4
replace expected=3 if freqvis==5
replace expected=2 if freqvis==6
replace expected=1.5 if freqvis==7
replace expected=1.2 if freqvis==8
replace expected=1 if freqvis==9

*prop of expected missed
gen propmis=hlthmnum/expected

/*
*check for non-possible values
list subjid visit visitdt misvis freqvis hlthfu hlthftxt hlthmnum apptna propmis if propmis>1 & propmis!=. & hivflag==1

*recode non-possible values
replace propc=. if propc>1
*/

tabstat expected if visit==1, stats(p25 p50 p75 mean sd)
tabstat expected, stats(p25 p50 p75 mean sd)
tabstat propmis, stats(p25 p50 p75 mean sd)

gen propc=0 if propmis==0
replace propc=1 if propmis>0 & propmis<0.5
replace propc=2 if propmis>=0.5 & propmis<1
replace propc=3 if propmis>=1 & propmis!=.

label def propc 0 "0% missed" 1 "1-49% missed" 2 "50-99% missed" 3 "100% missed"
label val propc propc

*describe analytic population
gsort subjid visit
tab visit hivflag
tab visit takearv if hivflag==1, missing

keep if hivflag==1 & takearv==1
bysort subjid: gen n=_n
tab n
/*n=2807 PLWH on ART*/

*complete case analysis
// gen pop=1 if misvis!=. & agec!=. & gender!=. & progid!=. & married!=. & edu_cat!=. & employed!=. & depress!=. & tb!=. & distance10!=. & timetoclinic_cat!=. & stigma!=. & disclose!=. & artsrv_a!=. & vl1000!=. & cd4_cat!=. & food!=. & alcohol!=. & drug!=. & everincarcerated!=. & missdose!=. & artdurc!=. & yr_artst_cat!=.

keep if misvis!=. & agec!=. & gender!=. & progid!=. & married!=. & edu_cat!=. & employed!=. & depress!=. & tb!=. & distance10!=. & timetoclinic_cat!=. & stigma!=. & disclose!=. & artsrv_a!=. & vl1000!=. & cd4_cat!=. & food!=. & alcohol!=. & drug!=. & everincarcerated!=. & missdose!=. & artdurc!=. & yr_artst_cat!=.

bysort subjid: gen n2=_n
tab n2

*summary of follow up time
bysort subjid: gen tot_v=_N
tabstat tot_v if n2==1, stats(mean sd p25 p50 p75)

gen first=visitdt if n2==1
bysort subjid: carryforward first, replace
format first %d

gsort subjid -visit
bysort subjid: gen n3=_n
gen last=visitdt if n3==1
bysort subjid: carryforward last, replace
format last %d

gen dur_fu=(last-first)/365.25
tabstat dur_fu if n2==1, stats(mean sd p25 p50 p75)

*year of first visit included
gen year1=year(visitdt) if n2==1
bysort subjid: carryforward year1, replace

*figs 1&2
gen freqvis2=1 if freqvis==1|freqvis==2
replace freqvis2=2 if freqvis==4
replace freqvis2=3 if freqvis==5
replace freqvis2=4 if freqvis==6
replace freqvis2=5 if freqvis==7|freqvis==8|freqvis==9

label def freqvis2 1 "Every 1-2 weeks" 2 "Monthly" 3 "Every 2 months" 4 "Every 3 months" 5 "Less than every 3 months"
label val freqvis2 freqvis2

tab freqvis2 if hivflag==1 & takearv==1 & n2==1, missing
tab freqvis2 misvis if hivflag==1 & takearv==1 & n2==1, missing chi2

tab year1 freqvis2 if n2==1, chi2

*table 1
table1_mc if n2==1 & freqvis<10, by(misvis) vars(agec cat\gender cat\progid cat\married cat\edu_cat cat\employed cat\alcohol cat\drug cat\food cat\everincarcerated cat\distance10 cat\timetoclinic_cat cat\artsrv_a cat\depress cat\tb cat\yr_artst_cat cat\artdurc cat\stigma cat\disclose cat\missdose cat\cd4_cat cat\vl1000 cat) onecol format(%2.1f) total(b) missing sav("table1_bin.xlsx", replace)

table1_mc if n2==1 & freqvis<10, by(propc) vars(agec cat\gender cat\progid cat\married cat\edu_cat cat\employed cat\alcohol cat\drug cat\food cat\everincarcerated cat\distance10 cat\timetoclinic_cat cat\artsrv_a cat\depress cat\tb cat\yr_artst_cat cat\artdurc cat\stigma cat\disclose cat\missdose cat\cd4_cat cat\vl1000 cat) onecol format(%2.1f) total(b) missing sav("table1_cat.xlsx", replace)

*other numbers for results
tabstat agev if n2==1, stats(p25 p50 p75)
tabstat howfar if n2==1, stats(p25 p50 p75)
tabstat timetoclinic if n2==1, stats(p25 p50 p75)
tabstat dur_art if n2==1, stats(p25 p50 p75)

*models
*unadjusted models (logistic)
egen pin = group(subjid)
xtgee misvis ib3.agec, i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee misvis i.gender, i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee misvis i.progid, i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee misvis i.married, i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee misvis i.edu_cat, i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee misvis ib1.employed, i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee misvis i.alcohol, i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee misvis i.drug, i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee misvis i.food, i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee misvis i.everincarcerated, i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee misvis i.distance10, i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee misvis i.timetoclinic_cat, i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee misvis i.artsrv_a, i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee misvis i.depress, i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee misvis i.tb, i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee misvis i.yr_artst_cat, i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee misvis i.artdurc, i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee misvis i.stigma, i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee misvis i.disclose, i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee misvis i.missdose, i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee misvis i.vl1000, i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee misvis ib1.cd4_cat, i(pin) family(bin) link(logit) corr(exch) robust ef nolog

*adjusted model (logistic)
xtgee misvis ib3.agec i.gender i.progid i.married i.edu_cat ib1.employed i.alcohol i.drug i.food i.everincarcerated i.distance10 i.timetoclinic_cat i.artsrv_a i.depress i.tb i.yr_artst_cat i.artdurc i.stigma i.disclose i.missdose i.vl1000 ib1.cd4_cat, i(pin) family(bin) link(logit) corr(exch) robust ef nolog

**check for multicollinearity
*run collin.ado from C:\Users\ndear\Box Sync\NDear\RV432\stata code\collin
collin agec gender progid married edu_cat employed artdurc yr_artst_cat distance10 timetoclinic_cat artsrv_a depress tb stigma disclose food alcohol drug everincarcerated vs cd4_cat missdose lag_misvis

*unadjusted models (multinomial logistic)
mlogit propc ib3.agec, cluster(pin) robust rr nolog
mlogit propc i.gender, cluster(pin) robust rr nolog
mlogit propc i.progid, cluster(pin) robust rr nolog
mlogit propc i.married, cluster(pin) robust rr nolog
mlogit propc i.edu_cat, cluster(pin) robust rr nolog
mlogit propc ib1.employed, cluster(pin) robust rr nolog
mlogit propc i.alcohol, cluster(pin) robust rr nolog
mlogit propc i.drug, cluster(pin) robust rr nolog
mlogit propc i.food, cluster(pin) robust rr nolog
mlogit propc i.everincarcerated, cluster(pin) robust rr nolog
mlogit propc i.distance10, cluster(pin) robust rr nolog
mlogit propc i.timetoclinic_cat, cluster(pin) robust rr nolog
mlogit propc i.artsrv_a, cluster(pin) robust rr nolog
mlogit propc i.depress, cluster(pin) robust rr nolog
mlogit propc i.tb, cluster(pin) robust rr nolog
mlogit propc i.yr_artst_cat, cluster(pin) robust rr nolog
mlogit propc i.artdurc, cluster(pin) robust rr nolog
mlogit propc i.stigma, cluster(pin) robust rr nolog
mlogit propc i.disclose, cluster(pin) robust rr nolog
mlogit propc i.missdose, cluster(pin) robust rr nolog
mlogit propc i.vl1000, cluster(pin) robust rr nolog
mlogit propc ib1.cd4_cat, cluster(pin) robust rr nolog

*adjusted models (multinomial logistic)
mlogit propc ib3.agec i.gender i.progid i.married i.edu_cat ib1.employed i.alcohol i.drug i.food i.everincarcerated i.distance10 i.timetoclinic_cat i.artsrv_a i.depress i.tb i.yr_artst_cat i.artdurc i.stigma i.disclose i.missdose i.vl1000 ib1.cd4_cat, cluster(pin) robust rr nolog

*reasons for missing clinic visits
gen caresickfam=1 if strpos(hlthmtxt,"CARE FOR SICK")|strpos(hlthmtxt,"CHILD WAS ADMITTED")|strpos(hlthmtxt,"SICK RELATIVE")|strpos(hlthmtxt,"SICK RELATIVE")|strpos(hlthmtxt,"SICK FATHER")|strpos(hlthmtxt,"LOOK AFTER")|strpos(hlthmtxt,"MOTHER WAS SICK")|strpos(hlthmtxt,"SICK GRAND DAUGHTER")|strpos(hlthmtxt,"SICK CHILD")|strpos(hlthmtxt,"SICK MOTHER")|strpos(hlthmtxt,"SICK NEPHEW")|strpos(hlthmtxt,"TREAT THE PARENTS")|strpos(hlthmtxt,"TAKING CARE OF")|strpos(hlthmtxt,"RELATIVE HAD ROAD TRAFFIC ACCIDENT")|strpos(hlthmtxt,"SPOUSE WAS ILL")|strpos(hlthmtxt,"SICK  PATIENT")|strpos(hlthmtxt,"SICK HUSBAND")|strpos(hlthmtxt,"ATTENDING TO A PATIENT")|strpos(hlthmtxt,"SICK SON")|strpos(hlthmtxt,"PATIENT IN HOSPITAL")

gen funeral=1 if strpos(hlthmtxt,"BURIAL")|strpos(hlthmtxt,"LOST A RELATIVE")|strpos(hlthmtxt,"BURRIAL")|strpos(hlthmtxt,"LOST MY SON")|strpos(hlthmtxt,"LOST A CLOSE RELATIVE")|strpos(hlthmtxt,"BEREAVED")|strpos(hlthmtxt,"FUNERAL")|strpos(hlthmtxt,"LOST FATHER")|strpos(hlthmtxt,"HUSBAND PASSED ON")|strpos(hlthmtxt,"LOST HER CHILD")|strpos(hlthmtxt,"BARRIES CEREMONY")|strpos(hlthmtxt,"BEREAVEMENT")|strpos(hlthmtxt,"LOST A RELATIVE")

gen domestic=1 if strpos(hlthmtxt,"SOCIAL CONFLICT")|strpos(hlthmtxt,"DOMESTIC")|strpos(hlthmtxt,"FAMILY CONFLICT")|strpos(hlthmtxt,"DISPUTE")|strpos(hlthmtxt,"CHASED BY YOUR MOTHER-IN-LAW")

gen hospital=1 if strpos(hlthmtxt,"HOSPITAL")|strpos(hlthmtxt,"OPERATION")|strpos(hlthmtxt,"ADMITTED TO GET KAPOSI'S SARCOMA DRUGS")|strpos(hlthmtxt,"GETTING TREATMENT")|strpos(hlthmtxt,"SURGERY")|strpos(hlthmtxt,"REHABILITATION CENTRE")

gen lostapptcard=1 if strpos(hlthmtxt,"APPOINTMENT CARD GOT LOST")|strpos(hlthmtxt,"LOST HIS FOLLOW -UP CARD")|strpos(hlthmtxt,"MISPLACED")|strpos(hlthmtxt,"LOST HIS APPOINTMENT CARD")|strpos(hlthmtxt,"MISPLACED")

gen incarcerated=1 if strpos(hlthmtxt,"INCARCERATED")|strpos(hlthmtxt,"PRISON")|strpos(hlthmtxt,"IMPROSONED")|strpos(hlthmtxt,"JAIL")|strpos(hlthmtxt,"IN LOCK UP")

gen diffclinic=1 if strpos(hlthmtxt,"ANOTHER CLINIC")|strpos(hlthmtxt," DIFFERENT FACILITY")|strpos(hlthmtxt,"NEAR DISPENSARY")

gen stopart=1 if strpos(hlthmtxt,"HAD STOPPED TAKING MEDS")|strpos(hlthmtxt,"TIRED OF THE DRUGS")

gen enoughart=1 if strpos(hlthmtxt,"HAD ENOUGH MEDICATIONS")|strpos(hlthmtxt,"HAD ALOT OF STOCK")|strpos(hlthmtxt,"HAD EXTRA PILLS")|strpos(hlthmtxt,"HAD ENOUGH ARVS")|strpos(hlthmtxt,"ENOUGH DRUGS")|strpos(hlthmtxt,"STILL HAD DRUGS")|strpos(hlthmtxt,"ENOUGH MEDICATIONS")|strpos(hlthmtxt,"STILL HAVE DRUGS")

gen workschool=1 if strpos(hlthmtxt,"JOB COMMITTMENT")|strpos(hlthmtxt,"MY EMPLOYER DIDN'T PERMIT ME")|strpos(hlthmtxt,"BOSS REFUSED")|strpos(hlthmtxt,"SCHOOL")|strpos(hlthmtxt,"TOO MUCH WORK")|strpos(hlthmtxt,"3 MONTHS TRAINING")|strpos(hlthmtxt,"ATTENDED INTERVIEW")|strpos(hlthmtxt,"GOT EMPLOYED")|strpos(hlthmtxt,"DENIED PERMISSION")|strpos(hlthmtxt,"AT WORK")|strpos(hlthmtxt,"WORK CONFLICT")|strpos(hlthmtxt,"EMPLOYER REFUSED")|strpos(hlthmtxt,"WENT FOR TRAINING")|strpos(hlthmtxt,"BOSS DID NOT RELEASE ME TO COME")|strpos(hlthmtxt,"TRAINING")|strpos(hlthmtxt,"EMPLOYMENT")|strpos(hlthmtxt,"SENT TO WORK")|strpos(hlthmtxt,"WORKED FROM FAR")|strpos(hlthmtxt,"GONE TO WORK")

replace hlthm_a=1 if hlthm_r==1|strpos(hlthmtxt,"BUSY")
replace hlthm_b=1 if strpos(hlthmtxt,"MONEY")
replace hlthm_c=1 if strpos(hlthmtxt,"TRANSPORT")
replace hlthm_d=1 if strpos(hlthmtxt,"EYE PROBLEM")|strpos(hlthmtxt,"WOUND ON MY FOOT")|strpos(hlthmtxt,"ACCIDENT")|strpos(hlthmtxt,"CONFUSED (VIRAL ENCEPHALITIS)")|strpos(hlthmtxt,"DEPRESSED")
replace hlthm_e=1 if strpos(hlthmtxt,"TOO FAR")
replace hlthm_j=1 if strpos(hlthmtxt,"FELT BETTER")
replace hlthm_o=1 if strpos(hlthmtxt,"ON DUTY")
replace hlthm_p=1 if strpos(hlthmtxt,"TRAVEL")|strpos(hlthmtxt,"WAS AWAY")|strpos(hlthmtxt,"HAD GONE")|strpos(hlthmtxt,"FROM FAR")|strpos(hlthmtxt,"FAR AWAY")|strpos(hlthmtxt,"WAS OUTSIDE THE COUNTRY")|strpos(hlthmtxt,"WAS ON A HOLIDAY")|strpos(hlthmtxt,"FAR AWAY")|strpos(hlthmtxt,"WAS OUT OF TOWN")|strpos(hlthmtxt,"WENT FOR A 21-DAY CHURCH PROGRAM")|strpos(hlthmtxt,"MOVED OUT OF AREA")
replace hlthm_p=0 if strpos(hlthmtxt,"COULD NOT TRAVEL DUE TO CIVIL UNREST")|strpos(hlthmtxt,"TRAVEL PROBLEM")
replace hlthm_q=1 if strpos(hlthmtxt,"FORGOT")|strpos(hlthmtxt,"FORGETFULNESS")|strpos(hlthmtxt,"DIDN'T UNDERSTAND THE APPOINTMENT")|strpos(hlthmtxt,"CONFUSED THE DATES")|strpos(hlthmtxt,"DID NOT CONFIRM THE DATE")|strpos(hlthmtxt,"NOT AWARE OF THE RETURN DATE")|strpos(hlthmtxt,"CONFUSED HIS RETURN DATE")|strpos(hlthmtxt,"DID NOT CHECK MY APPOINTMENT DATE")|strpos(hlthmtxt,"DIDN'T KNOW WHEN TO COME BACK")|strpos(hlthmtxt,"NEGLIGENCE")|strpos(hlthmtxt,"FORGETFULLNESS")|strpos(hlthmtxt,"MIXED UP APPOINTMENT DATE")|strpos(hlthmtxt,"DID NOT CHECK MY APPOINTMENT DATE")

gen other=1 if misvis==1 & hlthm_z==1 & (diffclinic!=1 & caresickfam!=1 & domestic!=1 & funeral!=1 & hospital!=1 & lostapptcard!=1 & incarcerated!=1 & stopart!=1 & enoughart!=1 & workschool!=1 & hlthm_a!=1 & hlthm_b!=1 & hlthm_c!=1 & hlthm_d!=1 & hlthm_e!=1 & hlthm_f!=1 & hlthm_g!=1 & hlthm_h!=1 & hlthm_i!=1 & hlthm_j!=1 & hlthm_k!=1 & hlthm_l!=1 & hlthm_m!=1 & hlthm_n!=1 & hlthm_o!=1 & hlthm_p!=1 & hlthm_q!=1)

*ppts missing visits
tab misvis if n2==1
tabstat hlthmnum if n2==1 & misvis==1, stats(p25 p50 p75)

*number unique ppts missing visits
gsort subjid -visit
gen missed=1 if misvis==1
bysort subjid: carryforward missed, replace
tab missed if visit==1

*how many reported missed visits at more than one visit
gsort subjid visit
bysort subjid: gen num_missed = sum(misvis == 1)
bysort subjid: egen maxmis = max(num_missed)

tab maxmis if visit==1 & maxmis>0
tabstat maxmis if visit==1, stats(min max mean p25 p50 p75)
tabstat maxmis if visit==1 & maxmis>1, stats(min max mean p25 p50 p75)

*ppt level reasons
gen reason1=1 if hlthm_a==1
gen reason2=1 if hlthm_b==1
gen reason3=1 if hlthm_c==1
gen reason4=1 if hlthm_d==1|hospital==1
gen reason5=1 if hlthm_e==1
gen reason6=1 if hlthm_f==1
gen reason7=1 if hlthm_g==1|domestic==1
gen reason8=1 if hlthm_h==1
gen reason9=1 if hlthm_i==1
gen reason10=1 if hlthm_j==1
gen reason11=1 if hlthm_k==1
gen reason12=1 if hlthm_l==1
gen reason13=1 if hlthm_m==1
gen reason14=1 if hlthm_n==1
gen reason15=1 if hlthm_o==1
gen reason16=1 if hlthm_p==1
gen reason17=1 if hlthm_q==1|lostapptcard==1
gen reason18=1 if caresickfam==1|funeral==1
gen reason19=1 if incarcerated==1
gen reason20=1 if diffclinic==1
gen reason21=1 if stopart==1
gen reason22=1 if enoughart==1
gen reason23=1 if workschool==1
gen reason24=1 if other==1

gsort subjid -visit
bysort subjid: carryforward reason1 reason2 reason3 reason4 reason5 reason6 reason7 reason8 reason9 reason10 reason11 reason12 reason13 reason14 reason15 reason16 reason17 reason18 reason19 reason20 reason21 reason22 reason23 reason24, replace

tab reason1 if visit==1 & missed==1
tab reason2 if visit==1 & missed==1
tab reason3 if visit==1 & missed==1
tab reason4 if visit==1 & missed==1
tab reason5 if visit==1 & missed==1
tab reason6 if visit==1 & missed==1
tab reason7 if visit==1 & missed==1
tab reason8 if visit==1 & missed==1
tab reason9 if visit==1 & missed==1
tab reason10 if visit==1 & missed==1
tab reason11 if visit==1 & missed==1
tab reason12 if visit==1 & missed==1
tab reason13 if visit==1 & missed==1
tab reason14 if visit==1 & missed==1
tab reason15 if visit==1 & missed==1
tab reason16 if visit==1 & missed==1
tab reason17 if visit==1 & missed==1
tab reason18 if visit==1 & missed==1
tab reason19 if visit==1 & missed==1
tab reason20 if visit==1 & missed==1
tab reason21 if visit==1 & missed==1
tab reason22 if visit==1 & missed==1
tab reason23 if visit==1 & missed==1
tab reason24 if visit==1 & missed==1

gen noreason=1 if reason1==. & reason2==. & reason3==. & reason4==. & reason5==. & reason6==. & reason7==. & reason8==. & reason9==. & reason10==. & reason11==. & reason12==. & reason13==. & reason14==. & reason15==. & reason16==. & reason17==. & reason18==. & reason19==. & reason20==. & reason21==. & reason22==. & reason23==. & reason24==.

tab noreason if visit==1 & missed==1