/*****************************************************************************
PROGRAM NAME: 		 AFRICOS PMTCT analysis     
AUTHOR:              Nicole Dear (adapted from AE original code)
DATE WRITTEN:        5MAR2024
REVIEWED BY:		
DATE REVIEWED:		
WRITTEN FOR:         AFRICOS 
PURPOSE:             
OVERVIEW:                                                       
INPUT DATA:          
OUTPUT DATA:           
RELIES ON:             
RELIED ON BY:          
SPECIAL INSTRUCTIONS:             
MODIFICATION HISTORY:  
DATE	MODIFIER	DESCRIPTION/REASON
*****************************************************************************/

*set working directory -----
cd "G:\DCAC\DCAC_PEPFAR\RV329\Analyses\PMTCT\Travers"

*load data -----
use pmtct_1mar2024.dta, clear

/*****************************************************************************
DATA CLEANING
*****************************************************************************/
*convert variable names to lowercase -----
rename *, lower

*carryforward variables collected only at enrollment and/or not captured during abbreviated study visits -----
bysort subjid: carryforward dobdtn, replace
bysort subjid: carryforward diagdtn, replace
bysort subjid: carryforward art_sdtn, replace
bysort subjid: carryforward gender, replace
bysort subjid: carryforward hivflag, replace

*add formats and create new variables as needed -----
replace progid=1 if strpos(subjid, "A") & progid==.
replace progid=2 if strpos(subjid, "B") & progid==.
replace progid=3 if strpos(subjid, "C") & progid==.
replace progid=4 if strpos(subjid, "D") & progid==.
replace progid=5 if strpos(subjid, "E") & progid==.

label var progid "Program site"
label define progid 1  "Kayunga, Uganda" 2 "South Rift Valley, Kenya" 3 "Kisumu West, Kenya" 4 "Mbeya, Tanzania" 5 "Abuja & Lagos Nigeria"
label val progid progid

gen country=1 if progid==1
replace country=2 if progid==2 | progid==3
replace country=3 if progid==4
replace country=4 if progid==5

label def country 1 "Uganda" 2 "Kenya" 3 "Tanzania" 4 "Nigeria"
label val country country
label var country "Country"

label var gender "Sex"
label def gender 1 "Male" 2 "Female"
label val gender gender

*clean & categorize age
replace agev=(visitdt-dobdtn)/365.25 if agev==.
format agev %6.0g

gen agec=0 if agev<25
replace agec=1 if agev>=25 & agev<40
replace agec=2 if agev>=40 & agev<50
replace agec=3 if agev>=50 & agev!=.
label def agec 0 "<25" 1 "25-39" 2 "40-49" 3 "50+"
label val agec agec
label var agec "Age at visit"
label var agev "Age at visit"

*age at enrollment
gen enroll_age=agev if visit==1
bysort subjid: carryforward enroll_age, replace

*age at HIV diagnosis
gen agedxc=(diagdtn-dobdtn)/365.25

gen agedx=0 if agedxc<25
replace agedx=1 if agedxc>=25 & agedxc<40
replace agedx=2 if agedxc>=40 & agedxc<50
replace agedx=3 if agedxc>=50 & agedxc!=.
label val agedx agec
label var agedxc "Age at Diagnosis"
label var agedx "Age at Diagnosis"

// count if age_del<agedx

*era of HIV diagnosis
gen erayear=year(diagdtn)

// gen era=1 if erayear<2006 
// replace era=2 if erayear>=2006 & erayear<2010
// replace era=3 if erayear>=2010 & erayear<2013
// replace era=4 if erayear>=2013 & erayear<2016
// replace era=5 if erayear>=2016 & erayear!=.
// label define era 1 "Prior to 2006" 2 "2006-2009" 3 "2010-2012" 4 "2013-2015" 5 "2016 onwards"
// label val era era
// label var era "Era of HIV diagnosis"

gen era2=1 if erayear<2016
replace era2=2 if erayear>=2016 & erayear!=.
label define era2 1 "Prior to 2016" 2 "2016 onwards"
label val era2 era2
label var era2 "Era of HIV diagnosis"

*collapse education (free text cleaned in SAS before export)
gen edu_cat=0 if educat==0 | educat==1
replace edu_cat=1 if educat==3 | educat==2
replace edu_cat=2 if educat>3 & educat!=.

label var edu_cat "Highest level of education"
label define edu_cat 0 "None or some primary" 1 "Primary or some secondary" 2 "Secondary and above"
label val edu_cat edu_cat

*pregnancy related variables -----
label def preg_out 1 "Live birth" 2 "Abortion" 3 "Stillbirth" 4 "Ongoing" 90 "Other"
label val preg_out preg_out

*fill in missing delivery date using age at delivery and mom DOB
gen deldt_imp = (dobdtn + (age_del*365.25)) if deldt==. & age_del!=.
format deldt_imp %td

replace deldt=deldt_imp if deldt==. & age_del!=.

gen deldt_yr=year(deldt)
label var deldt_yr "Year of delivery"

*fill in missing age at delivery using delivery date and mom DOB
gen age_del_imp = (deldt - dobdtn) / 365.25 if deldt!=. & age_del==.
replace age_del=age_del_imp if deldt!=. & age_del==.

*apply formats
label def yn 0 "No" 1 "Yes" 5 "NA" 7 "Unknown"
label val d_skill yn
label var d_skill "Delivery by skilled birth attendant"

label def d_place 1 "Home" 2 "Hospital"
label val d_place d_place
label var d_place "Place of delivery"

label def delivmet 0 "Vaginal" 1 "C-section"
label val delivmet delivmet
label var delivmet "Delivery method"

replace flsupx=. if flsupx==7
label val flsupx yn
label var flsupx "Folate supplement prescribed"

replace iptrxyn=. if iptrxyn==7
label val iptrxyn yn
label var iptrxyn "IPT with Sulfadoxine/Pyrimethamine prescribed"

replace pmtct=. if pmtct==7
label val pmtct yn
label var pmtct "PMTCT services accessed"

gen pmtct_period=0 if pmtct==0
replace pmtct_period=1 if pmtct==1 & pmtct_a==1 & pmtct_b!=1 & pmtct_c!=1
replace pmtct_period=2 if pmtct==1 & pmtct_a!=1 & pmtct_b==1 & pmtct_c!=1
replace pmtct_period=3 if pmtct==1 & pmtct_a!=1 & pmtct_b!=1 & pmtct_c==1
replace pmtct_period=4 if pmtct==1 & pmtct_a==1 & pmtct_b==1 & pmtct_c!=1
replace pmtct_period=5 if pmtct==1 & pmtct_a==1 & pmtct_b!=1 & pmtct_c==1
replace pmtct_period=6 if pmtct==1 & pmtct_a!=1 & pmtct_b==1 & pmtct_c==1
replace pmtct_period=7 if pmtct==1 & pmtct_a==1 & pmtct_b==1 & pmtct_c==1

label def pmtct_period 0 "No PMTCT" 1 "PMTCT services accessed before delivery" 2 "PMTCT services accessed at time of delivery" 3 "PMTCT services accessed postpartum" 4 "PMTCT services accessed before delivery and at time of delivery" 5 "PMTCT services accessed before delivery and postpartum" 6 "PMTCT services accessed at time of delivery and postpartum" 7 "PMTCT services accessed before delivery, at time of delivery and postpartum"
label val pmtct_period pmtct_period

*collpase missing and NA responses into unknown category
replace def_hiv=7 if def_hiv==.
replace def_hiv=7 if def_hiv==5
label def def_hiv 0 "HIV-" 1 "HIV+" 7 "Unknown"
label val def_hiv def_hiv
label var def_hiv "HIV status of child"

label val feed yn

gen infantfeed=0 if feed==0
replace infantfeed=1 if feed_met==1
replace infantfeed=2 if feed_met==2
replace infantfeed=3 if feed_met==3

label def infantfeed 0 "Mother not counseled on infant feeding" 1 "Exclusive breastfeeding" 2 "Exclusive replacement feeding" 3 "Mixed"
label val infantfeed infantfeed
label var infantfeed "Infant feeding method mother counseled on"

*mom dx with HIV during pregnancy
gen agedxr=trunc(agedxc)

gen mom_hivstatus=1 if mpristat==1 | ((agedxr < age_del) & (agedxr!=. & age_del!=.))
replace mom_hivstatus=2 if mom_hiv==1

label def mom_hivstatus 1 "Known HIV+ (HIV dx prior to pregnancy)" 2 "Newly dx with HIV during pregnancy"
label val mom_hivstatus mom_hivstatus
label var mom_hivstatus "Timing of mom HIV diagnosis"

gen mom_hivstatus2=1 if mpristat==1 | ((agedxr < age_del) & (agedxr!=. & age_del!=.))
replace mom_hivstatus2=2 if mom_hiv==1 & hiv_when==1
replace mom_hivstatus2=3 if mom_hiv==1 & hiv_when==2
replace mom_hivstatus2=4 if mom_hiv==1 & hiv_when==3
replace mom_hivstatus2=5 if mom_hiv==1 & hiv_when==4

label def mom_hivstatus2 1 "Known HIV+ (HIV dx prior to pregnancy)" 2 "Newly dx during 1st trimester" 3 "Newly dx during 2nd trimester" 4 "Newly dx during 3rd trimester" 5 "Newly dx at delivery"
label val mom_hivstatus2 mom_hivstatus2
label var mom_hivstatus2 "Timing of mom HIV diagnosis"

label val mom_hiv yn

*pregnancy outcomes -----
label def chd_stat 1 "Alive and well" 2 "Alive and chronically ill" 3 "Deceased" 7 "Unknown"
label val chd_stat chd_stat

*create gestational age variable (note: some are in weeks, some months - need to use unit variable)
gen gest37=0 if ((gest_age>=23 & gest_age<37 & gest_p==1) | (gest_age>6 & gest_age<9 & gest_p==2)) & gest_age!=. & preg_out==1
replace gest37=1 if ((gest_age>=37 & gest_p==1) | (gest_age>=9 & gest_p==2)) & gest_age!=. & preg_out==1
replace gest37=. if gest_age==. | preg_out!=1

label def gest37 0 "Preterm (<37 weeks)" 1 "Full term (37+ weeks)"
label val gest37 gest37
label var gest37 "Gestational age"

*flip coding to model preterm
gen preterm=0 if gest37==1
replace preterm=1 if gest37==0
label var preterm "Preterm birth"

*create variable for history of preterm birth outcome
gen prior_preterm=1 if preterm==1

gsort subjid deldt
bysort subjid: carryforward prior_preterm, replace

replace prior_preterm=. if gest37==0
replace prior_preterm=0 if prior_preterm==.

label def prior_preterm 0 "No prior history of preterm birth" 1 "Prior history of preterm birth"
label val prior_preterm prior_preterm
label var prior_preterm "Prior history of preterm birth"

*resort by subjid and visit
// gsort subjid visit

*infant mortality
gen deathdt_num = date(deathdt, "DMY")
format deathdt_num %td

gen age_died=(deathdt_num-deldt) if deathdt_num!=. & deldt!=.
replace chd_age=age_died if chd_age==. & age_died!=.
replace chd_dmy=1 if chd_dmy==. & age_died!=.

gen chldmort=.
replace chldmort=0 if chd_stat==1 | chd_stat==2
replace chldmort=1 if chd_stat==3 & ((chd_age<=356 & chd_dmy==1) | (chd_age<=12 & chd_dmy==2) | (chd_age<=1 & chd_dmy==3))
replace chldmort=2 if chd_stat==3 & chd_age==.
label var chldmort "Infant mortality"
label def chldmort 0 "Child alive" 1 "Child deceased <12 months of age" 2 "Child deceased age unknown"
label val chldmort chldmort

*ARV prescribed to mother
gen prenatal_art=1 if pres_a==1 | pres_b==1 | pres_c==1 | strpos(pres_txt, "ANTE")
gen intrapartum_art=1 if pres_d==1 | pres_e==1 | pres_f==1 | pres_l==1 | strpos(pres_txt, "INTRA") | strpos(pres_txt, "ITRA")
gen postpartum_art=1 if pres_g==1 | pres_h==1 | pres_i==1 | pres_j==1

gen mom_art=0 if prenatal_art==. & intrapartum_art==. & postpartum_art==.
replace mom_art=1 if prenatal_art==1 & intrapartum_art==. & postpartum_art==.
replace mom_art=2 if prenatal_art==. & intrapartum_art==1 & postpartum_art==.
replace mom_art=3 if prenatal_art==. & intrapartum_art==. & postpartum_art==1
replace mom_art=4 if prenatal_art==1 & intrapartum_art==1 & postpartum_art==.
replace mom_art=5 if prenatal_art==1 & intrapartum_art==. & postpartum_art==1
replace mom_art=6 if prenatal_art==. & intrapartum_art==1 & postpartum_art==1
replace mom_art=7 if prenatal_art==1 & intrapartum_art==1 & postpartum_art==1

label def mom_art 0 "None" 1 "Prenatal only" 2 "Intrapartum only" 3 "Postpartum only" 4 "Pre and intra" 5 "Pre and post" 6 "Intra and post" 7 "Pre, intra and postnatal"
label val mom_art mom_art
label var mom_art "ART prescribed to mother"

*frequencies of child ARV variables
label val chd_a yn
label val chd_b yn
label val chd_c yn
label val chd_d yn
label val chd_e yn
label val chd_f yn
label val chd_z yn

tab chd_arv trmtpro, missing
tab chd_arv chd_a, missing
tab chd_arv chd_b, missing
tab chd_arv chd_c, missing
tab chd_arv chd_d, missing
tab chd_arv chd_e, missing
tab chd_arv chd_f, missing
tab chd_arv chd_z, missing
tab oth13txt

gen childart1="SD NVP" if chd_a==1
gen childart2="Continuous NVP" if chd_b==1
gen childart3="3TC" if chd_c==1
gen childart4="AZT" if chd_d==1
gen childart5="Triple ARV" if chd_e==1
gen childart6="Unknown" if chd_f==1

egen childartconcat = concat(childart1 childart2 childart3 childart4 childart5 childart6 oth13txt), punct(,)
replace childartconcat = subinstr(childartconcat, ",", "", .)

tab childartconcat
tab childartconcat chd_arv, missing

tab chd_arv d_place, missing
tab childartconcat d_place, missing

label val chd_arv yn
label var chd_arv "ARV prescribed to child"

*create combined child ART variable
gen infant_art=0 if def_hiv==0
replace infant_art=1 if (def_hiv==1 | def_hiv==7) & chd_arv==0
replace infant_art=2 if (def_hiv==1 | def_hiv==7) & chd_arv==1 & childartconcat=="Continuous NVP"
replace infant_art=3 if (def_hiv==1 | def_hiv==7) & chd_arv==1 & (childartconcat=="Continuous NVPAZT" | childartconcat=="Continuous NVP6 WEEKS AZT" | childartconcat=="Continuous NVP6WEEEKS AZT" | childartconcat=="Continuous NVPGIVEN ALT FOR 6 WEEK")
replace infant_art=4 if (def_hiv==1 | def_hiv==7) & chd_arv==1 & (childartconcat=="SD NVP" | childartconcat=="NEVIRAPINE FOR 12 WE" | childartconcat=="NVP SYRUP")
replace infant_art=5 if (def_hiv==1 | def_hiv==7) & chd_arv==1 & (childartconcat=="SD NVPAZT" | childartconcat=="SD NVPAZTSEPTRIN")
replace infant_art=6 if (def_hiv==1 | def_hiv==7) & chd_arv==1 & (childartconcat=="3TCAZT" | childartconcat=="AZT" | childartconcat=="Continuous NVP3TCAZT" | childartconcat=="SD NVP3TCAZT" | childartconcat=="SD NVPContinuous NVP" | childartconcat=="SD NVPContinuous NVPAZT" | childartconcat=="SD NVPContinuous NVPAZTCTX" | childartconcat=="SD NVPTriple ARV" | childartconcat=="Triple ARV" | childartconcat=="Unknown" | childartconcat=="")
replace infant_art=7 if (def_hiv==1 | def_hiv==7) & (chd_arv==5 | chd_arv==7 | chd_arv==.)

label def infant_art 0 "Child HIV-" 1 "ARV not prescribed" 2 "Continuous NVP" 3 "Continuous NVP + AZT" 4 "Single dose NVP" 5 "Single dose NVP + AZT" 6 "Other/unspecified ARV" 7 "ARV status unknown"
label val infant_art infant_art
label var infant_art "ARV prescribed to child"

gen infant_art2=0 if infant_art==0
replace infant_art2=1 if infant_art==1
replace infant_art2=2 if infant_art==2 | infant_art==3
replace infant_art2=3 if infant_art==4 | infant_art==5
replace infant_art2=4 if infant_art==6 | infant_art==7

label def infant_art2 0 "Child HIV-" 1 "ARV not prescribed" 2 "Continuous NVP (alone or with AZT)" 3 "Single dose NVP (alone or with AZT)" 4 "Unspecified ARV or ARV status unknown"
label val infant_art2 infant_art2
label var infant_art2 "ARV prescribed to child"

label def trmtpro 1 "Treatment" 2 "Prophylaxis"
label val trmtpro trmtpro
*note: this variable has alot of missing (beyond missing due to skip pattern)

*was ARV given to child as prescribed
label def chd_give 0 "Not given" 1 "Sometimes given" 2 "Always given"
label val chd_give chd_give
label var chd_give "ART given to child as prescribed"

*generating study pregnancy variable
gen enrollmentdate=visitdt if visit==1
format enrollmentdate %td
bysort subjid: carryforward enrollmentdate, replace

gen studypreg=0
replace studypreg=1 if preg_out==1 & (deldt>enrollmentdate) & (deldt!=. & enrollmentdate!=.)
label def studypreg 0 "Prior to enrollment" 1 "During follow-up"
label val studypreg studypreg
label var studypreg "Pregnancy occurred"

/*****************************************************************************
DEFINE ANALYTIC POPULATION
*****************************************************************************/
*overall enrollment #s
bysort subjid: gen seq=_n
tab hivflag if seq==1, missing
tab hivflag gender if seq==1, missing

*restrict to female participants living with HIV
keep if gender==2 & hivflag==1

*only keep records with pregnancy reported
drop if preg_out==.

*drop pregnancies indicated as ongoing but visit date is prior to Jan 2023 (i.e., these women would no longer still be pregnant at time of data cutoff) (n=327)
drop if preg_out==4 & visitdt<date("01JAN2023", "DMY")

*drop pregnancies with duplicate delivery date
duplicates tag subjid deldt if deldt!=., gen(flag1)
duplicates drop subjid deldt if deldt!=. , force

// bysort subjid: gen flag2=1 if (visitdt-visitdt[_n-1]<365) & (preg_out==1 & preg_out[_n-1]==1)
// gen drop=1 if flag2==1 & nbirth!=2 & visit>1
// drop if flag2==1 & nbirth!=2 & visit>1

*then check whether any deliveries were reported less than 40 weeks (~280 days) apart and drop (n=16)
gsort subjid deldt
bysort subjid: gen flag2=1 if (deldt-deldt[_n-1]<280) & (preg_out==1 & preg_out[_n-1]==1) & deldt!=.
drop if flag2==1

// gsort subjid visit

*drop pregnancies missing delivery date unless still ongoing (n=17)
keep if preg_out==4 | (inlist(preg_out,1,2,3,90) & deldt!=.)

*drop any pregnancies with a delivery date that occurred before HIV dx
// gen flag3=1 if age_del<agedxc & age_del!=. & agedxc!=.
// drop if age_del<agedxc & age_del!=. & agedxc!=.

gen flag3=1 if deldt<diagdtn & deldt!=. & diagdtn!=.
drop if flag3==1

*drop those with missing HIV diagnosis date
drop if diagdtn==.

*drop those with missing delivery date
drop if deldt==.

*how many pregnancies total?
tab preg_out nbirth, missing
tab preg_out, missing
	
	*among how many women?
	bysort subjid: gen seq_w=_n
	tab seq_w, missing

	*age at enrollment among the n=618 women
	tabstat enroll_age if seq_w==1, stats (p25 p50 p75 mean sd n)
	
	*age at first pregnancy among the n=618 women
	tabstat agev if seq_w==1, stats (p25 p50 p75 mean sd n)
	
*keep singleton births (n=7 multiple births dropped)
drop if nbirth==2|nbirth==3

*frequencies of all pregnancy outcomes
tab preg_out, missing

*keep singleton live births
keep if preg_out==1

bysort subjid: gen n=_n
tab n

tab studypreg

*additional data categorization -----
xtile delivery_year_quartile = deldt_yr, n(4)
tab deldt_yr delivery_year_quartile

label def delivery_year_quartile 1 "1994-2010" 2 "2011-2015" 3 "2016-2018" 4 "2019-2023"
label val delivery_year_quartile delivery_year_quartile
label var delivery_year_quartile "Year of delivery (quartiles)"

*gravida and parity
xtile gravida_quartile = gravida, n(4)
tab gravida gravida_quartile

label def gravida_quartile 1 "1-2" 2 "3" 3 "4-5" 4 "6+"
label val gravida_quartile gravida_quartile
label var gravida_quartile "Number of pregnancies (gravida)"

xtile para_quartile = para, n(4)
tab para para_quartile

label def para_quartile 1 "1-2" 2 "3" 3 "4-5" 4 "6+"
label val para_quartile para_cat
label var para_quartile "Number of live births (parity)"

*maternal age at delivery categories
gen matage=1 if age_del<20 & age_del!=.
replace matage=2 if age_del>=20 & age_del<35
replace matage=3 if age_del>=35 & age_del!=.

label def matage 1 "Under 20" 2 "20-34" 3 "35+"
label val matage matage
label var matage "Age at delivery (years)"

*range of delivery dates
tab deldt

/*****************************************************************************
ANALYSIS TABLES
*****************************************************************************/
*table 1
// table1_mc, by(studypreg) vars(progid cat \ matage cat \ delivery_year_quartile cat \ edu_cat cat \ era2 cat \ gravida conts \ gravida_quartile cat \ para conts \ para_quartile cat \ pmtct cat \ pmtct_period cat \ flsupx cat \ iptrxyn cat \ infantfeed cat \ mom_hivstatus cat \ mom_hivstatus2 cat \ mom_art cat \ d_skill cat \ d_place cat \ delivmet cat \ def_hiv cat \ infant_art cat \ prior_preterm cat \ gest37 cat \ chldmort cat) onecol test missing total(b) format(%2.1f) sav("pmtct_table1.xlsx", replace)

table1_mc, by(country) vars(studypreg cat \ matage cat \ delivery_year_quartile cat \ edu_cat cat \ era2 cat \ gravida conts \ gravida_quartile cat \ para conts \ para_quartile cat \ pmtct cat \ pmtct_period cat \ flsupx cat \ iptrxyn cat \ infantfeed cat \ mom_hivstatus cat \ mom_hivstatus2 cat \ mom_art cat \ d_skill cat \ d_place cat \ delivmet cat \ def_hiv cat \ infant_art cat \ prior_preterm cat \ gest37 cat \ chldmort cat) onecol test missing total(b) format(%2.1f) sav("pmtct_table1.xlsx", replace)

*create flag for complete cases
gen cc=1 if progid!=. & matage!=. & delivery_year_quartile!=. & edu_cat!=. & gravida!=. & para!=. & era2!=. & mom_hivstatus!=. & d_skill!=. & preterm!=.
tab cc

*table 2: preterm birth
tab hivflag if cc==1

logit preterm i.progid if cc==1, vce(cluster subjid) or
logit preterm ib2.matage if cc==1, vce(cluster subjid) or
logit preterm i.delivery_year_quartile if cc==1, vce(cluster subjid) or
logit preterm ib2.edu_cat if cc==1, vce(cluster subjid) or
logit preterm gravida if cc==1, vce(cluster subjid) or
logit preterm para if cc==1, vce(cluster subjid) or
logit preterm ib2.era2 if cc==1, vce(cluster subjid) or
logit preterm i.mom_hivstatus if cc==1, vce(cluster subjid) or
logit preterm ib1.d_skill if cc==1, vce(cluster subjid) or
// logit preterm i.prior_preterm if cc==1, vce(cluster subjid) or /* some cell counts = 0, model does not converge */

*fully adjusted model
logit preterm i.progid ib2.matage i.delivery_year_quartile ib2.edu_cat gravida para ib2.era2 i.mom_hivstatus ib1.d_skill if cc==1, vce(cluster subjid) or

*try backward stepwise selection
stepwise, pr(.2) pe(.1): logit preterm (i.progid) (ib2.matage) (i.delivery_year_quartile) (ib2.edu_cat) gravida para (ib2.era2) (i.mom_hivstatus) (ib1.d_skill) if cc==1, vce(cluster subjid) or

*force program site in the model
stepwise, pr(.2) pe(.1) lockterm1: logit preterm (i.progid) (ib2.matage) (i.delivery_year_quartile) (ib2.edu_cat) gravida para (ib2.era2) (i.mom_hivstatus) (ib1.d_skill) if cc==1, vce(cluster subjid) or

*alternate approach - mixed effects logistic regression with a random intercept by participant, get essentially same results
// melogit preterm i.progid if cc==1 || subjid:, or

*table 3: infant mortality (within 12 months)
tab chldmort, missing
tab chldmort if cc==1, missing
tab hivflag if cc==1 & (chldmort==0|chldmort==1)

*recode infant ART variable due to small cell counts
gen infant_anyart=0 if infant_art2==1
replace infant_anyart=1 if infant_art2==2 | infant_art2==3 | infant_art2==4 | infant_art2==0

logit chldmort i.progid if cc==1 & (chldmort==0|chldmort==1), vce(cluster subjid) or
logit chldmort ib2.matage if cc==1 & (chldmort==0|chldmort==1), vce(cluster subjid) or
logit chldmort i.delivery_year_quartile if cc==1 & (chldmort==0|chldmort==1), vce(cluster subjid) or
logit chldmort ib2.edu_cat if cc==1 & (chldmort==0|chldmort==1), vce(cluster subjid) or
logit chldmort gravida if cc==1 & (chldmort==0|chldmort==1), vce(cluster subjid) or
logit chldmort para if cc==1 & (chldmort==0|chldmort==1), vce(cluster subjid) or
logit chldmort ib2.era2 if cc==1 & (chldmort==0|chldmort==1), vce(cluster subjid) or
logit chldmort i.mom_hivstatus if cc==1 & (chldmort==0|chldmort==1), vce(cluster subjid) or
logit chldmort ib1.d_skill if cc==1 & (chldmort==0|chldmort==1), vce(cluster subjid) or
logit chldmort i.preterm if cc==1 & (chldmort==0|chldmort==1), vce(cluster subjid) or
// logit chldmort i.infant_art2 if cc==1 & (chldmort==0|chldmort==1), vce(cluster subjid) or /* some cell counts = 0, model does not converge */
logit chldmort ib1.infant_anyart if cc==1 & (chldmort==0|chldmort==1), vce(cluster subjid) or

*fully adjusted model
logit chldmort i.progid ib2.matage i.delivery_year_quartile ib2.edu_cat gravida para ib2.era2 i.mom_hivstatus ib1.d_skill i.preterm i.infant_anyart if cc==1 & (chldmort==0|chldmort==1), vce(cluster subjid) or

*try backward stepwise selection
stepwise, pr(.2) pe(.1): logit chldmort (i.progid) (ib2.matage) (i.delivery_year_quartile) (ib2.edu_cat) gravida para (ib2.era2) (i.mom_hivstatus) (ib1.d_skill) (i.preterm) (ib1.infant_anyart) if cc==1 & (chldmort==0|chldmort==1), vce(cluster subjid) or

*force program site in the model
stepwise, pr(.2) pe(.1) lockterm1: logit chldmort (i.progid) (ib2.matage) (i.delivery_year_quartile) (ib2.edu_cat) gravida para (ib2.era2) (i.mom_hivstatus) (ib1.d_skill) (i.preterm) (ib1.infant_anyart) if cc==1 & (chldmort==0|chldmort==1), vce(cluster subjid) or

*alternate approach - mixed effects logistic regression with a random intercept by participant, get essentially same results
// melogit chldmort i.progid if cc==1 & (chldmort==0|chldmort==1) || subjid:, or

*look at associations between covariates
pwcorr gravida para if cc==1, obs sig star(0.05)
tab matage del_year_cat if cc==1, chi2
tab era2 del_year_cat if cc==1, chi2 exact
tab era2 mom_hivstatus if cc==1, chi2 exact
tab del_year_cat mom_hivstatus if cc==1, chi2 exact
tab del_year_cat infant_anyart if cc==1, chi2 exact
tab mom_hivstatus infant_anyart if cc==1, chi2 exact

ranksum gravida, by(preterm)
ranksum para, by(preterm)
