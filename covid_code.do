/***********************************************************
Program: covid_code
Date: 6 November 2020
Author: Nicole Dear
Data in: covid_14dec2020.dta
Data out:
***********************************************************/
cd "C:\Users\ndear\Box Sync\Shared Files- Reed and Esber\covid-19\Brief"

// use covid_4apr2021.dta, clear
// keep if visitdt<=date("28feb2021","DMY")

use covid_25jul2021.dta, clear

/*
*C01-0629 seroconverted between pre and post covid visits -- considered HIV- for purpose of analysis since no pre covid HIV variables would have been available, will footnote in manuscript (**ended up dropping from analysis bc this ppt had missing age)
replace hivflag=2 if subjid=="C01-0629"
*/

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

*replace those with missing hivflag
replace hivflag=1 if visit==1 & hivflag==. & hivstat==1

drop if visit>50
drop if misvis==1

*check for duplicates
duplicates tag subjid visit, gen(dup_id)
tab dup_id

list subjid visit visitdt fdanna gender hivflag sequence vl drawper if dup_id==1

drop if subjid=="A01-0011" & visit==16 & sequence==2
drop if subjid=="A01-0030" & visit==16 & sequence==2
drop if subjid=="A01-0152" & visit==15 & sequence==2
drop if subjid=="A01-0204" & visit==14 & sequence==2
drop if subjid=="A01-0210" & visit==14 & sequence==3
drop if subjid=="A01-0225" & visit==14 & sequence==2
drop if subjid=="A01-0226" & visit==13 & sequence==2
drop if subjid=="A01-0312" & visit==12 & sequence==2
drop if subjid=="A01-0346" & visit==12 & sequence==2
drop if subjid=="A01-0401" & visit==11 & sequence==2
drop if subjid=="B01-0045" & visit==12 & datestamp==date("29sep2020","DMY")
// drop if subjid=="B04-0094" & visit==12 & sequence==2

duplicates tag subjid visit, gen(dup_id2)
tab dup_id2

drop usubjid-age50 art_sdtn_first-nodata version-dur_wd

*create pre/post covid flag
gsort subjid visit
bysort subjid: carryforward progid, replace
bysort subjid: carryforward site_id, replace

*fill in site and progid
replace progid=1 if strpos(subjid, "A")
replace progid=2 if strpos(subjid, "B")
replace progid=3 if strpos(subjid, "C")
replace progid=4 if strpos(subjid, "D")
replace progid=5 if strpos(subjid, "E")

replace site_id="A01" if strpos(subjid, "A01")
replace site_id="B01" if strpos(subjid, "B01")
replace site_id="B02" if strpos(subjid, "B02")
replace site_id="B03" if strpos(subjid, "B03")
replace site_id="B04" if strpos(subjid, "B04")
replace site_id="B05" if strpos(subjid, "B05")
replace site_id="B06" if strpos(subjid, "B06")
replace site_id="C01" if strpos(subjid, "C01")
replace site_id="D01" if strpos(subjid, "D01")
replace site_id="D02" if strpos(subjid, "D02")
replace site_id="E01" if strpos(subjid, "E01")
replace site_id="E03" if strpos(subjid, "E03")

*******
*data cleaning/categorzing/labeling
gsort subjid visit
bysort subjid: carryforward progid, replace
bysort subjid: carryforward site_id, replace
bysort subjid: carryforward dobdtn, replace
bysort subjid: carryforward diagdtn, replace
bysort subjid: carryforward art_sdtn, replace
bysort subjid: carryforward howfrkm, replace
bysort subjid: carryforward educat, replace
bysort subjid: carryforward marital, replace
// bysort subjid: carryforward hivflag, replace
// bysort subjid: carryforward hivstat, replace
bysort subjid: carryforward gender, replace

label def fdanna 12 "abbreviated phone visit" 13 "abbreviated in-person visit"
label val fdanna fdanna

label def hivflag 1 "PLWH" 2 "HIV-uninfected"
label val hivflag hivflag
label var hivflag "HIV status at enrollment"
label var hivstat "HIV status at visit"

label define progid 1 "Kayunga, Uganda" 2 "South Rift Valley, Kenya" 3 "Kisumu West, Kenya" 4 "Mbeya, Tanzania" 5 "Abuja & Lagos Nigeria"
label val progid progid
label var progid "Site"

gen country=1 if progid==1
replace country=2 if progid==2|progid==3
replace country=3 if progid==4
replace country=4 if progid==5
label define country 1 "Uganda" 2 "Kenya" 3 "Tanzania" 4 "Nigeria"
label val country country

label def gender 1 "Male" 2 "Female"
label val gender gender
label var gender "Sex"

replace age=(visitdt-dobdtn)/365.25 if age==.
gen agec=0 if age<30
replace agec=1 if age>=30 & age<40
replace agec=2 if age>=40 & age<50
replace agec=3 if age>=50 & age!=.
label def agec 0 "<30" 1 "30-39" 2 "40-49" 3 "50+"
label val agec agec
label var agec "Age (years)"

gen agec2=0 if age<30
replace agec2=1 if age>=30 & age!=.
label def agec2 0 "<30" 1 "30+"
label val agec2 agec2

gen edu_cat=0 if educat==0 | educat==1
replace edu_cat=1 if educat==3 | educat==2
replace edu_cat=2 if educat>3 & educat!=.
label define edu_cat 0 "None or some primary" 1 "Primary or some secondary" 2 "Secondary and above"
label val edu_cat edu_cat
label var edu_cat "Education"

gen married=1 if marital==2
replace married=0 if marital!=2 & marital!=.
label define married 1 "Married" 0 "Not married"
label val married married
label var married "Marital status"

gen distance=0 if howfrkm<10
replace distance=1 if howfrkm>=10 & howfrkm!=.
label def distance 0 "<10 km" 1 "10+ km"
label val distance distance 
label var distance "Distance from clinic (KM)"

gen distance25=0 if howfrkm<25
replace distance25=1 if howfrkm>=25 & howfrkm!=.
label def distance25 0 "<25 km" 1 "25+ km"
label val distance25 distance25 
label var distance25 "Distance from clinic (KM)"

label def yn 0 "No" 1 "Yes"
label val takearv yn
label val depress yn
label var depress "Depressed (CES-D score>=16)"

*income by quartile (per country)
// gen country=1 if progid==1
// replace country=2 if progid==2 | progid==3
// replace country=3 if progid==4
// replace country=4 if progid==5
//
// replace hhincome=. if hhincome==7 | hhincome==90
// egen income_4tile = xtile(hhincome), n(4) by(country)

*PTSD: positive ptsd is a score of 3+ (add # yes responses out of the 4 PTSD questions)
// replace nightm=. if nightm==8
// replace avoid=. if avoid==8
// replace onguard=. if onguard==8
// replace detached=. if detached==8
//
// egen ptsd_sum = rowtotal(nightm avoid onguard detached)
// gen ptsd=0 if ptsd_sum<3
// replace ptsd=1 if ptsd_sum>=3 & ptsd_sum!=.
// label def ptsd 0 "No PTSD" 1 "PTSD"
// label val ptsd ptsd
// label var ptsd "PTSD"

*mmd
gen mmd=0 if mo_art<3 & mo_art!=.
replace mmd=1 if mo_art>=3 & mo_art!=.
label val mmd yn

gen mmd6=0 if mo_art<6 & mo_art!=.
replace mmd6=1 if mo_art>=6 & mo_art!=.
label val mmd6 yn

*food security
gen reduced=0 if mealcut==0
replace reduced=1 if mealcut>=1 & mealcut!=.

label val employed yn
label val food yn
label val reduced yn

*missed HIV clinic visits
gen adherclinicvis=1 if (hlthmnum==0 & apptna!=1) | apptna==1
replace adherclinicvis=0 if hlthmnum>0 & hlthmnum!=. & apptna!=1
label def adherclinicvis 1 "No missed clinic visits" 0 "Missed 1+ clinic visits"
label val adherclinicvis adherclinicvis
label var adherclinicvis "Adherence to clinic visits (past 6 months)"

*ART adherence
gen adherent=1 if missarv==0
replace adherent=0 if missarv>=1 & missarv!=.
label define adherent 1 "No missed doses ART" 0 "Missed 1+ doses ART"
label val adherent adherent
label var adherent "ART Adherence (past 30 days)"

*ART adherence
gen adherent2=0 if takearv==0
replace adherent2=1 if missarv==0 & takearv==1
replace adherent2=2 if missarv>=1 & missarv!=. & takearv==1
label define adherent2  0 "not on ART" 1 "No missed doses ART" 2 "Missed 1+ doses ART"
label val adherent2 adherent2
label var adherent2 "ART Adherence (past 30 days)"

/*
*ART regimen
gen ART=1 if tenofovir==1 & lamivudine==1 & dolutegravir==1
replace ART=2 if tenofovir==1 & lamivudine==1 & efavirenz==1
replace ART=3 if zidovudine==1 & nevirapine==1 & lamivudine==1
replace ART=4 if zidovudine==1 & efavirenz==1 & lamivudine==1
replace ART=5 if tenofovir==1 & lamivudine==1 & nevirapine==1
replace ART=6 if abacavir==1 & lamivudine==1 & efavirenz==1
replace ART=7 if lopinavir==1 | atazanavir==1 | ritonavir==1
replace ART=8 if art_sdtn==.
replace ART=9 if art_sdtn!=. & ART==.
label def ART 1 "TLD" 2 "TLE" 3 "AZT/NVP/3TC" 4 "AZT/EFV/3TC" 5 "TDF/NVP/3TC" 6 "ABC/3TC/EFV" 7 "PI-based" 8 "naive" 9 "other"
label val ART ART

*checking to make sure everyone on TLD classified as tld
gen tld=0
replace tld=1 if dolutegravir==1 & tenofovir==1 & lamivudine==1

count if tld==1
count if tld==0 & strpos(arv_code, "J55")
count if tld==0 & strpos(arv_code, "J12") /*n=36 on DTG contained non-TLD regimen*/
count if tld==0 & strpos(medicat, "19. Tenofovir/Lamivudine/Dolutegravi")

list subjid visit visitdt arv_code medicat art_sdtn if tld==0 & strpos(arv_code, "J12")

/*reason for treatment interruption
A01-0626       2   22jan2020 --> discontinued, compliance
D01-0305      10   03mar2020 --> missing stop code
D01-0405       8   04nov2019 --> MOH recommended regimen change
E01-0033      12   03sep2019 --> discontinued, compliance*/
*/

*viral load
replace dur_art=(visitdt-art_sdtn)/365.25 if dur_art==. & art_sdtn!=.
gen vs=0 if vl>=1000 & vl!=. & dur_art>.5 & dur_art!=.
replace vs=1 if vl<1000 & dur_art>.5 & dur_art!=.
label def vs 0 "Failing" 1 "Suppressed"
label val vs vs

*viral failure
gen vf=1 if vl>=1000 & vl!=. & dur_art>.5 & dur_art!=.
replace vf=0 if vl<1000 & dur_art>.5 & dur_art!=.
label def vf 1 "Failing" 0 "Suppressed"
label val vf vf

*viral load
gen vllt1000=0 if takearv==0
replace vllt1000=1 if vl<1000 & takearv==1
replace vllt1000=2 if (vl>=1000 & vl!=.) & takearv==1
label def vllt1000 0 "not on ART" 1 "On ART, vl<1000" 2 "On ART, vl>=1000"
label val vllt1000 vllt1000

/*******
/*extra request from Dr Ake: how many developed either new viral failure or new low level viremia then characterize those folks - factors associated with losing viral suppression in covid context. Are there demographic trends?  Do they have reported medication nonadherence?  Clinic visits missed?  Food insecurity?  Access to multimonth dispensing?  Are they more likely to be on nonTLD regimens?*/

keep if hivflag==1
replace dur_art=(visitdt-art_sdtn)/365.25 if dur_art==. & art_sdtn!=.

*******
*viral load suppression status at last visit before covid
gen vllt200=1 if vl<200 & dur_art>.5 & dur_art!=. & precovid==1
replace vllt200=0 if vl>=200 & vl!=. & dur_art>.5 & dur_art!=. & precovid==1

*viral load suppression status at most recent post covid visit
gen vlcat=0 if vl<200 & dur_art>.5 & dur_art!=. & postcovid==1
replace vlcat=1 if vl>=200 & vl<1000 & dur_art>.5 & dur_art!=. & postcovid==1
replace vlcat=2 if vl>=1000 & vl!=. & dur_art>.5 & dur_art!=. & postcovid==1

gsort subjid visit
bysort subjid: carryforward vllt200, replace

keep if vllt200!=. & vlcat!=.
keep if postcovid==1

gen vlstatus=0 if vllt200==1 & vlcat==0
replace vlstatus=1 if vllt200==1 & vlcat==1
replace vlstatus=2 if vllt200==1 & vlcat==2
replace vlstatus=3 if vllt200==0 & vlcat==0
replace vlstatus=4 if vllt200==0 & vlcat==1
replace vlstatus=5 if vllt200==0 & vlcat==2

label def vlstatus 0 "<200 c/ml to <200 c/ml" 1 "<200 c/ml to LLV" 2 "<200 c/ml to >=1000 c/ml" 3 ">200 c/ml to <200 c/ml" 4 ">200 c/ml to LLV" 5 ">200 c/ml to >=1000 c/ml"
label val vlstatus vlstatus
tab vlstatus, missing

gen vlstatus2=0 if vllt200==1 & vlcat==0
replace vlstatus2=1 if vllt200==1 & (vlcat==1 | vlcat==2)
replace vlstatus2=2 if vllt200==0 & vlcat==0
replace vlstatus2=3 if vllt200==0 & (vlcat==1 | vlcat==2)

label def vlstatus2 0 "<200 c/ml to <200 c/ml" 1 "<200 c/ml to >=200 c/ml" 2 ">200 c/ml to <200 c/ml" 3 ">200 c/ml to >=200 c/ml"
label val vlstatus2 vlstatus2

tab vlstatus, missing
tab vlstatus2, missing

*characterize using data from most recent precovid visit
table1_mc if vlstatus<3, by(vlstatus) vars(progid cate\gender cate\agec cate\age conts\edu_cat cate\married cate\distance cate\depress cate\ptsd cate\food cate\reduced cate\misclinicvis cate\adherent cate\ART cate) onecol missing catrowperc sav("table1_vfllv.xlsx")

table1_mc if vlstatus<3, vars(progid cate\gender cate\agec cate\age conts\edu_cat cate\married cate\distance cate\depress cate\ptsd cate\food cate\reduced cate\misclinicvis cate\adherent cate\ART cate) onecol missing sav("table1_vfllv_all.xlsx")

table1_mc if vlstatus2<2, by(vlstatus2) vars(progid cate\gender cate\agec cate\age conts\edu_cat cate\married cate\distance cate\depress cate\ptsd cate\food cate\reduced cate\misclinicvis cate\adherent cate\ART cate) onecol missing catrowperc sav("table1_vf200.xlsx")

table1_mc if vlstatus2<2, vars(progid cate\gender cate\agec cate\age conts\edu_cat cate\married cate\distance cate\depress cate\ptsd cate\food cate\reduced cate\misclinicvis cate\adherent cate\ART cate) onecol missing sav("table1_vf200_all.xlsx")

*******updated to inlcude remained or became unsuppressed at 1000 copy level
*viral load suppression status at last visit before covid
gen vllt1000=1 if vl<1000 & dur_art>.5 & dur_art!=. & precovid==1
replace vllt1000=0 if vl>=1000 & vl!=. & dur_art>.5 & dur_art!=. & precovid==1

*viral load suppression status at most recent post covid visit
gen vlcat=0 if vl<1000 & dur_art>.5 & dur_art!=. & postcovid==1
replace vlcat=1 if vl>=1000 & vl!=. & dur_art>.5 & dur_art!=. & postcovid==1

gsort subjid visit
bysort subjid: carryforward vllt1000, replace

keep if vllt1000!=. & vlcat!=.
keep if postcovid==1

gen vlstatus3=0 if vllt1000==1 & vlcat==0
replace vlstatus3=1 if vllt1000==1 & vlcat==1
replace vlstatus3=2 if vllt1000==0 & vlcat==0
replace vlstatus3=3 if vllt1000==0 & vlcat==1

label def vlstatus3 0 "<1000 c/ml to <1000 c/ml" 1 "<1000 c/ml to >=1000 c/ml" 2 ">=1000 c/ml to <1000 c/ml" 3 "remained >=1000 c/ml"
label val vlstatus3 vlstatus3

tab vlstatus3, missing

table1_mc if vlstatus3<2, by(vlstatus3) vars(progid cate\gender cate\agec cate\age conts\edu_cat cate\married cate\distance cate\depress cate\ptsd cate\food cate\reduced cate\misclinicvis cate\adherent cate\ART cate) onecol missing catrowperc sav("table1_vf1000.xlsx")

table1_mc if vlstatus3<2, vars(progid cate\gender cate\agec cate\age conts\edu_cat cate\married cate\distance cate\depress cate\ptsd cate\food cate\reduced cate\misclinicvis cate\adherent cate\ART cate) onecol missing sav("table1_vf1000_all.xlsx")

*******/
*create pre/post covid flag
gen covidera=0 if visitdt>=date("01jan2019","DMY") & visitdt<=date("19mar2020","DMY")
replace covidera=1 if ((visitdt>=date("26may2020","DMY") & progid==1)|(visitdt>=date("19jun2020","DMY") & progid==2 & site_id=="B01")|(visitdt>=date("23jun2020","DMY") & progid==2 & site_id!="B01")|(visitdt>=date("21oct2020","DMY") & progid==3)|(visitdt>=date("07may2020","DMY") & progid==4)|(visitdt>=date("08jun2020","DMY") & progid==5))

drop if visitdt>date("19mar2020","DMY") & ((visitdt<date("26may2020","DMY") & progid==1)|(visitdt<date("19jun2020","DMY") & progid==2 & site_id=="B01")|(visitdt<date("23jun2020","DMY") & progid==2 & site_id!="B01")|(visitdt<date("21oct2020","DMY") & progid==3)|(visitdt<date("07may2020","DMY") & progid==4)|(visitdt<date("08jun2020","DMY") & progid==5))

gen covidera_v2=0 if covidera==0
replace covidera_v2=1 if covidera==1 & visitdt<=date("07sep2020","DMY")
replace covidera_v2=2 if covidera==1 & visitdt>date("07sep2020","DMY") & visitdt<=date("28feb2021","DMY")
replace covidera_v2=3 if covidera==1 & visitdt>date("28feb2021","DMY")

label def covidera_v2 0 "Pre-covid" 1 "7 May to 7 Sep 2020" 2 "8 Sep 2020 to 28 Feb 2021" 3 "1 Mar to 25 July 2021"
label val covidera_v2 covidera_v2

*******
*flag most recent pre and post covid visits
gsort subjid -visit
bysort subjid: gen n1=_n

gen pre=1 if covidera==0
gen post=1 if covidera==1

by subjid (n1), sort: gen byte precovid = sum(pre)==1 & sum(pre[_n - 1])==0
by subjid (n1), sort: gen byte postcovid = sum(post)==1 & sum(post[_n - 1])==0

*restrict to those who had a last visit on or after 1 Jan 2019
gsort subjid -visit
gen lastvisit=visitdt if n1==1
bysort subjid: carryforward lastvisit, replace
gsort subjid visit 
bysort subjid: carryforward lastvisit, replace
format lastvisit %d
keep if lastvisit>=date("01jan2019","DMY") & lastvisit!=.

tab postcovid hivflag, row
tab precovid hivflag, row /*Between 1 January 2019 and 28 February 2021, 2666 AFRICOS participants completed at least one visit and were considered actively engaged with the study, including 2280 PLWH and 386 participants without HIV*/

*keep those with pre and post covid visits
// keep if post==1 | precovid==1
keep if post==1 | pre==1
gsort subjid -visit
bysort subjid: carryforward post, gen(postcovidvisit)
gsort subjid visit
bysort subjid: carryforward pre, gen(precovidvisit)
keep if postcovidvisit==1 & precovidvisit==1

**drop visits with missing covariate data (participant C01-0629)
list subjid visit visitdt dobdtn if hivflag==. | gender==. | agec==. | progid==.
keep if hivflag!=. & gender!=. & agec!=. & progid!=.

gsort subjid -visit
bysort subjid: gen n2=_n
tab n2 covidera if hivflag==1, missing /*n=1447 PLWH including n=447 with >1 post covid visits*/
tab n2 covidera if hivflag==2, missing /*n=228 HIV-uninfected ppts including n=74 with >1 post covid visits*/

*table 1
table1 if precovid==1, by(hivflag) vars(progid cat\country cat\gender cat\agec cat\agec2 cat\age conts\edu_cat cat\married cate\distance cat\employed cat\food cat\reduced cat) onecol missing sav("table1_cid.xlsx")

*******/
*restrict to those with viral loads post covid
// gen pre_vl_avail=1 if vs!=. & pre==1
// gen post_vl_avail=1 if vs!=. & post==1
//
// gsort subjid -visit
// bysort subjid: carryforward post_vl_avail, replace
//
// gsort subjid visit
// bysort subjid: carryforward pre_vl_avail, replace
//
// keep if pre_vl_avail==1 & post_vl_avail==1

*******/
/*
*split covidera into early vs late
tabstat visitdt if covidera==1, stat(mean median) format(%d)

gen covidera_v2=0 if covidera==0
replace covidera_v2=1 if covidera==1
replace covidera_v2=2 if covidera==1 & visitdt>date("07sep2020","DMY")

label def covidera_v2 0 "Pre-covid" 1 "Early COVID-19" 2 "Late COVID-19"
label val covidera_v2 covidera_v2
*/

**models
egen pin = group(subjid)

*potential confounders
xtgee covidera i.gender, i(pin) family(bin) link(logit) corr(ind) robust ef nolog
xtgee covidera i.progid, i(pin) family(bin) link(logit) corr(ind) robust ef nolog
xtgee covidera i.agec, i(pin) family(bin) link(logit) corr(ind) robust ef nolog
xtgee covidera i.employed, i(pin) family(bin) link(logit) corr(ind) robust ef nolog
xtgee covidera i.adherent if hivflag==1, i(pin) family(bin) link(logit) corr(ind) robust ef nolog

*PLWH - version 1, unadjusted
xtgee adherent i.covidera if hivflag==1, i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee adherent i.gender if hivflag==1, i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee adherent i.agec if hivflag==1, i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee adherent i.progid if hivflag==1, i(pin) family(bin) link(logit) corr(exch) robust ef nolog

xtgee vs i.covidera if hivflag==1, i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee vs i.gender if hivflag==1, i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee vs i.agec if hivflag==1, i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee vs i.progid if hivflag==1, i(pin) family(bin) link(logit) corr(exch) robust ef nolog

xtgee adherclinicvis i.covidera if hivflag==1, i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee adherclinicvis i.gender if hivflag==1, i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee adherclinicvis i.agec if hivflag==1, i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee adherclinicvis i.progid if hivflag==1, i(pin) family(bin) link(logit) corr(exch) robust ef nolog

xtgee food i.covidera if hivflag==1, i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee food i.gender if hivflag==1, i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee food i.agec if hivflag==1, i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee food i.progid if hivflag==1, i(pin) family(bin) link(logit) corr(exch) robust ef nolog

xtgee reduced i.covidera if hivflag==1, i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee reduced i.gender if hivflag==1, i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee reduced i.agec if hivflag==1, i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee reduced i.progid if hivflag==1, i(pin) family(bin) link(logit) corr(exch) robust ef nolog

*PLWH - version 1, adjusted
xtgee adherent i.covidera i.gender i.agec i.progid if hivflag==1, i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee vs i.covidera i.gender i.agec i.progid if hivflag==1, i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee adherclinicvis i.covidera i.gender i.agec i.progid if hivflag==1, i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee food i.covidera i.gender i.agec i.progid if hivflag==1, i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee reduced i.covidera i.gender i.agec i.progid if hivflag==1, i(pin) family(bin) link(logit) corr(exch) robust ef nolog

*PLWH - version 2, unadjusted
xtgee adherent i.covidera_v2 if hivflag==1, i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee vs i.covidera_v2 if hivflag==1, i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee adherclinicvis i.covidera_v2 if hivflag==1, i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee food i.covidera_v2 if hivflag==1, i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee reduced i.covidera_v2 if hivflag==1, i(pin) family(bin) link(logit) corr(exch) robust ef nolog

*PLWH - version 2, adjusted
xtgee adherent i.covidera_v2 i.gender i.agec i.progid if hivflag==1, i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee vs i.covidera_v2 i.gender i.agec i.progid if hivflag==1, i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee adherclinicvis i.covidera_v2 i.gender i.agec i.progid if hivflag==1, i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee food i.covidera_v2 i.gender i.agec i.progid if hivflag==1, i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee reduced i.covidera_v2 i.gender i.agec i.progid if hivflag==1, i(pin) family(bin) link(logit) corr(exch) robust ef nolog

*HIV-uninfected - version 1, unadjusted
xtgee food i.covidera if hivflag==2, i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee food i.gender if hivflag==2, i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee food i.agec if hivflag==2, i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee food i.progid if hivflag==2, i(pin) family(bin) link(logit) corr(exch) robust ef nolog

xtgee reduced i.covidera if hivflag==2, i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee reduced i.gender if hivflag==2, i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee reduced i.agec if hivflag==2, i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee reduced i.progid if hivflag==2, i(pin) family(bin) link(logit) corr(exch) robust ef nolog

*HIV-uninfected - version 1, adjusted
xtgee food i.covidera i.gender i.agec i.progid if hivflag==2, i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee reduced i.covidera i.gender i.agec i.progid if hivflag==2, i(pin) family(bin) link(logit) corr(exch) robust ef nolog

*HIV-uninfected - version 2, unadjusted
xtgee food i.covidera_v2 if hivflag==2, i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee reduced i.covidera_v2 if hivflag==2, i(pin) family(bin) link(logit) corr(exch) robust ef nolog

*HIV-uninfected - version 2, adjusted
xtgee food i.covidera_v2 i.gender i.agec i.progid if hivflag==2, i(pin) family(bin) link(logit) corr(exch) robust ef nolog
xtgee reduced i.covidera_v2 i.gender i.agec i.progid if hivflag==2, i(pin) family(bin) link(logit) corr(exch) robust ef nolog