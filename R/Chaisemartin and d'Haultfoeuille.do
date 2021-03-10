  ***** 
* Clustered std error analysis, ACA, DIDM
*****
cls
clear	
set more off
set trace off
set matsize 10000

if c(username)=="bngla" {
	gl data "\Users\bngla\Dropbox\PhD requirements\Nonemployer data\Data"
	gl output "\Users\bngla\Dropbox\PhD requirements\ACA and alt\Output"
    gl do "\Users\bngla\Dropbox\PhD requirements\ACA and alt"
	gl home "\Users\bngla\Dropbox\PhD requirements\ACA and alt"
}
if c(username)=="bglasner" {
	gl data "\Users\bglasner\Dropbox\PhD requirements\Nonemployer data\Data"
	gl output "\Users\bglasner\Dropbox\PhD requirements\ACA and alt\Output"
    gl do "\Users\bglasner\Dropbox\PhD requirements\ACA and alt"
	gl home "\Users\bglasner\Dropbox\PhD requirements\ACA and alt"
}
*ssc install did_multiplegt
*net install cleanplots, from("https://tdmize.github.io/data/cleanplots")
set scheme cleanplots, perm
gl total = 0
gl all = 1

if $total ==1{
import delimited "$data/medicaid_analysis_data_total.csv", clear
*   0 # "uber Not Active"
*   1 # "Uber Active"
label define ubers 0 "No Uber"  1 "Uber Active" 
label values uber_active ubers
label variable uber_active "Is Uber Active in the County?"


did_multiplegt estab_pop id_numeric years medicaid,  ///
				placebo(8) ///
				breps(100) ///
				controls() ///
				cluster(st) ///
				weight(lf_avg) 
ereturn list
		graph export "$output\estab_chaisemartin_total.pdf", replace
		graph close
		
did_multiplegt receipt_estab id_numeric years medicaid,   ///
				placebo(8) ///
				breps(100) ///
				controls() ///
				cluster(st) ///
				weight(lf_avg)  
ereturn list
		graph export "$output\receipt_chaisemartin_total.pdf", replace
		graph close
		
		}
		
if $all ==1{

 import delimited "$data/medicaid_analysis_data_total.csv", clear
*   0 # "uber Not Active"
*   1 # "Uber Active"
label define ubers 0 "No Uber"  1 "Uber Active" 
label values uber_active ubers
label variable uber_active "Is Uber Active in the County?"


did_multiplegt estab_pop id_numeric years medicaid,  ///
				placebo(8) ///
				breps(100) ///
				controls() ///
				cluster(st) ///
				weight(lf_avg) 
ereturn list
		graph export "$output\estab_chaisemartin_total.pdf", replace
		graph close
		
did_multiplegt receipt_estab id_numeric years medicaid,   ///
				placebo(8) ///
				breps(100) ///
				controls() ///
				cluster(st) ///
				weight(lf_avg)  
ereturn list
		graph export "$output\receipt_chaisemartin_total.pdf", replace
		graph close

import delimited "$data/medicaid_analysis_data_allnonemployers.csv", clear
gl industries 00 11 21 22 23 24 31-33 42 48-49 51 52 53 54 56 61 62 71 72 81

 preserve
 drop if naics != "11"
 did_multiplegt estab_pop id_numeric years medicaid,    ///
				placebo(8) ///
				breps(100) ///
				controls() ///
				cluster(st) ///
				weight(lf_avg)  
		ereturn list

did_multiplegt receipt_estab id_numeric years medicaid,     ///
				placebo(8) ///
				breps(100) ///
				controls() ///
				cluster(st) ///
				weight(lf_avg)  
		ereturn list
restore


 preserve
 drop if naics != "21"
 did_multiplegt estab_pop id_numeric years medicaid,    ///
				placebo(8) ///
				breps(100) ///
				controls() ///
				cluster(st) ///
				weight(lf_avg)  
		ereturn list

did_multiplegt receipt_estab id_numeric years medicaid,    ///
				placebo(8) ///
				breps(100) ///
				controls() ///
				cluster(st) ///
				weight(lf_avg)  
		ereturn list
		
restore

 preserve
 drop if naics != "22"
 did_multiplegt estab_pop id_numeric years medicaid,    ///
				placebo(8) ///
				breps(100) ///
				controls() ///
				cluster(st) ///
				weight(lf_avg)  
		ereturn list

did_multiplegt receipt_estab id_numeric years medicaid,    ///
				placebo(8) ///
				breps(100) ///
				controls() ///
				cluster(st) ///
				weight(lf_avg)  
		ereturn list
		
restore

 preserve
 drop if naics != "23"
 did_multiplegt estab_pop id_numeric years medicaid,   ///
				placebo(8) ///
				breps(100) ///
				controls() ///
				cluster(st) ///
				weight(lf_avg)  
		ereturn list

did_multiplegt receipt_estab id_numeric years medicaid,    ///
				placebo(8) ///
				breps(100) ///
				controls() ///
				cluster(st) ///
				weight(lf_avg)  
		ereturn list
		
restore

 preserve
 drop if naics != "31-33"
 did_multiplegt estab_pop id_numeric years medicaid,   ///
				placebo(8) ///
				breps(100) ///
				controls() ///
				cluster(st) ///
				weight(lf_avg)  
		ereturn list

did_multiplegt receipt_estab id_numeric years medicaid,    ///
				placebo(8) ///
				breps(100) ///
				controls() ///
				cluster(st) ///
				weight(lf_avg)  
		ereturn list
		
restore

 preserve
 drop if naics != "42"
 did_multiplegt estab_pop id_numeric years medicaid,    ///
				placebo(8) ///
				breps(100) ///
				controls() ///
				cluster(st) ///
				weight(lf_avg)  
		ereturn list

did_multiplegt receipt_estab id_numeric years medicaid,    ///
				placebo(8) ///
				breps(100) ///
				controls() ///
				cluster(st) ///
				weight(lf_avg)  
		ereturn list
		
restore

 preserve
 drop if naics != "44-45"
 did_multiplegt estab_pop id_numeric years medicaid,    ///
				placebo(8) ///
				breps(100) ///
				controls() ///
				cluster(st) ///
				weight(lf_avg)  
		ereturn list

did_multiplegt receipt_estab id_numeric years medicaid,     ///
				placebo(8) ///
				breps(100) ///
				controls() ///
				cluster(st) ///
				weight(lf_avg)  
		ereturn list
		
restore

 preserve
 drop if naics != "48-49"
 did_multiplegt estab_pop id_numeric years medicaid,    ///
				placebo(8) ///
				breps(100) ///
				controls() ///
				cluster(st) ///
				weight(lf_avg)  
		ereturn list

did_multiplegt receipt_estab id_numeric years medicaid,    ///
				placebo(8) ///
				breps(100) ///
				controls() ///
				cluster(st) ///
				weight(lf_avg)  
		ereturn list
		
restore

 preserve
 drop if naics != "51"
 did_multiplegt estab_pop id_numeric years medicaid,    ///
				placebo(8) ///
				breps(100) ///
				controls() ///
				cluster(st) ///
				weight(lf_avg)  
		ereturn list

did_multiplegt receipt_estab id_numeric years medicaid,    ///
				placebo(8) ///
				breps(100) ///
				controls() ///
				cluster(st) ///
				weight(lf_avg)  
		ereturn list
		
restore

 preserve
 drop if naics != "52"
 did_multiplegt estab_pop id_numeric years medicaid,   ///
				placebo(8) ///
				breps(100) ///
				controls() ///
				cluster(st) ///
				weight(lf_avg)  
		ereturn list

did_multiplegt receipt_estab id_numeric years medicaid,    ///
				placebo(8) ///
				breps(100) ///
				controls() ///
				cluster(st) ///
				weight(lf_avg)  
		ereturn list
		
restore

 preserve
 drop if naics != "53"
 did_multiplegt estab_pop id_numeric years medicaid,    ///
				placebo(8) ///
				breps(100) ///
				controls() ///
				cluster(st) ///
				weight(lf_avg)  
		ereturn list

did_multiplegt receipt_estab id_numeric years medicaid,    ///
				placebo(8) ///
				breps(100) ///
				controls() ///
				cluster(st) ///
				weight(lf_avg)  
		ereturn list
		
restore

 preserve
 drop if naics != "54"
 did_multiplegt estab_pop id_numeric years medicaid,    ///
				placebo(8) ///
				breps(100) ///
				controls() ///
				cluster(st) ///
				weight(lf_avg)  
		ereturn list

did_multiplegt receipt_estab id_numeric years medicaid,    ///
				placebo(8) ///
				breps(100) ///
				controls() ///
				cluster(st) ///
				weight(lf_avg)  
		ereturn list
		
restore

 preserve
 drop if naics != "56"
 did_multiplegt estab_pop id_numeric years medicaid,    ///
				placebo(8) ///
				breps(100) ///
				controls() ///
				cluster(st) ///
				weight(lf_avg)  
		ereturn list

did_multiplegt receipt_estab id_numeric years medicaid,    ///
				placebo(8) ///
				breps(100) ///
				controls() ///
				cluster(st) ///
				weight(lf_avg)  
		ereturn list
		
restore

 preserve
 drop if naics != "61"
 did_multiplegt estab_pop id_numeric years medicaid,    ///
				placebo(8) ///
				breps(100) ///
				controls() ///
				cluster(st) ///
				weight(lf_avg)  
		ereturn list

did_multiplegt receipt_estab id_numeric years medicaid,    ///
				placebo(8) ///
				breps(100) ///
				controls() ///
				cluster(st) ///
				weight(lf_avg)  
		ereturn list
		
restore

 preserve
 drop if naics != "62"
 did_multiplegt estab_pop id_numeric years medicaid,   ///
				placebo(8) ///
				breps(100) ///
				controls() ///
				cluster(st) ///
				weight(lf_avg)  
		ereturn list

did_multiplegt receipt_estab id_numeric years medicaid,   ///
				placebo(8) ///
				breps(100) ///
				controls() ///
				cluster(st) ///
				weight(lf_avg)  
		ereturn list
		
restore

 preserve
 drop if naics != "71"
 did_multiplegt estab_pop id_numeric years medicaid,   ///
				placebo(8) ///
				breps(100) ///
				controls() ///
				cluster(st) ///
				weight(lf_avg)  
		ereturn list

did_multiplegt receipt_estab id_numeric years medicaid,   ///
				placebo(8) ///
				breps(100) ///
				controls() ///
				cluster(st) ///
				weight(lf_avg)  
		ereturn list
		
restore

 preserve
 drop if naics != "72"
 did_multiplegt estab_pop id_numeric years medicaid,   ///
				placebo(8) ///
				breps(100) ///
				controls() ///
				cluster(st) ///
				weight(lf_avg)  
		ereturn list

did_multiplegt receipt_estab id_numeric years medicaid,   ///
				placebo(8) ///
				breps(100) ///
				controls() ///
				cluster(st) ///
				weight(lf_avg)  
		ereturn list
		
restore

 preserve
 drop if naics != "81"
 did_multiplegt estab_pop id_numeric years medicaid,   ///
				placebo(8) ///
				breps(100) ///
				controls() ///
				cluster(st) ///
				weight(lf_avg)  
		ereturn list

did_multiplegt receipt_estab id_numeric years medicaid,   ///
				placebo(8) ///
				breps(100) ///
				controls() ///
				cluster(st) ///
				weight(lf_avg)  
		ereturn list
		
restore
		}
