

libname data 'C:\Users\...\Mobility_data';



PROC IMPORT OUT= data DATAFILE= "C:\Users\...\Mobility_data\KDI_Mobility.xlsx" 
            DBMS=xlsx REPLACE;
     SHEET="KDI2018"; 
     GETNAMES=YES;
RUN;


data mob_01 (keep=sq1 sq2 sq3 sq4 sq5 sq6                /* demgraphics */
                  sq10                                   /* health */ 
                  q1 q2 q3 q5_1 q5_2 q5_3 q5_4 q5_2 q5_3 q5_4 q5_5 q5_6 q5_7 q5_8 q5_9 q5_10 q5_11        /* satisfaction */
                  q6_1 q6_2 q6_3 q6_4 q6_5 q6_6 q6_7 q6_8 q6_9 q6_10 q6_11 q6_12 q6_13   /* anxeity */
				  q16 q17 q18 q18_1                      /* reference */    
                  q19_1 q19_2 q19_3 q20 q21              /* mobility */
                  qc1 qc1_1 qc1_2                        /* suicidal ideology */
				  dq3_1 dq3_2 dq3_2_1                    /* location */
				  age edu1 edu2 inc_home1 inc_home2 area1 area2 

				  health
				  
); 
set data; run;



/******************    CODE BOOK   ******************

sq1: sex   sq2: age  sq3: add_c  sq4: add_o
q1: happy_c q2: happy_p  q3: happy_f 
q5_1: sat_inc q5_2: sat_occu q5_3: sat_edu q5_4: sat_care q5_5: sat_hous q5_6: sat_env q5_7: sat_family q5_8: sat_neighborhood q5_9: sat_participation q5_10: sat_culture q5_11: sat_safety        
q6_1: anx_inc q6_2: anx_occu q6_3: anx_edu q6_4: sat_care q6_5: anx_hous q6_6: anx_env q6_7: anx_family q6_8: anx_neighborhood q6_9: anx_participation q6_10: anx_culture q6_11: axn_safety
q16: ref_inc  q17: ref_happy q18: ref_other_inc  q18_1: ref_people 
q19_1: ind_class q19_2: parent_class  q19_3:  q20: expectation_upward q21: expectation_downward
qc1: retire qc1_1: anx_retire qc1_2: choice_retire qc2: suicide qc2_1: suicide_reason
dq3_1: address_metro dq3_2: address_micro dq3_2_1: address_code
age edu1 edu2 inc_home1 inc_home2 area1 area2 
health
				  
*****************************************************/


/* Recategorizing variables */

data mob_02 ; set data;

mon=q19_1-q19_2;
if mon > 0 then moc = 'up';
if mon = 0 then moc = 'st';
if mon < 0 then moc = 'do';


if q1 <6 then hap ='0';
else if q1 > 5 then hap ='1';

if sq10 > 3 then health_w = '1';
else health_w = '0';

if sq10 < 3 then health_b = '1';
else health_b = '0';

if sq10=5 then sq10i=1;
if sq10=4 then sq10i=2;
if sq10=3 then sq10i=3;
if sq10=2 then sq10i=4;
if sq10=1 then sq10i=5;

if sq2 > 59 then aggrp = '60s';
else if 59 >= sq2 > 49 then aggrp = '50s';
else if 49 >= sq2 > 39 then aggrp = '40s';
else if 39 >= sq2 > 29 then aggrp = '30s';
else if sq2 < 31 then aggrp = '20s';


if sq6 =< 2 then edui = 'mid';
else if sq6 >3 then edui = 'col';
else edui = 'hi';


if sq15 < 4 or sq15 = 9 then pi = 'L';
else if 3 < sq15 < 6 then pi = 'M';
else if sq15 > 5 then pi = 'H';


if sq16 < 4 then hinc = 'L';
else if 3 < sq16 < 6 then hinc = 'M';
else if sq16 > 5 then hinc = 'H';


if sq3 in (1) then rlgi = 'C'; /* Capital */
else if sq3 in (5. 9) then rlgi = 'M'; /* Metro */
else if sq4 in (1) then rlgi = 'L';
else rlgi = 'S';


if sq7 in (1) then mari = 'M';
else if sq7 in (2) then mari = 'N';
else if sq7 in (3) then mari = 'D';
else mari = 'O';
/*if sq2 = 2 then age=1;*/


run;

proc univariate data=mob_02; var q1;histogram; run;

proc sort data=mob_02; by moc; run;
proc freq data=mob_02; table sq10; by moc; where sq2 > 60; run;
proc freq data=mob_02; table sq10; by moc; where 60 > sq2 > 50; run;
proc freq data=mob_02; table sq10; by moc; where 50 > sq2 > 40; run;
proc freq data=mob_02; table sq10; by moc; where 40 > sq2 > 30; run;
proc freq data=mob_02; table sq10; by moc; where sq2 < 31; run;


/* Physical - All - HHIC */

/** Final model 1 **/
proc logistic data=mob_02 descending;
class moc (ref='st') aggrp (ref='20s') sq1(ref='1') edui (ref='col') hinc (ref='H') rlgi (ref='C') mari (ref='M')/ param=ref;
model health_b = moc sq1 aggrp edui hinc rlgi mari;
run;

proc logistic data=mob_02 descending;
class moc (ref='st') aggrp (ref='20s') edui (ref='col') hinc (ref='H') rlgi (ref='C') mari (ref='M')/ param=ref;
model health_w = moc aggrp edui hinc rlgi mari;
run;

/** Final model 2 **/
proc logistic data=mob_02 descending;
class moc (ref='st') aggrp (ref='30s') sq1(ref='1') edui (ref='col') hinc (ref='H') rlgi (ref='C') mari (ref='M')/ param=ref;
model health_b = moc sq1 aggrp edui hinc rlgi mari;
where 30 <= sq2 < 60;
run;

proc logistic data=mob_02 descending;
class moc (ref='st') aggrp (ref='30s') edui (ref='col') hinc (ref='H') rlgi (ref='C') mari (ref='M')/ param=ref;
model health_w = moc aggrp edui hinc rlgi mari;
where 31 <= sq2 < 61;
run;

proc logistic data=mob_02 descending;
class moc (ref='st') edui (ref='col') hinc (ref='H') rlgi (ref='C') mari (ref='M')/ param=ref;
model health_b = moc edui hinc rlgi mari;
where 60 < sq2;
run;

proc logistic data=mob_02 descending;
class moc (ref='st') edui (ref='col') hinc (ref='H') rlgi (ref='C') mari (ref='M')/ param=ref;
model health_w = moc edui hinc rlgi mari;
where 60 < sq2;
run;

proc logistic data=mob_02 descending;
class moc (ref='st') edui (ref='col') hinc (ref='H') rlgi (ref='C') mari (ref='M')/ param=ref;
model health_b = moc edui hinc rlgi mari;
where 50 <= sq2 < 60;
run;

proc logistic data=mob_02 descending;
class moc (ref='st') edui (ref='col') hinc (ref='H') rlgi (ref='C') mari (ref='M')/ param=ref;
model health_w = moc edui hinc rlgi mari;
where 51 <= sq2 < 61;
run;

/** Final model 3 **/
proc logistic data=mob_02 descending;
class moc (ref='st') edui (ref='col') sq1(ref='1') hinc (ref='H') rlgi (ref='C') mari (ref='M')/ param=ref;
model health_b = moc sq1 edui hinc rlgi mari;
where 40 <= sq2 < 50;
run;

proc logistic data=mob_02 descending;
class moc (ref='st') edui (ref='col') hinc (ref='H') rlgi (ref='C') mari (ref='M')/ param=ref;
model health_w = moc edui hinc rlgi mari;
where 41 <= sq2 < 51;
run;

proc logistic data=mob_02 descending;
class moc (ref='st') edui (ref='col') hinc (ref='H') rlgi (ref='C') mari (ref='M')/ param=ref;
model health_b = moc edui hinc rlgi mari;
where 31 <= sq2 < 41;
run;

proc logistic data=mob_02 descending;
class moc (ref='st') edui (ref='col') hinc (ref='H') rlgi (ref='C') mari (ref='M')/ param=ref;
model health_w = moc edui hinc rlgi mari;
where 31 <= sq2 < 41;
run;

proc logistic data=mob_02 descending;
class moc (ref='st') edui (ref='col') hinc (ref='H') rlgi (ref='C') mari (ref='M')/ param=ref;
model health_b = moc edui hinc rlgi mari;
where sq2 < 31;
run;

proc logistic data=mob_02 descending;
class moc (ref='st') edui (ref='col') hinc (ref='H') rlgi (ref='C') mari (ref='M')/ param=ref;
model health_w = moc edui hinc rlgi mari;
where sq2 < 31;
run;


/* No age effect */
proc logistic data=mob_02 descending;
class moc (ref='st') aggrp (ref='30s') edui (ref='col') hinc (ref='H') rlgi (ref='C') mari (ref='M')/ param=ref;
model hlth = moc edui hinc rlgi mari;
run;


proc logistic data=mob_02 descending;
class moc (ref='st') edui (ref='col') hinc (ref='H') rlgi (ref='C') mari (ref='M')/ param=ref;
model health_b = moc edui hinc rlgi mari;
where 31 <= sq2;
run;

proc logistic data=mob_02 descending;
class moc (ref='st') edui (ref='col') hinc (ref='H') rlgi (ref='C') mari (ref='M')/ param=ref;
model health_w = moc edui hinc rlgi mari;
where 31 <= sq2;
run;

proc logistic data=mob_02 descending;
class moc (ref='st') edui (ref='col') hinc (ref='H') rlgi (ref='C') mari (ref='M')/ param=ref;
model health_b = moc edui hinc rlgi mari;
where 41 <= sq2;
run;

proc logistic data=mob_02 descending;
class moc (ref='st') edui (ref='col') hinc (ref='H') rlgi (ref='C') mari (ref='M')/ param=ref;
model health_w = moc edui hinc rlgi mari;
where 41 <= sq2;
run;

proc logistic data=mob_02 descending;
class moc (ref='st') edui (ref='col') hinc (ref='H') rlgi (ref='C') mari (ref='M')/ param=ref;
model health_b = moc edui hinc rlgi mari;
where 51 <= sq2;
run;

proc logistic data=mob_02 descending;
class moc (ref='st') edui (ref='col') hinc (ref='H') rlgi (ref='C') mari (ref='M')/ param=ref;
model health_w = moc edui hinc rlgi mari;
where 51 <= sq2;
run;


proc freq data=mob_02; table sq10; by moc; run;



/* Physical - All - PI */
proc logistic data=mob_02 descending;
class moc (ref='st') aggrp (ref='20s') edui (ref='col') pi (ref='H') rlgi (ref='C') mari (ref='M')/ param=ref;
model health_b = moc sq1 aggrp edui pi rlgi mari;
run;

proc logistic data=mob_02 descending;
class moc (ref='st') aggrp (ref='20s') edui (ref='col') pi (ref='H') rlgi (ref='C') mari (ref='M')/ param=ref;
model health_w = moc sq1 aggrp edui pi rlgi mari;
run;

proc logistic data=mob_02 descending; /* No Sig */
class moc (ref='st') aggrp (ref='30s') edui (ref='col') pi (ref='H') rlgi (ref='C') mari (ref='M')/ param=ref;
model health_b = moc sq1 aggrp edui pi rlgi mari;
where 31 <= sq2 < 61;
run;

proc logistic data=mob_02 descending; /* No Sig */
class moc (ref='st') aggrp (ref='30s') edui (ref='col') pi (ref='H') rlgi (ref='C') mari (ref='M')/ param=ref;
model health_w = moc sq1 aggrp edui pi rlgi mari;
where 31 <= sq2 < 61;
run;

proc logistic data=mob_02 descending;
class moc (ref='st') edui (ref='col') pi (ref='H') rlgi (ref='C') mari (ref='M')/ param=ref;
model health_b = moc sq1 edui pi rlgi mari;
where sq2 >60;
run;

proc logistic data=mob_02 descending;
class moc (ref='st') edui (ref='col') pi (ref='H') rlgi (ref='C') mari (ref='M')/ param=ref;
model health_w = moc sq1 edui pi rlgi mari;
where sq2 >60;
run;

proc logistic data=mob_02 descending; /* Sig */
class moc (ref='st') aggrp (ref='50s') edui (ref='col') pi (ref='H') rlgi (ref='C') mari (ref='M')/ param=ref;
model health_b = moc sq1 edui pi rlgi mari;
where sq2 > 50;
run;

proc logistic data=mob_02 descending; /* Sig */
class moc (ref='st') aggrp (ref='50s') edui (ref='col') pi (ref='H') rlgi (ref='C') mari (ref='M')/ param=ref;
model health_w = moc sq1 edui pi rlgi mari;
where sq2 > 50;
run;

proc logistic data=mob_02 descending; /* Sig */
class moc (ref='st') aggrp (ref='40s') edui (ref='col') pi (ref='H') rlgi (ref='C') mari (ref='M')/ param=ref;
model health_b = moc sq1 edui pi rlgi mari;
where sq2 > 40;
run;

proc logistic data=mob_02 descending; /* Sig */
class moc (ref='st') aggrp (ref='40s') edui (ref='col') pi (ref='H') rlgi (ref='C') mari (ref='M')/ param=ref;
model health_w = moc sq1 edui pi rlgi mari;
where sq2 > 40;
run;

proc logistic data=mob_02 descending; 
class moc (ref='st') aggrp (ref='30s') edui (ref='col') pi (ref='H') rlgi (ref='C') mari (ref='M')/ param=ref;
model health_b = moc sq1 aggrp edui pi rlgi mari;
where 31 <= sq2;
run;

proc logistic data=mob_02 descending; 
class moc (ref='st') aggrp (ref='30s') edui (ref='col') pi (ref='H') rlgi (ref='C') mari (ref='M')/ param=ref;
model health_w = moc sq1 aggrp edui pi rlgi mari;
where 31 <= sq2;
run;

proc logistic data=mob_02 descending; /* No Sig */
class moc (ref='st') edui (ref='col') pi (ref='H') rlgi (ref='C') mari (ref='M')/ param=ref;
model health_b = moc sq1 edui pi rlgi mari;
where 51 <= sq2 < 60;
run;

proc logistic data=mob_02 descending; /* No Sig */
class moc (ref='st') edui (ref='col') pi (ref='H') rlgi (ref='C') mari (ref='M')/ param=ref;
model health_w = moc sq1 edui pi rlgi mari;
where 51 <= sq2 < 60;
run;

proc logistic data=mob_02 descending; /* No Sig */
class moc (ref='st') edui (ref='col') pi (ref='H') rlgi (ref='C') mari (ref='M')/ param=ref;
model health_b = moc sq1 edui pi rlgi mari;
where 41 <= sq2 < 50;
run;

proc logistic data=mob_02 descending; /* No Sig */
class moc (ref='st') edui (ref='col') pi (ref='H') rlgi (ref='C') mari (ref='M')/ param=ref;
model health_w = moc sq1 edui pi rlgi mari;
where 41 <= sq2 < 50;
run;

proc logistic data=mob_02 descending; /* No Sig */
class moc (ref='st') edui (ref='col') pi (ref='H') rlgi (ref='C') mari (ref='M')/ param=ref;
model health_b = moc sq1 edui pi rlgi mari;
where 31 <= sq2 < 40;
run;

proc logistic data=mob_02 descending; /* No Sig */
class moc (ref='st') edui (ref='col') pi (ref='H') rlgi (ref='C') mari (ref='M')/ param=ref;
model health_w = moc sq1 edui pi rlgi mari;
where 31 <= sq2 < 40;
run;

proc logistic data=mob_02 descending; /* No Sig */
class moc (ref='st') edui (ref='col') pi (ref='H') rlgi (ref='C') mari (ref='M')/ param=ref;
model health_b = moc sq1 edui pi rlgi mari;
where sq2 <31 ;
run;

proc logistic data=mob_02 descending; /* No Sig */
class moc (ref='st') edui (ref='col') pi (ref='H') rlgi (ref='C') mari (ref='M')/ param=ref;
model health_w = moc sq1 edui pi rlgi mari;
where sq2 <31 ;
run;




/* Happiness - HHI */

/** Final model 1 **/
proc logistic data=mob_02 descending;
class moc (ref='st') aggrp (ref='20s') sq1(ref='1') edui (ref='col') hinc (ref='H') rlgi (ref='C') mari (ref='M')/ param=ref;
model hap = moc sq1 aggrp edui hinc rlgi mari;
run;

/** Final model 2 **/
proc logistic data=mob_02 descending;
class moc (ref='st') aggrp (ref='30s') sq1(ref='1') edui (ref='col') hinc (ref='H') rlgi (ref='C') mari (ref='M')/ param=ref;
model hap = moc sq1 aggrp edui hinc rlgi mari;
where 30 <= sq2 < 60;
run;

proc logistic data=mob_02 descending;
class moc (ref='st') aggrp (ref='40s') sq1(ref='1') edui (ref='col') hinc (ref='H') rlgi (ref='C') mari (ref='M')/ param=ref;
model hap = moc sq1 aggrp edui hinc rlgi mari;
where 40 <= sq2 < 60;
run;

/** Final model 3 **/
proc logistic data=mob_02 descending;
class moc (ref='st') edui (ref='col') sq1(ref='1') hinc (ref='H') rlgi (ref='C') mari (ref='M')/ param=ref;
model hap = moc sq1 edui hinc rlgi mari;
where 50 > sq2 > 39;
run;


proc logistic data=mob_02 descending;
class hap moc (ref='st') sq1 aggrp (ref='20s') edui hinc (ref='L') rlgi mari/ param=ref;
model hap = moc sq1 aggrp edui hinc rlgi mari;
/*where sq2 > 59;*/
run;

proc logistic data=mob_02 descending; /* Sig */
class hap moc (ref='st') sq1 edui hinc (ref='L') rlgi mari/ param=ref;
model hap = moc sq1 edui hinc rlgi mari;
where 61 > sq2 > 30;
run;

proc logistic data=mob_02 descending; /* Sig */
class hap moc (ref='st') sq1 aggrp (ref='30s') edui hinc (ref='L') rlgi mari/ param=ref;
model hap = moc sq1 edui hinc rlgi mari;
where sq2 > 30;
run;

proc logistic data=mob_02 descending; /* Sig */
class hap moc (ref='st') sq1 aggrp (ref='40s') edui hinc (ref='L') rlgi mari/ param=ref;
model hap = moc sq1 edui hinc rlgi mari;
where sq2 > 40;
run;

proc logistic data=mob_02 descending; /* Not Sig */
class hap moc (ref='st') sq1 aggrp (ref='50s') edui hinc (ref='L') rlgi mari/ param=ref;
model hap = moc sq1 edui hinc rlgi mari;
where sq2 > 50;
run;

proc logistic data=mob_02 descending; /* Not Sig */
class hap moc (ref='st') sq1 edui hinc (ref='L') rlgi mari/ param=ref;
model hap = moc sq1 edui hinc rlgi mari;
where sq2 > 59;
run;

proc logistic data=mob_02 descending; /* Not Sig */
class hap moc (ref='st') sq1 edui hinc (ref='L') rlgi mari/ param=ref;
model hap = moc sq1 edui hinc rlgi mari;
where 60 > sq2 > 49;
run;

proc logistic data=mob_02 descending; /* Sig */
class hap moc (ref='st') sq1 edui hinc (ref='L') rlgi mari/ param=ref;
model hap = moc sq1 edui hinc rlgi mari;
where 51 > sq2 > 40;
run;

proc logistic data=mob_02 descending; /* Not Sig */
class hap moc (ref='st') sq1 edui hinc (ref='L') rlgi mari/ param=ref;
model hap = moc sq1 edui hinc rlgi mari;
where 40 > sq2 > 29;
run;

proc logistic data=mob_02 descending; /* Not Sig */
class hap moc (ref='st') sq1 edui hinc (ref='L') rlgi mari/ param=ref;
model hap = moc sq1 edui hinc rlgi mari;
where 30 > sq2;
run;

/* Happiness - PI */

proc logistic data=mob_02 descending;
class hap moc (ref='st') sq1 aggrp (ref='20s') edui pi (ref='L') rlgi mari/ param=ref;
model hap = moc sq1 aggrp edui pi rlgi mari;
/*where sq2 > 59;*/
run;

proc logistic data=mob_02 descending; /* Sig */
class hap moc (ref='st') sq1 aggrp (ref='30s') edui pi (ref='L') rlgi mari/ param=ref;
model hap = moc sq1 edui pi rlgi mari;
where sq2 > 30;
run;

proc logistic data=mob_02 descending; /* Sig */
class hap moc (ref='st') sq1 aggrp (ref='40s') edui pi (ref='L') rlgi mari/ param=ref;
model hap = moc sq1 edui pi rlgi mari;
where sq2 > 40;
run;

proc logistic data=mob_02 descending; /* Not Sig */
class hap moc (ref='st') sq1 aggrp (ref='50s') edui pi (ref='L') rlgi mari/ param=ref;
model hap = moc sq1 edui pi rlgi mari;
where sq2 > 50;
run;

proc logistic data=mob_02 descending; /* Not Sig */
class hap moc (ref='st') sq1 edui pi (ref='L') rlgi mari/ param=ref;
model hap = moc sq1 edui pi rlgi mari;
where sq2 > 60;
run;

proc logistic data=mob_02 descending; /* Not Sig */
class hap moc (ref='st') sq1 edui pi (ref='L') rlgi mari/ param=ref;
model hap = moc sq1 edui pi rlgi mari;
where 61 > sq2 > 30;
run;

proc logistic data=mob_02 descending; /* Not Sig */
class hap moc (ref='st') sq1 edui pi (ref='L') rlgi mari/ param=ref;
model hap = moc sq1 edui pi rlgi mari;
where 61 > sq2 > 50;
run;

proc logistic data=mob_02 descending; /* Sig */
class hap moc (ref='st') sq1 edui pi (ref='L') rlgi mari/ param=ref;
model hap = moc sq1 edui pi rlgi mari;
where 51 > sq2 > 40;
run;

proc logistic data=mob_02 descending; /* Not Sig */
class hap moc (ref='st') sq1 edui pi (ref='L') rlgi mari/ param=ref;
model hap = moc sq1 edui pi rlgi mari;
where 41 > sq2 > 30;
run;

proc logistic data=mob_02 descending; /* Not Sig */
class hap moc (ref='st') sq1 edui pi (ref='L') rlgi mari/ param=ref;
model hap = moc sq1 edui pi rlgi mari;
where 31 > sq2;
run;




/* Desc Stats */
proc freq data=mob_02; tables sq1 sq4 sq6 sq10 moc;
run;

proc means data=mob_02; var sq2; run;



/* Mobility By age group */

proc freq data=mob_02; tables moc; run;
proc freq data=mob_02; tables moc; where 60 > sq2 > 29; run;
proc freq data=mob_02; tables moc; where 30 > sq2; run;
proc freq data=mob_02; tables moc; where 40 > sq2 > 29; run;
proc freq data=mob_02; tables moc; where 50 > sq2 > 39; run;
proc freq data=mob_02; tables moc; where 60 > sq2 > 49; run;
proc freq data=mob_02; tables moc; where sq2 > 59; run;


proc sort data=mob_02; by aggrp; run;

proc freq data=mob_02; tables moc; by aggrp; run;

proc means data=mob_02; var mon q19_1 q12_2; run;
proc means data=mob_02; var mon q19_1 q12_2; by aggrp; run;


/* Health By age group */
proc sort data=mob_02; by aggrp; run;

proc freq data=mob_02; tables sq10; by aggrp; run;
proc freq data=mob_02; tables hap; by aggrp; run;

proc means data=mob_02; var sq10; run;
proc means data=mob_02; var sq10; by aggrp; run;

proc means data=mob_02; var q1; run;
proc means data=mob_02; var q1; by aggrp; run;


proc sort data=mob_02; by aggrp; run;
proc means data=mob_02; var sq10; by aggrp; where moc ='up'; run;
proc means data=mob_02; var sq10; by aggrp; where moc ='st'; run;
proc means data=mob_02; var sq10; by aggrp; where moc ='do'; run;


/* For table */

/* Self-rated health */
proc means data=mob_02; var sq10; run;
proc means data=mob_02; var sq10; where moc ='up'; run;
proc means data=mob_02; var sq10; where moc ='st'; run;
proc means data=mob_02; var sq10; where moc ='do'; run;

proc means data=mob_02; var sq10; where 61 > sq2 > 30; run;
proc means data=mob_02; var sq10; where moc ='up' and 61 > sq2 > 30; run;
proc means data=mob_02; var sq10; where moc ='st' and 61 > sq2 > 30; run;
proc means data=mob_02; var sq10; where moc ='do' and 61 > sq2 > 30; run;

proc means data=mob_02; var sq10; where sq2 > 60; run;
proc means data=mob_02; var sq10; where moc ='up' and sq2 > 60; run;
proc means data=mob_02; var sq10; where moc ='st' and sq2 > 60; run;
proc means data=mob_02; var sq10; where moc ='do' and sq2 > 60; run;

proc means data=mob_02; var sq10; where 51> sq2 > 40; run;
proc means data=mob_02; var sq10; where moc ='up' and 51> sq2 > 40; run;
proc means data=mob_02; var sq10; where moc ='st' and 51> sq2 > 40; run;
proc means data=mob_02; var sq10; where moc ='do' and 51> sq2 > 40; run;

/* Inversed */
proc means data=mob_02; var sq10i; run;
proc means data=mob_02; var sq10i; where moc ='up'; run;
proc means data=mob_02; var sq10i; where moc ='st'; run;
proc means data=mob_02; var sq10i; where moc ='do'; run;

proc means data=mob_02; var sq10i; where 60 > sq2 > 29; run;
proc means data=mob_02; var sq10i; where moc ='up' and 60 > sq2 > 29; run;
proc means data=mob_02; var sq10i; where moc ='st' and 60 > sq2 > 29; run;
proc means data=mob_02; var sq10i; where moc ='do' and 60 > sq2 > 29; run;

proc means data=mob_02; var sq10i; where sq2 > 59; run;
proc means data=mob_02; var sq10i; where moc ='up' and sq2 > 59; run;
proc means data=mob_02; var sq10i; where moc ='st' and sq2 > 59; run;
proc means data=mob_02; var sq10i; where moc ='do' and sq2 > 59; run;

proc means data=mob_02; var sq10i; where 60> sq2 > 49; run;
proc means data=mob_02; var sq10i; where moc ='up' and 60> sq2 > 49; run;
proc means data=mob_02; var sq10i; where moc ='st' and 60> sq2 > 49; run;
proc means data=mob_02; var sq10i; where moc ='do' and 60> sq2 > 49; run;

proc means data=mob_02; var sq10i; where 50> sq2 > 39; run;
proc means data=mob_02; var sq10i; where moc ='up' and 50> sq2 > 39; run;
proc means data=mob_02; var sq10i; where moc ='st' and 50> sq2 > 39; run;
proc means data=mob_02; var sq10i; where moc ='do' and 50> sq2 > 39; run;

proc means data=mob_02; var sq10i; where 40> sq2 > 29; run;
proc means data=mob_02; var sq10i; where moc ='up' and 40> sq2 > 29; run;
proc means data=mob_02; var sq10i; where moc ='st' and 40> sq2 > 29; run;
proc means data=mob_02; var sq10i; where moc ='do' and 40> sq2 > 29; run;

proc means data=mob_02; var sq10i; where sq2 < 30; run;
proc means data=mob_02; var sq10i; where moc ='up' and sq2 < 30; run;
proc means data=mob_02; var sq10i; where moc ='st' and sq2 < 30; run;
proc means data=mob_02; var sq10i; where moc ='do' and sq2 < 30; run;

proc freq data=mob_02; tables health_b; where moc ='up'; run;
proc freq data=mob_02; tables health_b; where moc ='st'; run;
proc freq data=mob_02; tables health_b; where moc ='do'; run;

proc freq data=mob_02; tables health_b; where moc ='up' and 30 < sq2 < 61; run;
proc freq data=mob_02; tables health_b; where moc ='st' and 30 < sq2 < 61; run;
proc freq data=mob_02; tables health_b; where moc ='do' and 30 < sq2 < 61; run;

proc freq data=mob_02; tables health_b; where moc ='up' and 60 < sq2; run;
proc freq data=mob_02; tables health_b; where moc ='st' and 60 < sq2; run;
proc freq data=mob_02; tables health_b; where moc ='do' and 60 < sq2; run;

proc freq data=mob_02; tables health_b; where moc ='up' and 40 < sq2 < 51; run;
proc freq data=mob_02; tables health_b; where moc ='st' and 40 < sq2 < 51; run;
proc freq data=mob_02; tables health_b; where moc ='do' and 40 < sq2 < 51; run;

proc freq data=mob_02; tables health_b; where moc ='up' and sq2 < 31; run;
proc freq data=mob_02; tables health_b; where moc ='st' and sq2 < 31; run;
proc freq data=mob_02; tables health_b; where moc ='do' and sq2 < 31; run;

/* Happiness */
proc means data=mob_02; var q1; run;
proc means data=mob_02; var q1; where moc ='up'; run;
proc means data=mob_02; var q1; where moc ='st'; run;
proc means data=mob_02; var q1; where moc ='do'; run;

proc means data=mob_02; var q1; where 60> sq2 > 29; run;
proc means data=mob_02; var q1; where moc ='up' and 60> sq2 > 29; run;
proc means data=mob_02; var q1; where moc ='st' and 60> sq2 > 29; run;
proc means data=mob_02; var q1; where moc ='do' and 60> sq2 > 29; run;

proc means data=mob_02; var q1; where sq2 > 60; run;
proc means data=mob_02; var q1; where moc ='up' and sq2 > 60; run;
proc means data=mob_02; var q1; where moc ='st' and sq2 > 60; run;
proc means data=mob_02; var q1; where moc ='do' and sq2 > 60; run;

proc means data=mob_02; var q1; where 61> sq2 > 50; run;
proc means data=mob_02; var q1; where moc ='up' and 61> sq2 > 50; run;
proc means data=mob_02; var q1; where moc ='st' and 61> sq2 > 50; run;
proc means data=mob_02; var q1; where moc ='do' and 61> sq2 > 50; run;

proc means data=mob_02; var q1; where 50> sq2 > 39; run;
proc means data=mob_02; var q1; where moc ='up' and 50> sq2 > 39; run;
proc means data=mob_02; var q1; where moc ='st' and 50> sq2 > 39; run;
proc means data=mob_02; var q1; where moc ='do' and 50> sq2 > 39; run;

proc means data=mob_02; var q1; where 41> sq2 > 30; run;
proc means data=mob_02; var q1; where moc ='up' and 41> sq2 > 30; run;
proc means data=mob_02; var q1; where moc ='st' and 41> sq2 > 30; run;
proc means data=mob_02; var q1; where moc ='do' and 41> sq2 > 30; run;

proc means data=mob_02; var q1; where 31> sq2 ; run;
proc means data=mob_02; var q1; where moc ='up' and 31> sq2; run;
proc means data=mob_02; var q1; where moc ='st' and 31> sq2; run;
proc means data=mob_02; var q1; where moc ='do' and 31> sq2; run;


proc freq data=mob_02; tables hap; where moc ='up' ; run;
proc freq data=mob_02; tables hap; where moc ='st' ; run;
proc freq data=mob_02; tables hap; where moc ='do' ; run;

proc freq data=mob_02; tables hap; where moc ='up' and 30 < sq2 < 61; run;
proc freq data=mob_02; tables hap; where moc ='st' and 30 < sq2 < 61; run;
proc freq data=mob_02; tables hap; where moc ='do' and 30 < sq2 < 61; run;

proc freq data=mob_02; tables hap; where moc ='up' and 60 < sq2; run;
proc freq data=mob_02; tables hap; where moc ='st' and 60 < sq2; run;
proc freq data=mob_02; tables hap; where moc ='do' and 60 < sq2; run;

proc freq data=mob_02; tables hap; where moc ='up' and 40 < sq2 < 51; run;
proc freq data=mob_02; tables hap; where moc ='st' and 40 < sq2 < 51; run;
proc freq data=mob_02; tables hap; where moc ='do' and 40 < sq2 < 51; run;

proc freq data=mob_02; tables hap; where moc ='up' and sq2 < 31; run;
proc freq data=mob_02; tables hap; where moc ='st' and sq2 < 31; run;
proc freq data=mob_02; tables hap; where moc ='do' and sq2 < 31; run;


/* Class-self */
proc means data=mob_02; var q19_1; run;
proc means data=mob_02; var q19_1; where moc ='up'; run;
proc means data=mob_02; var q19_1; where moc ='st'; run;
proc means data=mob_02; var q19_1; where moc ='do'; run;

proc means data=mob_02; var q19_1; where 60> sq2 > 29; run;
proc means data=mob_02; var q19_1; where moc ='up' and 60> sq2 > 29; run;
proc means data=mob_02; var q19_1; where moc ='st' and 60> sq2 > 29; run;
proc means data=mob_02; var q19_1; where moc ='do' and 60> sq2 > 29; run;

proc means data=mob_02; var q19_1; where sq2 > 59; run;
proc means data=mob_02; var q19_1; where moc ='up' and sq2 > 60; run;
proc means data=mob_02; var q19_1; where moc ='st' and sq2 > 60; run;
proc means data=mob_02; var q19_1; where moc ='do' and sq2 > 60; run;

proc means data=mob_02; var q19_1; where 50> sq2 > 39; run;
proc means data=mob_02; var q19_1; where moc ='up' and 50> sq2 > 39; run;
proc means data=mob_02; var q19_1; where moc ='st' and 50> sq2 > 39; run;
proc means data=mob_02; var q19_1; where moc ='do' and 50> sq2 > 39; run;

proc means data=mob_02; var q19_1; where 31> sq2; run;
proc means data=mob_02; var q19_1; where 30> sq2; run;
proc means data=mob_02; var q19_1; where 40> sq2 > 29; run;
proc means data=mob_02; var q19_1; where 50> sq2 > 39; run;
proc means data=mob_02; var q19_1; where 60> sq2 > 49; run;

/* Class-Parents */
proc means data=mob_02; var q19_2; run;
proc means data=mob_02; var q19_2; where moc ='up'; run;
proc means data=mob_02; var q19_2; where moc ='st'; run;
proc means data=mob_02; var q19_2; where moc ='do'; run;

proc means data=mob_02; var q19_2; where 60> sq2 > 29; run;
proc means data=mob_02; var q19_2; where moc ='up' and 60> sq2 > 29; run;
proc means data=mob_02; var q19_2; where moc ='st' and 60> sq2 > 29; run;
proc means data=mob_02; var q19_2; where moc ='do' and 60> sq2 > 29; run;

proc means data=mob_02; var q19_2; where sq2 > 60; run;
proc means data=mob_02; var q19_2; where moc ='up' and sq2 > 60; run;
proc means data=mob_02; var q19_2; where moc ='st' and sq2 > 60; run;
proc means data=mob_02; var q19_2; where moc ='do' and sq2 > 60; run;

proc means data=mob_02; var q19_2; where 50> sq2 > 39; run;
proc means data=mob_02; var q19_2; where moc ='up' and 50> sq2 > 39; run;
proc means data=mob_02; var q19_2; where moc ='st' and 50> sq2 > 39; run;
proc means data=mob_02; var q19_2; where moc ='do' and 50> sq2 > 39; run;


/* mobility */

proc means data=mob_02; var mon; run;
proc means data=mob_02; var mon; where moc ='up'; run;
proc means data=mob_02; var mon; where moc ='st'; run;
proc means data=mob_02; var mon; where moc ='do'; run;

proc means data=mob_02; var mon; where 60> sq2 > 29; run;
proc means data=mob_02; var mon; where moc ='up' and 60> sq2 > 29; run;
proc means data=mob_02; var mon; where moc ='st' and 60> sq2 > 29; run;
proc means data=mob_02; var mon; where moc ='do' and 60> sq2 > 29; run;

proc means data=mob_02; var mon; where sq2 > 59; run;
proc means data=mob_02; var mon; where moc ='up' and sq2 > 60; run;
proc means data=mob_02; var mon; where moc ='st' and sq2 > 60; run;
proc means data=mob_02; var mon; where moc ='do' and sq2 > 60; run;

proc means data=mob_02; var mon; where 50> sq2 > 39; run;
proc means data=mob_02; var mon; where moc ='up' and 50> sq2 > 39; run;
proc means data=mob_02; var mon; where moc ='st' and 50> sq2 > 39; run;
proc means data=mob_02; var mon; where moc ='do' and 50> sq2 > 39; run;

proc means data=mob_02; var mon; where 60> sq2 > 49; run;
proc means data=mob_02; var mon; where moc ='up' and 50> sq2 > 39; run;
proc means data=mob_02; var mon; where moc ='st' and 50> sq2 > 39; run;
proc means data=mob_02; var mon; where moc ='do' and 50> sq2 > 39; run;

proc means data=mob_02; var mon; where 40> sq2 > 29; run;

proc means data=mob_02; var mon; where 30> sq2; run;

/* Age */
proc means data=mob_02; var sq2; run;
proc means data=mob_02; var sq2; where moc ='up'; run;
proc means data=mob_02; var sq2; where moc ='st'; run;
proc means data=mob_02; var sq2; where moc ='do'; run;

proc means data=mob_02; var sq2; where 60> sq2 > 29; run;
proc means data=mob_02; var sq2; where moc ='up' and 60> sq2 > 29; run;
proc means data=mob_02; var sq2; where moc ='st' and 60> sq2 > 29; run;
proc means data=mob_02; var sq2; where moc ='do' and 60> sq2 > 29; run;

proc means data=mob_02; var sq2; where sq2 > 60; run;
proc means data=mob_02; var sq2; where moc ='up' and sq2 > 60; run;
proc means data=mob_02; var sq2; where moc ='st' and sq2 > 60; run;
proc means data=mob_02; var sq2; where moc ='do' and sq2 > 60; run;

proc means data=mob_02; var sq2; where 50> sq2 > 39; run;
proc means data=mob_02; var sq2; where moc ='up' and 50> sq2 > 39; run;
proc means data=mob_02; var sq2; where moc ='st' and 50> sq2 > 39; run;
proc means data=mob_02; var sq2; where moc ='do' and 50> sq2 > 39; run;


/* Gender */

proc freq data=mob_02; table sq1; run;
proc freq data=mob_02; table sq1; where moc ='up'; run;
proc freq data=mob_02; table sq1; where moc ='st'; run;
proc freq data=mob_02; table sq1; where moc ='do'; run;

proc freq data=mob_02; table sq1; where 60> sq2 > 29; run;
proc freq data=mob_02; table sq1; where moc ='up' and 60> sq2 > 29; run;
proc freq data=mob_02; table sq1; where moc ='st' and 60> sq2 > 29; run;
proc freq data=mob_02; table sq1; where moc ='do' and 60> sq2 > 29; run;

proc freq data=mob_02; table sq1; where sq2 > 60; run;
proc freq data=mob_02; table sq1; where moc ='up' and sq2 > 60; run;
proc freq data=mob_02; table sq1; where moc ='st' and sq2 > 60; run;
proc freq data=mob_02; table sq1; where moc ='do' and sq2 > 60; run;

proc freq data=mob_02; table sq1; where 50> sq2 > 39; run;
proc freq data=mob_02; table sq1; where moc ='up' and 50> sq2 > 39; run;
proc freq data=mob_02; table sq1; where moc ='st' and 50> sq2 > 39; run;
proc freq data=mob_02; table sq1; where moc ='do' and 50> sq2 > 39; run;


proc means data=mob_02; var sq1; run;
proc means data=mob_02; var sq1; where moc ='up'; run;
proc means data=mob_02; var sq1; where moc ='st'; run;
proc means data=mob_02; var sq1; where moc ='do'; run;

proc means data=mob_02; var sq1; where sq2 > 60; run;
proc means data=mob_02; var sq1; where moc ='up' and sq2 > 60; run;
proc means data=mob_02; var sq1; where moc ='st' and sq2 > 60; run;
proc means data=mob_02; var sq1; where moc ='do' and sq2 > 60; run;

proc means data=mob_02; var sq1; where 51> sq2 > 40; run;
proc means data=mob_02; var sq1; where moc ='up' and 51> sq2 > 40; run;
proc means data=mob_02; var sq1; where moc ='st' and 51> sq2 > 40; run;
proc means data=mob_02; var sq1; where moc ='do' and 51> sq2 > 40; run;


/* Education */
proc freq data=mob_02; table edui; run;
proc freq data=mob_02; table edui; where moc ='up'; run;
proc freq data=mob_02; table edui; where moc ='st'; run;
proc freq data=mob_02; table edui; where moc ='do'; run;

proc freq data=mob_02; table edui; where 29 < sq2 <60; run;
proc freq data=mob_02; table edui; where moc ='up' and 29 < sq2 <60; run;
proc freq data=mob_02; table edui; where moc ='st' and 29 < sq2 <60; run;
proc freq data=mob_02; table edui; where moc ='do' and 29 < sq2 <60; run;

proc freq data=mob_02; table edui; where 39 < sq2 <50; run;
proc freq data=mob_02; table edui; where moc ='up' and 39 < sq2 <50; run;
proc freq data=mob_02; table edui; where moc ='st' and 39 < sq2 <50; run;
proc freq data=mob_02; table edui; where moc ='do' and 39 < sq2 <50; run;


proc freq data=mob_02; table edui; where 61> sq2 > 30; run;
proc freq data=mob_02; table edui; where moc ='up' and 61> sq2 > 30; run;
proc freq data=mob_02; table edui; where moc ='st' and 61> sq2 > 30; run;
proc freq data=mob_02; table edui; where moc ='do' and 61> sq2 > 30; run;

proc freq data=mob_02; table edui; where sq2 > 59; run;
proc freq data=mob_02; table edui; where moc ='up' and sq2 > 60; run;
proc freq data=mob_02; table edui; where moc ='st' and sq2 > 60; run;
proc freq data=mob_02; table edui; where moc ='do' and sq2 > 60; run;

proc freq data=mob_02; table edui; where sq2 < 50; run;

proc freq data=mob_02; table edui; where 61> sq2 > 50; run;
proc freq data=mob_02; table edui; where moc ='up' and 61> sq2 > 50; run;
proc freq data=mob_02; table edui; where moc ='st' and 61> sq2 > 50; run;
proc freq data=mob_02; table edui; where moc ='do' and 61> sq2 > 50; run;

proc freq data=mob_02; table edui; where 50> sq2 > 39; run;
proc freq data=mob_02; table edui; where moc ='up' and 51> sq2 > 40; run;
proc freq data=mob_02; table edui; where moc ='st' and 51> sq2 > 40; run;
proc freq data=mob_02; table edui; where moc ='do' and 51> sq2 > 40; run;

proc freq data=mob_02; table edui; where 41> sq2 > 30; run;
proc freq data=mob_02; table edui; where moc ='up' and 41> sq2 > 30; run;
proc freq data=mob_02; table edui; where moc ='st' and 41> sq2 > 30; run;
proc freq data=mob_02; table edui; where moc ='do' and 41> sq2 > 30; run;

proc freq data=mob_02; table edui; where 30> sq2 ; run;
proc freq data=mob_02; table edui; where moc ='up' and 31> sq2; run;
proc freq data=mob_02; table edui; where moc ='st' and 31> sq2; run;
proc freq data=mob_02; table edui; where moc ='do' and 31> sq2; run;

proc freq data=mob_02; table moc; where edui ='mid' ; run;
proc freq data=mob_02; table moc; where edui ='hi' ; run;
proc freq data=mob_02; table moc; where edui ='col' ; run;

proc freq data=mob_02; table moc; where edui ='mid' and sq2 <30 ; run;
proc freq data=mob_02; table moc; where edui ='hi' and sq2 <30; run;
proc freq data=mob_02; table moc; where edui ='col' and sq2 <30 ; run;

proc freq data=mob_02; table moc; where edui ='mid' and 29 < sq2 <40 ; run;
proc freq data=mob_02; table moc; where edui ='hi' and 29 < sq2 <40; run;
proc freq data=mob_02; table moc; where edui ='col' and 29 <  sq2 <40 ; run;

proc freq data=mob_02; table moc; where edui ='mid' and 39 < sq2 <50 ; run;
proc freq data=mob_02; table moc; where edui ='hi' and 39 < sq2 <50; run;
proc freq data=mob_02; table moc; where edui ='col' and 39 <  sq2 <50 ; run;

proc freq data=mob_02; table moc; where edui ='mid' and 49 < sq2 < 60 ; run;
proc freq data=mob_02; table moc; where edui ='hi'  and 49 < sq2 < 60; run;
proc freq data=mob_02; table moc; where edui ='col' and 49 < sq2 < 60 ; run;

proc freq data=mob_02; table moc; where edui ='mid' and 59 < sq2 ; run;
proc freq data=mob_02; table moc; where edui ='hi'  and 59 < sq2 ; run;
proc freq data=mob_02; table moc; where edui ='col' and 59 < sq2 ; run;

/* Income */

/* PI */
proc freq data=mob_02; table sq15 sq16; by moc; run;

proc freq data=mob_02; table pi; run;
proc freq data=mob_02; table pi; where moc ='up'; run;
proc freq data=mob_02; table pi; where moc ='st'; run;
proc freq data=mob_02; table pi; where moc ='do'; run;

proc freq data=mob_02; table pi; where 29 < sq2 < 60; run;
proc freq data=mob_02; table pi; where moc ='up' and 29 < sq2 < 60; run;
proc freq data=mob_02; table pi; where moc ='st' and 29 < sq2 < 60; run;
proc freq data=mob_02; table pi; where moc ='do' and 29 < sq2 < 60; run;

proc freq data=mob_02; table pi; where sq2 > 60; run;
proc freq data=mob_02; table pi; where moc ='up' and sq2 > 60; run;
proc freq data=mob_02; table pi; where moc ='st' and sq2 > 60; run;
proc freq data=mob_02; table pi; where moc ='do' and sq2 > 60; run;

proc freq data=mob_02; table pi; where 61> sq2 > 50; run;
proc freq data=mob_02; table pi; where moc ='up' and 61> sq2 > 50; run;
proc freq data=mob_02; table pi; where moc ='st' and 61> sq2 > 50; run;
proc freq data=mob_02; table pi; where moc ='do' and 61> sq2 > 50; run;

proc freq data=mob_02; table pi; where 50> sq2 > 39; run;
proc freq data=mob_02; table pi; where moc ='up' and 50> sq2 > 39; run;
proc freq data=mob_02; table pi; where moc ='st' and 50> sq2 > 39; run;
proc freq data=mob_02; table pi; where moc ='do' and 50> sq2 > 39; run;

proc freq data=mob_02; table pi; where 41> sq2 > 30; run;
proc freq data=mob_02; table pi; where moc ='up' and 41> sq2 > 30; run;
proc freq data=mob_02; table pi; where moc ='st' and 41> sq2 > 30; run;
proc freq data=mob_02; table pi; where moc ='do' and 41> sq2 > 30; run;

proc freq data=mob_02; table pi; where 31> sq2; run;
proc freq data=mob_02; table pi; where moc ='up' and 31> sq2; run;
proc freq data=mob_02; table pi; where moc ='st' and 31> sq2; run;
proc freq data=mob_02; table pi; where moc ='do' and 31> sq2; run;

proc sort data=mob_02; by moc; run;
proc means data=mob_02; vars sq15 sq16; by moc; where 61> sq2 > 30; run;
proc means data=mob_02; vars sq15 sq16; by moc; where sq2 > 60; run;
proc means data=mob_02; vars sq15 sq16; by moc; where 61> sq2 > 50; run;
proc means data=mob_02; vars sq15 sq16; by moc; where 51> sq2 > 40; run;
proc means data=mob_02; vars sq15 sq16; by moc; where 41> sq2 > 30; run;
proc means data=mob_02; vars sq15 sq16; by moc; where 31> sq2 ; run;

/* by age group */
proc freq data=mob_02; table moc; where pi ='L' ; run;
proc freq data=mob_02; table moc; where pi ='M' ; run;
proc freq data=mob_02; table moc; where pi ='H' ; run;

proc freq data=mob_02; table moc; where pi ='L' and sq2 <30 ; run;
proc freq data=mob_02; table moc; where pi ='M' and sq2 <30; run;
proc freq data=mob_02; table moc; where pi ='H' and sq2 <30 ; run;

proc freq data=mob_02; table moc; where pi ='L' and 29 < sq2 <40 ; run;
proc freq data=mob_02; table moc; where pi ='M' and 29 < sq2 <40; run;
proc freq data=mob_02; table moc; where pi ='H' and 29 <  sq2 <40 ; run;

proc freq data=mob_02; table moc; where pi ='L' and 39 < sq2 <50 ; run;
proc freq data=mob_02; table moc; where pi ='M' and 39 < sq2 <50; run;
proc freq data=mob_02; table moc; where pi ='H' and 39 < sq2 <50 ; run;

proc freq data=mob_02; table moc; where pi ='L' and 49 < sq2 < 60 ; run;
proc freq data=mob_02; table moc; where pi ='M'  and 49 < sq2 < 60; run;
proc freq data=mob_02; table moc; where pi ='H' and 49 < sq2 < 60 ; run;

proc freq data=mob_02; table moc; where pi ='L' and 59 < sq2 ; run;
proc freq data=mob_02; table moc; where pi ='M'  and 59 < sq2 ; run;
proc freq data=mob_02; table moc; where pi ='H' and 59 < sq2 ; run;


/* HHI */

proc freq data=mob_02; table hinc; run;
proc freq data=mob_02; table hinc; where moc ='up'; run;
proc freq data=mob_02; table hinc; where moc ='st'; run;
proc freq data=mob_02; table hinc; where moc ='do'; run;

proc freq data=mob_02; table hinc; where 29 < sq2 < 60; run;
proc freq data=mob_02; table hinc; where moc ='up' and 29 < sq2 < 60; run;
proc freq data=mob_02; table hinc; where moc ='st' and 29 < sq2 < 60; run;
proc freq data=mob_02; table hinc; where moc ='do' and 29 < sq2 < 60; run;

proc freq data=mob_02; table hinc; where sq2 > 60; run;
proc freq data=mob_02; table hinc; where moc ='up' and sq2 > 60; run;
proc freq data=mob_02; table hinc; where moc ='st' and sq2 > 60; run;
proc freq data=mob_02; table hinc; where moc ='do' and sq2 > 60; run;

proc freq data=mob_02; table hinc; where 61> sq2 > 50; run;
proc freq data=mob_02; table hinc; where moc ='up' and 61> sq2 > 50; run;
proc freq data=mob_02; table hinc; where moc ='st' and 61> sq2 > 50; run;
proc freq data=mob_02; table hinc; where moc ='do' and 61> sq2 > 50; run;


proc freq data=mob_02; table hinc; where 50> sq2 > 39; run;
proc freq data=mob_02; table hinc; where moc ='up' and 50> sq2 > 39; run;
proc freq data=mob_02; table hinc; where moc ='st' and 50> sq2 > 39; run;
proc freq data=mob_02; table hinc; where moc ='do' and 50> sq2 > 39; run;


proc freq data=mob_02; table hinc; where 41> sq2 > 30; run;
proc freq data=mob_02; table hinc; where moc ='up' and 41> sq2 > 30; run;
proc freq data=mob_02; table hinc; where moc ='st' and 41> sq2 > 30; run;
proc freq data=mob_02; table hinc; where moc ='do' and 41> sq2 > 30; run;

proc freq data=mob_02; table hinc; where 31> sq2; run;
proc freq data=mob_02; table hinc; where moc ='up' and 31> sq2; run;
proc freq data=mob_02; table hinc; where moc ='st' and 31> sq2; run;
proc freq data=mob_02; table hinc; where moc ='do' and 31> sq2; run;


proc freq data=mob_02; table moc; where hinc ='L' ; run;
proc freq data=mob_02; table moc; where hinc ='M' ; run;
proc freq data=mob_02; table moc; where hinc ='H' ; run;

proc freq data=mob_02; table moc; where hinc ='L' and sq2 <30 ; run;
proc freq data=mob_02; table moc; where hinc ='M' and sq2 <30; run;
proc freq data=mob_02; table moc; where hinc ='H' and sq2 <30 ; run;

proc freq data=mob_02; table moc; where hinc ='L' and 29 < sq2 <40 ; run;
proc freq data=mob_02; table moc; where hinc ='M' and 29 < sq2 <40; run;
proc freq data=mob_02; table moc; where hinc ='H' and 29 <  sq2 <40 ; run;

proc freq data=mob_02; table moc; where hinc ='L' and 39 < sq2 <50 ; run;
proc freq data=mob_02; table moc; where hinc ='M' and 39 < sq2 <50; run;
proc freq data=mob_02; table moc; where hinc ='H' and 39 < sq2 <50 ; run;

proc freq data=mob_02; table moc; where hinc ='L' and 49 < sq2 < 60 ; run;
proc freq data=mob_02; table moc; where hinc ='M'  and 49 < sq2 < 60; run;
proc freq data=mob_02; table moc; where hinc ='H' and 49 < sq2 < 60 ; run;

proc freq data=mob_02; table moc; where hinc ='L' and 59 < sq2 ; run;
proc freq data=mob_02; table moc; where hinc ='M'  and 59 < sq2 ; run;
proc freq data=mob_02; table moc; where hinc ='H' and 59 < sq2 ; run;


/* Region */
proc freq data=mob_02; table rlgi; run;
proc freq data=mob_02; table rlgi; where moc ='up'; run;
proc freq data=mob_02; table rlgi; where moc ='st'; run;
proc freq data=mob_02; table rlgi; where moc ='do'; run;

proc freq data=mob_02; table rlgi; where 60> sq2 > 29; run;
proc freq data=mob_02; table rlgi; where moc ='up' and 60> sq2 > 29; run;
proc freq data=mob_02; table rlgi; where moc ='st' and 60> sq2 > 29; run;
proc freq data=mob_02; table rlgi; where moc ='do' and 60> sq2 > 29; run;

proc freq data=mob_02; table rlgi; where sq2 > 60; run;
proc freq data=mob_02; table rlgi; where moc ='up' and sq2 > 60; run;
proc freq data=mob_02; table rlgi; where moc ='st' and sq2 > 60; run;
proc freq data=mob_02; table rlgi; where moc ='do' and sq2 > 60; run;

proc freq data=mob_02; table rlgi; where 50> sq2 > 39; run;
proc freq data=mob_02; table rlgi; where moc ='up' and 50> sq2 > 39; run;
proc freq data=mob_02; table rlgi; where moc ='st' and 50> sq2 > 39; run;
proc freq data=mob_02; table rlgi; where moc ='do' and 50> sq2 > 39; run;


proc freq data=mob_02; table moc; where rlgi ='C' ; run;
proc freq data=mob_02; table moc; where rlgi ='M' ; run;
proc freq data=mob_02; table moc; where rlgi ='L' ; run;
proc freq data=mob_02; table moc; where rlgi ='S' ; run;

proc freq data=mob_02; table moc; where rlgi ='C' and 30> sq2; run;
proc freq data=mob_02; table moc; where rlgi ='M' and 30> sq2; run;
proc freq data=mob_02; table moc; where rlgi ='L' and 30> sq2; run;
proc freq data=mob_02; table moc; where rlgi ='S' and 30> sq2; run;

proc freq data=mob_02; table moc; where rlgi ='C' and 40> sq2 >29; run;
proc freq data=mob_02; table moc; where rlgi ='M' and 40> sq2 >29; run;
proc freq data=mob_02; table moc; where rlgi ='L' and 40> sq2 >29; run;
proc freq data=mob_02; table moc; where rlgi ='S' and 40> sq2 >29; run;

proc freq data=mob_02; table moc; where rlgi ='C' and 50> sq2 >39; run;
proc freq data=mob_02; table moc; where rlgi ='M' and 50> sq2 >39; run;
proc freq data=mob_02; table moc; where rlgi ='L' and 50> sq2 >39; run;
proc freq data=mob_02; table moc; where rlgi ='S' and 50> sq2 >39; run;

proc freq data=mob_02; table moc; where rlgi ='C' and 60> sq2 >49; run;
proc freq data=mob_02; table moc; where rlgi ='M' and 60> sq2 >49; run;
proc freq data=mob_02; table moc; where rlgi ='L' and 60> sq2 >49; run;
proc freq data=mob_02; table moc; where rlgi ='S' and 60> sq2 >49; run;

proc freq data=mob_02; table moc; where rlgi ='C' and sq2 >59; run;
proc freq data=mob_02; table moc; where rlgi ='M' and sq2 >59; run;
proc freq data=mob_02; table moc; where rlgi ='L' and sq2 >59; run;
proc freq data=mob_02; table moc; where rlgi ='S' and sq2 >59; run;



/* Marital Status */
proc freq data=mob_02; table sq7; run;
proc freq data=mob_02; table mari; run;
proc freq data=mob_02; table mari; where moc ='up'; run;
proc freq data=mob_02; table mari; where moc ='st'; run;
proc freq data=mob_02; table mari; where moc ='do'; run;

proc freq data=mob_02; table mari; where 60> sq2 > 29; run;
proc freq data=mob_02; table mari; where moc ='up' and 60> sq2 > 29; run;
proc freq data=mob_02; table mari; where moc ='st' and 60> sq2 > 29; run;
proc freq data=mob_02; table mari; where moc ='do' and 60> sq2 > 29; run;

proc freq data=mob_02; table mari; where sq2 > 60; run;
proc freq data=mob_02; table mari; where moc ='up' and sq2 > 60; run;
proc freq data=mob_02; table mari; where moc ='st' and sq2 > 60; run;
proc freq data=mob_02; table mari; where moc ='do' and sq2 > 60; run;

proc freq data=mob_02; table mari; where 61> sq2 > 50; run;
proc freq data=mob_02; table mari; where moc ='up' and 61> sq2 > 50; run;
proc freq data=mob_02; table mari; where moc ='st' and 61> sq2 > 50; run;
proc freq data=mob_02; table mari; where moc ='do' and 61> sq2 > 50; run;

proc freq data=mob_02; table mari; where 50> sq2 > 39; run;
proc freq data=mob_02; table mari; where moc ='up' and 50> sq2 > 39; run;
proc freq data=mob_02; table mari; where moc ='st' and 50> sq2 > 39; run;
proc freq data=mob_02; table mari; where moc ='do' and 50> sq2 > 39; run;

proc freq data=mob_02; table mari; where 41> sq2 > 30; run;
proc freq data=mob_02; table mari; where moc ='up' and 41> sq2 > 30; run;
proc freq data=mob_02; table mari; where moc ='st' and 41> sq2 > 30; run;
proc freq data=mob_02; table mari; where moc ='do' and 41> sq2 > 30; run;

proc freq data=mob_02; table mari; where 30> sq2; run;
proc freq data=mob_02; table mari; where moc ='up' and 31> sq2; run;
proc freq data=mob_02; table mari; where moc ='st' and 31> sq2; run;
proc freq data=mob_02; table mari; where moc ='do' and 31> sq2; run;


/* SES analysis by mobility */
proc means data=mob_02; var hinc edui; where moc ='up'; run;
proc means data=mob_02; var hinc edui; where moc ='st'; run;
proc means data=mob_02; var hinc edui; where moc ='do'; run;


/* Stats


Male: 988 (49.4%) female: 1012 (50.6%)
Mean age: 46.87
High school: 33.75
College degree: 48.55%
Postgraduate: 2%
Highschool or higher (84.3%)
College or higher: 50.55%
Up: 39.95
Stable: 38.3
Down: 21.75


*/


/* Logistic regression */

/** Worse? **/






/************************************************************************************************************/
/******************************************** Suicidal ideation *********************************************/


/* model
sq1: sex (char)
sq2: age (num)
sq6: edu (char)
sq7: marital (char)
sq8: num_child (num)
sq9: num_family (num) 
sq10: health (char)
sq12: religion (char)
sq12_1: re_num (num) 
sq13: sns (char)
sq15: inc_ind (char)
sq16: inc_hh (char)

q1: happy (num)
q5_1: sat_inc (num)
q5_2: sat_job (num)
q5_3: sat_edu (num)
q5_4: sat_health (num)
q5_5: sat_hous (num)
q5_6: sat_env (num)
q5_7: sat_family (num)
q5_8: sat_tie (num)
q5_9: sat_parti (num)
q5_10: sat_cult (num)
q5_11: sat_safe (num)
q5_12: sat_secu (num)
q5_13: sat_poli (num)

qc2: suicidal ideation (chr)
 
*/

data sui; set data;
if qc2 eq 1 then sui='1';
if qc2 eq 2 then sui='0';
if sq1 eq 1 then sex='M';
if sq1 eq 2 then sex='F';
if sq6 eq 1 then edu='ele';
if sq6 eq 2 then edu='mid';
if sq6 eq 3 then edu='hi';
if sq6 eq 4 then edu='co';
if sq6 eq 5 then edu='gr';
if sq7 eq 1 then mar='ma';
if sq7 eq 2 then mar='un';
if sq7 eq 3 then mar='di';
if sq7 eq 4 then mar='gr';
if sq7 eq 5 then mar='etc';
if sq10 eq 1 then hlt='good';
if sq10 eq 2 then hlt='good';
if sq10 eq 3 then hlt='ok';
if sq10 eq 4 then hlt='bad';
if sq10 eq 5 then hlt='bad';
if sq12 eq 1 then rlg='bud';
if sq12 eq 2 then rlg='pro';
if sq12 eq 3 then rlg='cat';
if sq12 eq 4 then rlg='no';
if sq12 eq 5 then rlg='etc';

run;


proc logistic data=sui descending;
class sex edu mar hlt rlg sq13 / param=ref;
model qc2 = sex edu mar hlt rlg sq13 sq2 sq15 sq16  ;
where sq2 > 59;
run;


proc logistic data=sui descending;
class sex edu (ref= 'ele') mar hlt rlg sq13 / param=ref;
model qc2 = edu;
where sq2 > 59;
run;

proc logistic data=sui descending;
class sex edu (ref= 'ele') mar hlt rlg sq13 / param=ref;
model qc2 = sq13;
where sq2 > 59;
run;


proc logistic data=sui descending;
class sex edu (ref= 'ele') mar hlt rlg sq13 / param=ref;
model qc2 = sq15;
where sq2 > 59;
run;

proc logistic data=sui descending;
class sex edu (ref= 'ele') mar hlt rlg sq13 / param=ref;
model qc2 = sq16;
where sq2 > 59;
run;


proc sort data=sui; by sex; run;
proc means data=sui; var qc2; by sex;
where sq2 > 59;
run;

proc sort data=sui; by edu; run;
proc means data=sui; var qc2; by edu;
where sq2 > 59;
run;

proc sort data=sui; by mar; run;
proc means data=sui; var qc2; by mar;
where sq2 > 59;
run;

proc sort data=sui; by hlt; run;
proc means data=sui; var qc2; by hlt;
where sq2 > 59;
run;

proc sort data=sui; by sq16; run;
proc means data=sui; var qc2; by sq16;
where sq2 > 59;
run;


/* not sig
sq7
*/




data sui; set data;

if mon > 0 then moc = 'up';
if mon = 0 then moc = 'stable';
if mon < 0 then moc = 'down';





sq1: sex   sq2: age  sq3: add_c  sq4: add_o
q1: happy_c q2: happy_p  q3: happy_f 
q5_1: sat_inc q5_2: sat_occu q5_3: sat_edu q5_4: sat_care q5_5: sat_hous q5_6: sat_env q5_7: sat_family q5_8: sat_neighborhood q5_9: sat_participation q5_10: sat_culture q5_11: sat_safety        
q6_1: anx_inc q6_2: anx_occu q6_3: anx_edu q6_4: sat_care q6_5: anx_hous q6_6: anx_env q6_7: anx_family q6_8: anx_neighborhood q6_9: anx_participation q6_10: anx_culture q6_11: axn_safety
q16: ref_inc  q17: ref_happy q18: ref_other_inc  q18_1: ref_people 
q19_1: ind_class q19_2: parent_class  q19_3:  q20: expectation_upward q21: expectation_downward
qc1: retire qc1_1: anx_retire qc1_2: choice_retire qc2: suicide qc2_1: suicide_reason
dq3_1: address_metro dq3_2: address_micro dq3_2_1: address_code
age edu1 edu2 inc_home1 inc_home2 area1 area2 
health
