

/* Computing the difference between the coefficients of interest */

/* Self-rated */
proc logistic data=mob_02 descending;
class moc (ref='st') aggrp (ref='20s') sq1(ref='1') edui (ref='col') hinc (ref='H') rlgi (ref='C') mari (ref='M')/ param=ref;
model health_b = moc sq1 aggrp edui hinc rlgi mari;
run;

/****** Best estimate ******/
proc logistic data=mob_02 descending;
class moc (ref='st') aggrp (ref='20s') sq1(ref='1') edui (ref='col') hinc (ref='H') rlgi (ref='C') mari (ref='M')/ param=ref;
model health_b = moc sq1 aggrp edui hinc rlgi mari;
contrast 'Difference between do and up' moc 1 -1 0;
contrast 'Difference between st and do' moc 1 0 -1;
contrast 'Difference between st and up' moc 0 1 -1;
run;


proc logistic data=mob_02 descending;
class moc (ref='st') aggrp (ref='20s') sq1(ref='1') edui (ref='col') hinc (ref='H') rlgi (ref='C') mari (ref='M')/ param=ref;
model health_b = moc sq1 aggrp edui hinc rlgi mari;
contrast 'Difference between st and do' moc 1 -1;
where moc not in ('up');
run;
/* p=.0257 ->  */


proc logistic data=mob_02 descending;
class moc (ref='st') aggrp (ref='20s') sq1(ref='1') edui (ref='col') hinc (ref='H') rlgi (ref='C') mari (ref='M')/ param=ref;
model health_b = moc sq1 aggrp edui hinc rlgi mari;
contrast 'Difference between st and up' moc 1 -1;
where moc not in ('do');
run;
/* p=0.8330*/


proc logistic data=mob_02 descending;
class moc /*(ref='st')*/ aggrp (ref='20s') sq1(ref='1') edui (ref='col') hinc (ref='H') rlgi (ref='C') mari (ref='M')/ param=ref;
model health_b = moc sq1 aggrp edui hinc rlgi mari;
contrast 'Difference between do and up' moc 1 -1;
where moc not in ('st');
run;
/* p=0.1133*/




/****** Happiness ******/

/* Best estimate */
proc logistic data=mob_02 descending;
class moc (ref='st') aggrp (ref='20s') sq1(ref='1') edui (ref='col') hinc (ref='H') rlgi (ref='C') mari (ref='M')/ param=ref;
model hap = moc sq1 aggrp edui hinc rlgi mari;
contrast 'Difference between do and up' moc 1 -1 0;
contrast 'Difference between st and do' moc 1 0 -1;
contrast 'Difference between st and up' moc 0 1 -1;
run;


proc logistic data=mob_02 descending;
class moc (ref='st') aggrp (ref='20s') sq1(ref='1') edui (ref='col') hinc (ref='H') rlgi (ref='C') mari (ref='M')/ param=ref;
model hap = moc sq1 aggrp edui hinc rlgi mari;
contrast 'Difference between st and do' moc 1 -1;
where moc not in ('up');
run;
/* p=.3334 ->  */


proc logistic data=mob_02 descending;
class moc (ref='st') aggrp (ref='20s') sq1(ref='1') edui (ref='col') hinc (ref='H') rlgi (ref='C') mari (ref='M')/ param=ref;
model hap = moc sq1 aggrp edui hinc rlgi mari;
contrast 'Difference between st and up' moc 1 -1;
where moc not in ('do');
run;
/* p=0.0008*/


proc logistic data=mob_02 descending;
class moc /*(ref='st')*/ aggrp (ref='20s') sq1(ref='1') edui (ref='col') hinc (ref='H') rlgi (ref='C') mari (ref='M')/ param=ref;
model hap = moc sq1 aggrp edui hinc rlgi mari;
contrast 'Difference between do and up' moc 1 -1;
where moc not in ('st');
run;
/* p=0.0004*/


