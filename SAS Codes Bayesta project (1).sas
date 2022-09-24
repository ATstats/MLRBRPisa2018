 proc datasets;
	contents data=BAYESTA.PHLDATA order=collate;
quit;

ods noproctitle;

/*** Analyze categorical variables ***/
title "Frequencies for Categorical Variables";

proc freq data=BAYESTA.PHLDATA;
	tables STRATUM / plots=(freqplot);
run;

/*** Analyze numeric variables ***/
title "Descriptive Statistics for Numeric Variables";

proc means data=BAYESTA.PHLDATA n nmiss min mean median max std;
	var CNTSCHID ESCS student_hindrance teacher_hindrance PERFMATH 
		parent_support gender;
run;

title;

proc univariate data=BAYESTA.PHLDATA noprint;
	histogram CNTSCHID ESCS student_hindrance teacher_hindrance 
		PERFMATH parent_support gender;
run;
