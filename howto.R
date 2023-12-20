library(BART)
load("C:/Users/aakuhlemeier/Documents/GitHub/LookAHEAD-PITE/LookAHEAD_trt.RData")
load("C:/Users/aakuhlemeier/Documents/GitHub/LookAHEAD-PITE/LookAHEAD_ctl.RData")

load(".../GitHub/LookAHEAD-PITE/LookAHEAD_ctl.RData")

##CODEBOOK OF BASELINE PREDICTORS##

#VARIABLE			DESCRIPTION					RANGE
#baselinewgt_kg	| baseline weight	(kg)			|   [58.00,182.65]
#female		| participant sex				|   [0,1]
#raceth_black	| race/ethnicity (Black)		|   [0,1]
#raceth_hisp	| race/ethnicity (Hispanic)		|   [0,1]
#raceth_other	| race/ethnicity (Other race)		|   [0,1]
#[white-ref]	| 
#age			| age (in years)				|   [44,76]
#lesshs		| educ (less than HS)			|   [0,1]
#hs			| educ (HS or GED)			|   [0,1]
#morehs		| educ (AA or voc/tech) 		|   [0,1]
#[somecollege-ref]|
#ba			| educ (Bachelor's)			|   [0,1]	
#ma			| educ (Master's, some grad)		|   [0,1]
#docprof		| educ (MD, PhD, JD)			|   [0,1]
#unemployed		| unemployment status			|   [0,1]
#divorced		| marital status (divorced)		|   [0,1]
#widowed		| marital status (widowed)		|   [0,1]
#nevmarr		| mar. status (never married) 	|   [0,1]
#[married-ref]	|
#currsmoke		| smoking status (current)		|   [0,1]
#pastsmoke		| smoking status (in the past)	|   [0,1]
#[neversmoked-ref]|
#total_alcohol	| alcohol consumption (oz/wk) 	|   [0,864]
#binge		| edeat2hr&edcontrol=yes*		|   [0,1]
#avgabi		| ankle brachial index			|   [0.61,2.68]
#hba1cpct_lab	| HbA1c (%)					|   [4.7,14.5]
#maxexmets		| maxMET during graded exercise test|   [3.3,16.7]
#diab_family	| family history, diabetes		|   [0,1]
#cvdhis		| personal history, cvd			|   [0,1]
#prob1yr		| Framingham Risk score, 1 year	|   [0,0.13]
#met_syn_tot	| # of metabolic syndrome criteria	|   [1,5]
#genhlth		| SF-36 general health score		|   [16.23, 63.90]
#beck_score_nowgt	| Beck Depression Inventory**		|   [0,35]
#any_diabdrug	| taking any diabetes meds		|   [0,1]
#any_htndrug	| taking any anti-hypertensive meds	|   [0,1]
#any_insulin	| taking insulin				|   [0,1]
#any_lipidrug	| taking any lipid-lowering meds	|   [0,1]

#*Variable "binge" is a composite of two items:
#	edeat2hr="Did you ever eat a really big amount of food within a short time (2 hours or less)?"
#	edcontrol="When you ate a really big amount of food, did you ever feel that you could not control what or how much you were eating?"
#	binge=1 if edeat2hr&edcontrol=1; binge=0 if edeat2hr=0 and/or edcontrol=0
#**Beck Depression Inventory score does not include the question in the inventory about eating/weight

##ENTER VALUES FOR EACH VARIABLE
baselinewgt_kg<-142.3
female<-0
raceth_black<-0 
raceth_hisp<- 0
raceth_other<-0
age<-58 
lesshs<-0 
hs<-0 
morehs<-0 
ba<-1 
ma<-0
docprof<-0
unemployed<-1 
divorced<-0 
widowed<-0 
nevmarr<-0 
currsmoke<-1 
pastsmoke<-0 
total_alcohol<-0 
binge<-1
avgabi<-1.243902 
hba1cpct_lab<-7.5
maxexmets<-8.5
diab_family<-0
cvdhis<-0
prob1yr<-0.02099903 
met_syn_tot<-5  
genhlth<-52.93366 
beck_score_nowgt<-0
any_diabdrug<-1
any_htndrug<-1 
any_insulin<-0
any_lipidrug<-1

##COMBINE VALUES ON ALL PREDICTORS INTO A VECTOR (1 per person)
vector<-c(baselinewgt_kg,female,raceth_black,raceth_hisp,raceth_other,age,lesshs,hs,morehs,ba,ma,docprof,unemployed,
	divorced,widowed,nevmarr,currsmoke,pastsmoke,total_alcohol,binge,avgabi,hba1cpct_lab,maxexmets,diab_family,cvdhis,
	prob1yr,met_syn_tot,genhlth,beck_score_nowgt,any_diabdrug,any_htndrug,any_insulin,any_lipidrug)
matrix<-as.matrix(vector)

##GENERATE PREDICTIONS UNDER TREATMENT AND CONTROL USING BART ALGORITHM
predt<-predict(ttrees,t(matrix))
predc<-predict(ctrees,t(matrix))

##CALCULATE PREDICTED INDIVIDUAL TREATMENT EFFECT (1000 trees?)
pite.ind<-predt-predc
mean(pite.ind)
#[1] -9.106779	##On average, this person would be expected to lost 9.1% of their body weight with LookAHEAD intervention

##CALCULATE CREDIBLE INTERVAL
pite.ind.ci<-quantile(pite.ind, probs=c(0.2,0.8))
#       20%        80% 
#-10.980817  -7.340483 #It is likely (60%) that this person would lose between 7.3% and 11% of their body weight with the LookAHEAD intervention 
