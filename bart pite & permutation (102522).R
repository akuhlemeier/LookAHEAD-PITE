library(BART)
source("M:/Alena/PITE code/pite&prmt_bart_continuous(100521).R")
data<-read.csv("M:/look-ahead/Data/cleanedlookahead(102322).csv")

nperm<-1000

y<-data$pctwgtchange
trt<-data$trt
cov<-data[,c("baselinewgt_kg","female","raceth_black","raceth_hisp",
	"raceth_other","age","lesshs","hs","morehs","ba","ma","docprof","unemployed",
	"divorced","widowed","nevmarr","currsmoke","pastsmoke","total_alcohol","binge",
	"avgabi","hba1cpct_lab","maxexmets","diab_family","cvdhis","prob1yr",
	"met_syn_tot","genhlth","beck_score_nowgt","any_diabdrug","any_htndrug",
	"any_insulin","any_lipidrug")]
dim(cov)

set.seed(664524)
bart.pite<-PITE.bart.GLM(trt,y,cov)
bart.perm<-PITE.bart.prmt(bart.pite,nperm,938503)
save.image("M:/look-ahead/updatedLAresults(102322).RData")



pite.ind<-bart.pite$pite.ind
pred.tx<-bart.pite$predt
pred.cntl<-bart.pite$predc

pite.ind.trees<-pred.tx-pred.cntl
pite.ind.ci<-t(apply(pite.ind.trees,2,quantile, probs=c(0.2,0.8)))
colnames(pite.ind.ci)<-c("lowerCI","upperCI")
piteids<-cbind(data,pite.ind,pite.ind.ci)

#
hist(bart.pite$pite.ind)
par(mfrow=c(1,2))
hist(bart.pite$pite.trt.cond)
hist(bart.pite$pite.cntl.cond)
## Effect.Size   : num 0.275

##Histograms for Permutation Test
bart.alpha<-(sum(ifelse(bart.perm$prmt.sd <= bart.perm$pite.sd, 0, 1 ),
	na.rm=T)/(nperm-bart.perm$failed.prmts))


hist(bart.perm$prmt.sd, xlab = "SD of the PITE for each permuted dataset \n p-value=0.000",
	xlim=c(0.5,2.5), main=" ")
abline(v=bart.perm$pite.sd, col="red")
