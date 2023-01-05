##DO YOU WANT TO SUBTRACT OUT ATE? MAKE SURE CODE IS CORRECT##



# Check BART ITE CI coverage
library(BART)
library(gplots)
############################################################################################
##Look Ahead BART
data<-read.csv("M:/look-ahead/Data/analysis data.csv")

df<-read.csv("M:/look-ahead/Data/working data.csv")

set.seed(356497)
 xt <- as.matrix(cov[trt == 1,])
  xc <- as.matrix(cov[trt == 0,])
  yt <- y[trt == 1] ##- ATE.est
  yc <- y[trt == 0]

  mt <- wbart(xt,yt,ntree=200)
  mc <- wbart(xc,yc,ntree=200)
  predt<-tryCatch( predict(mt, cov), error=function(e) NA)
  predc<-tryCatch( predict(mc, cov), error=function(e) NA)
pred_c<-apply(predc,MARGIN=2,FUN=mean)
pred_t<-apply(predt,MARGIN=2,FUN=mean)

  pite.ind <- predt - predc
  pite.sd <- sd(pite.ind)
############################################################################################
##MRC CODE
##test data=out of sample data, generate?  
 # # Train BART model and obtain predictions (suppress output)
 # invisible(capture.output(post <- pbart(train.dat, Y, test.dat, ntree=200)))
  
## only relevant for binary outcome?
  # We note that the predictions are on the z-score scale
#  test.yhat <- post$yhat.test
  
  # Turn z-score into probabilities		##Do we want to do this or do we want them on original scale?
#  test.pred.prob <- pnorm(test.yhat, 0, 1)
  
  # Extract predictions for P(Y=1) under treatment 0 (all posterior samples for all test subjects)
#  pred.trt0 <- test.pred.prob[, 1:n.test]
  # Extract predictions for P(Y=1) under treatment 1 (all posterior samples for all test subjects)
#  pred.trt1 <- test.pred.prob[, n.test + (1:n.test)]
  # Predicted ITE (risk difference)
#  pred.ite <- pred.trt1 - pred.trt0
  
  # Posterior mean for ITE for each test subject
#  pred.ite.mean <- apply(pred.ite, 2, mean)	## this code calculates PITEs first, then mean
								## our code calculates means for T & C separately then PITEs
###############################################################################################

  # Posterior mean for ITE for each test subject
  pite.ind.mean <- apply(pite.ind, 2, mean)
  predt.mean <- apply(predt, 2, mean)
  predc.mean <- apply(predc, 2, mean)

  # 95% posterior credible interval for ITE for each test subject
  pite.ind.ci <- apply(pite.ind, 2, quantile, probs = c(0.2, 0.8))##(0.1,0.9)(0.05,0.95)??

#Code used for CI plot in JCCP paper
tgraph<-t(pite.ind.ci)
graph<-as.data.frame(cbind(data$P_ID,data$trt, pite.ind.mean, tgraph,predt.mean,predc.mean))
colnames(graph)<-c("id","txassgn","pite","lowerCI","upperCI","predt","predc")

set.seed(125667)
rand100<-graph[sample(nrow(graph), 100), ]				#pull random sample of 100 participants
rgraph<-as.data.frame(rand100)

#Sort pites to determine ids for 25th and 75th percentile
ograph <- rgraph[order(rgraph$pite),]


ograph_c<-ograph[which(ograph$txassgn==0),]
ograph_t<-ograph[which(ograph$txassgn==1),]

diff(ograph_c$pite)
which.min(diff(ograph_c$pite)) 
ograph_c$pite	#25,26: 27361 & 12035
fix(ograph_c)
ograph_c[which(ograph_c$id==27361 |ograph_c$id==12035),]
data[which(data$P_ID==27361 |data$P_ID==12035),]

diff(ograph_t$pite)
which.min(diff(ograph_t$pite))
ograph_t$pite	#27,28:10389,48569
fix(ograph_t)
ograph_t[which(ograph_t$id==10389|ograph_t$id==48569),]
data[which(data$P_ID==10389|data$P_ID==48569),]



#diff(ograph_c$pite) ##ids:10084 14000
#which.min(diff(ograph_c$pite))
#ograph_c[which(ograph_c$id==10084|ograph_c$id==14000),] ##highlight these people in the intervals gea--


ograph$sortid <- 1:nrow(ograph)
###25%ile: -9.555389 (id:35905) 75%ile:-6.703601 (id:29046)
##rand100: -9.506168 (id:28897) 75%ile:-6.701898 (id:10719)
x<-ograph$sortid
y<-ograph$pite
ui<-ograph$upperCI
li<-ograph$lowerCI
ylab<-"Predicted % weight loss under treatment"
xlab<-"Random sample of 100 individuals with 2 cases at 25th and 75th percentile in the full sample, ordered by predicted probability"


plotCI(x=x,y=y,ui=ui,li=li, ylab=strwrap(ylab),	
	xlab=strwrap(xlab),cex.lab=1.3, cex.axis=0.75)

points(x=27,y=-9.506168,pch=19,col="red") 
points(x=75,y=-6.701898,pch=19,col="red") 
text(x=27, y=-7,labels="25th percentile PITE")
text(x=75, y=-3.5,labels="75th percentile PITE")
arrows(x0=27,y0=-7.2,x1=27,y1=-9.5,col="red",length=0.15)
arrows(x0=75,y0=-3.7,x1=75,y1=-6.69,col="red",length=0.15)

#abline(h=0, col="red")
#mtext(" |-------higher predicted weight loss------|",side=2,adj=0,padj=1.5)
#mtext(" |-------lower predicted weight loss------|",side=2,adj=1,padj=1.5)


################################################################################################  
  # Calculate the true treatment effect (risk difference) for each test subject
  s.test.trt0 <- effect(x1.test, x2.test, x3.test, rep(0, n.test))
  s.test.trt1 <- effect(x1.test, x2.test, x3.test, rep(1, n.test))
  true.trt.diff <- 1 / (1 + exp(s.test.trt1)) - 1 / (1 + exp(s.test.trt0))
  
  # plot(pred.ite.mean ~ true.trt.diff, pch=16,xlim=c(-0.5,0.5),ylim=c(-0.5,0.5))
  # abline(0,1)
  
  # Record coverage results and width of posterior credible intervals
  for (j in 1:n.test) {
    # Whether or not in the ith repetition the 95% CI for subject j includes the truth 
    include[i,j] <-
      (true.trt.diff[j] >= pred.ite.ci[1, j]) &
      (true.trt.diff[j] <= pred.ite.ci[2, j])
    # In the ith repetition, the Width of 95% CI for subject j
    width[i,j] <- pred.ite.ci[2, j]-pred.ite.ci[1, j]
  }
  
  print(paste0("Loop ",i," completed"))

}

proc.time() - ptm

# saveRDS(include, file = "cover_n_5000_ntree_200.rds")
# saveRDS(width, file = "width_n_5000_ntree_200.rds")

# Load saved objects and summarize coverage results
n_train <- c(500,5000)
n_tree <- c(50,200)
comb <- expand.grid(n_tree,n_train)
n_comb <- dim(comb)[1]
summary_coverage <- data.frame(matrix(rep(NA, n_comb*3),nrow=n_comb))
colnames(summary_coverage) <- c("median","25th percentile","75th percentile")
rownames(summary_coverage) <- c("n=500 with 50 trees","n=500 with 200 trees","n=5000 with 50 trees","n=5000 with 200 trees")

for (i in 1:n_comb){
  tree_size <- comb$Var1[i]
  train_size <- comb$Var2[i]
  setting <- paste("n",train_size,"ntree",tree_size,sep="_")
  # Calculate coverage probability for each replication across n.test subjects (not rigorous definition of coverage in the frequentist framework)
  assign(paste("coverage_p",setting,sep="_"),apply(readRDS(paste("cover_",setting,".rds",sep="")),1,mean))
  # Calculate median and IQR across repetitions
  summary_coverage[i,] <- round(quantile(get(paste("coverage_p",setting,sep="_")),c(0.5,0.25,0.75)),3)
}


