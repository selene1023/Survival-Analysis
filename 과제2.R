library(survival)

time1<-c(6,6,6,7,10,13,16,22,23,6,9,10,11,17,19,20,25,32,32,34,35)      #Group1
status1<-c(rep(1,9),rep(0,12))

time2<-c(1,1,2,2,3,4,4,5,5,8,8,8,8,11,11,12,12,15,17,22,23)             #Group2
status2<-rep(1,21)

logWBC<-c(2.31,4.06,3.28,4.43,2.96,2.88,3.60,2.32,2.57,3.20,2.80,2.70,2.60,2.16,2.05,2.01,1.78,2.20,2.53,1.47,1.45,2.80,5.00,4.91,4.48,4.01,4.36,2.42,3.49,3.97,3.52,3.05,2.32,3.26,3.49,2.12,1.50,3.06,2.30,2.95,2.73,1.97)

remission<-data.frame(time=c(time1,time2),status=c(status1,status2),group=c(rep(0,21),rep(1,21)),lWBC=logWBC)

#a. K-M curves for three groups
c.lWBC <- rep("medium",length(remission$lWBC))
for (i in 1:length(remission$lWBC)){
  if(remission$lWBC[i] > 3.0) c.lWBC[i] <- "high"}
for (i in 1:length(remission$lWBC)){
  if(remission$lWBC[i] < 2.31) c.lWBC[i] <- "low"}

remission$c.lWBC<-c.lWBC

remission$c.lWBC<-ifelse(remission$lWBC>3, "high",ifelse(remission$lWBC<=2.30,"low","medium"))

head(remission)

attach(remission)

t.surv<-Surv(time,status)

r.survfit <- survfit(t.surv~c.lWBC)
summary(r.survfit)

plot(r.survfit, col=c(1,2,3))
legend(25, 1, c("high","low","medium"), col=c(1,2,3), lty=c(1,1,1))

plot(r.survfit <- survfit(t.surv~c.lWBC), col=c(1,2,3))
legend("topright", c("high","low","medium"), col=c(1,2,3), lty=c(1,1,1))

#c. Log rank test
r.survdiff <- survdiff(t.surv ~ c.lWBC, rho = 0)
r.survdiff