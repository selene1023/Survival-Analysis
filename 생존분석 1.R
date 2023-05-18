library(survival)

#Problem 1.1 dataset

Survtime<-c(7,6,6,5,2,4)
Status<-c(0,1,0,0,1,1)

time1<-Surv(Survtime,Status)
time1

# Remission dataset

time1<-c(6,6,6,7,10,13,16,22,23,6,9,10,11,17,19,20,25,32,32,34,35)      #Group1
status1<-c(rep(1,9),rep(0,12))

time2<-c(1,1,2,2,3,4,4,5,5,8,8,8,8,11,11,12,12,15,17,22,23)             #Group2
status2<-rep(1,21)

logWBC<-c(2.31,4.06,3.28,4.43,2.96,2.88,3.60,2.32,2.57,3.20,2.80,2.70,2.60,2.16,2.05,2.01,1.78,2.20,2.53,1.47,1.45,2.80,5.00,4.91,4.48,4.01,4.36,2.42,3.49,3.97,3.52,3.05,2.32,3.26,3.49,2.12,1.50,3.06,2.30,2.95,2.73,1.97)

remission<-data.frame(time=c(time1,time2),status=c(status1,status2),group=c(rep(0,21),rep(1,21)),lWBC=logWBC)

head(remission)
attach(remission)

my.surv1<-Surv(time1,status1);my.surv1
my.surv2<-Surv(time2,status2);my.surv2
my.surv<-Surv(time,status);my.surv


t.surv<-Surv(time,status)      
trt.surv<-Surv(time[group==0],status[group==0])     #treatment group surv object
pla.surv<-Surv(time[group==1],status[group==1])     #placebo group surv object

trt.survfit<-survfit(trt.surv~1)
pla.survfit<-survfit(pla.surv~1)

trt.survfit; summary(trt.survfit)
pla.survfit; summary(pla.survfit)

par(mfrow=c(1,2))
plot(trt.survfit)
plot(pla.survfit)

plot(t.survfit<-survfit(t.surv~group),col=c(1,2))
legend(25,0.9,legend=c("treatment","placebo"),col=c(1,2),lty=c(1,1))


survdiff(t.surv~group, rho=0)
survdiff(t.surv~group, rho=1)

c.lWBC<-rep("normal",length(lWBC)); ori<-10^lWBC
for(i in 1:length(lWBC)){if(ori[i]>10000) c.lWBC[i]<-"high"}
for(i in 1:length(lWBC)){if(ori[i]<4000) c.lWBC[i]<-"low"}
survdiff(t.surv~group+strata(c.lWBC),rho=0)