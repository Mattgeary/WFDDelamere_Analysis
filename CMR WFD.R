library(reshape2)

input.data<-read.csv("Book3.csv")
names(input.data)<-tolower(names(input.data))
CMR.dat<-melt(input.data,id=c("animal_id","sex"))
CMR.dat<-subset(CMR.dat, value==1)
names(CMR.dat) <-c("ind","sex","date","pop")
CMR.dat$date <-gsub("x","",CMR.dat$date)
CMR.dat$date <-gsub("\\.","-",CMR.dat$date)
CMR.dat$date <-as.POSIXct(CMR.dat$date, origin = "2017-05-22") 
CMR.dat$ind <-as.factor(paste("ind",CMR.dat$ind,sep=""))
CMR.dat$pop <-as.factor(CMR.dat$pop)
captocc <-data.frame(pop="1",start=c("2017-05-22","2017-05-25","2017-05-30","2017-06-01","2017-06-07","2017-06-13","2017-06-16","2017-06-19","2017-06-26","2017-07-04","2017-07-07"),end=c("2017-05-23","2017-05-26","2017-05-31","2017-06-02","2017-06-08","2017-06-14","2017-06-17","2017-06-20","2017-06-27","2017-07-05","2017-07-08"))
captocc$start<-as.POSIXct(captocc$start)
captocc$end<-as.POSIXct(captocc$end)

WFD.cmr<-CMRData(indvar = CMR.dat,captocc = captocc, unit="day")
surv <- as.formula(~1)
capt <- as.formula(~t)

fit <- CMRFit(surv, capt, WFD.cmr)
fit



#function to read the matrix and create the capture history strings
pasty<-function(x)
{
  k<-ncol(x)
  n<-nrow(x)
  out<-array(dim=n)
  for (i in 1:n)
  {
    y<-(x[i,]>0)*1
    out[i]<-paste(y[1],y[2],y[3],y[4],y[5],y[6],y[7],y[8],y[9],y[10],y[11],y[12],sep="")
  }
  return(out)
}

input.data<-read.csv("Book3.csv")
#head(input.data)

#capture history data frame for RMark
capt.hist1 <- data.frame(ch=pasty(input.data[,3:14]), sex = input.data$sex)

model=crm(capt.hist1)
model


library(CMRCT)

## Not run: 
##Let define some arguments
start <- as.POSIXct(strptime("01/01/2000", "%d/%m/%Y"))
factind <- list(sex=c("M", "F"))
##numind <- list(weight=c(0.5, 4))
numtps <- list(rain=c(0, 100))

##Simulate the set of covariables
set.seed(123)
simvars <- CMRSimVars(n=15 , npop=4, start=start, step="6 months",
                      nstep=12, factind=factind, numtps=numtps, length="5 weeks",
                      clage=c(2), minage=0.1, maxage=20)

##Plot the simulated capture occasions
plot(simvars)

##True model
surv <- as.formula(~pop + age)
capt <- as.formula(~t)

##True model parameters values
#Survival parameters: Intercept, pop2, pop3, pop4, age1
surv.pars <- c(-2, 1.5, -0.1, -0.5, 0.3)
#Capture parameters: Intercept, t
capt.pars <- c(-1, 0.24)
theta <- c(surv.pars, capt.pars)

##Simulating a dataset
set.seed(456)
simdata <- CMRSimData(simvars=simvars, surv=surv, capt=capt, theta=theta)

##Plot the simulated capture occasions and capture histories
plot(simdata)

##Fit the model
fit <- CMRFit(surv=surv, capt=capt, cmrdata=simdata)
fit

s = -2.772759
c = -3.049549
s_back <- exp(s)/(1+exp(s))
c_back <- exp(c)/(1+exp(c))
s_back
c_back

s_CIl <- -2.772759-1.96*(0.6206933)
s_CI95L <-exp(s_CIl)/(1+exp(s_CIl))
s_CI95L
s_CIu <- -2.772759+1.96*0.6206933
s_CI95U <-exp(s_CIu)/(1+exp(s_CIu))
s_CI95U

c_CIl <- -3.049549-1.96*0.511335
c_CI95L <-exp(c_CIl)/(c+exp(s_CIl))
c_CI95L
c_CIu <- -3.049549+1.96*0.511335
c_CI95U <-exp(c_CIu)/(1+exp(c_CIu))
c_CI95U
