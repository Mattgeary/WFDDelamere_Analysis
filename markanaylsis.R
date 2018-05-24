#install.packages("RMark")
library(RMark)
require(reshape)

################
#function to read the matrix and create the capture history strings
pasty<-function(x) 
  {
    k<-ncol(x)
    n<-nrow(x)
    out<-array(dim=n)
    for (i in 1:n)
      {
        y<-(x[i,]>0)*1
        out[i]<-paste(y[1],y[2],y[3],y[4],y[5],sep="")
        }
    return(out)
    }

##################

input.data<-read.csv("Book3.csv")
#head(input.data)

read.csv("now.csv")
now<-read.csv("now.csv")
x <- cbind(now$Week,now$Week)
y <- cbind(now$Male,now$Female)
matplot(x,y,type="p", pch = 16, bty = "n", xlab = "Week of the Year", ylab = "Number of Individuals Marked")
legend("topright", legend=c("Male", "Female"), pch=16, col=c("black","red"))


#capture history data frame for RMark
capt.hist <- data.frame(ch=pasty(input.data[,3:14]))
#View(capt.hist)
capt.hist1 <- data.frame(ch=pasty(input.data[,3:14]), sex = input.data$sex)
#View(capt.hist1)
capt.hist1 <- capt.hist1[capt.hist1$ch != '00000',]

#process data to CJS format
#necessary because sex is a factor (group) not a continuous covariate
capthist1.processed=process.data(capt.hist1,model="CJS",groups="sex")

#this is not necessary because it is automatically created in the mark() function.
#but it allows us to see (and change) all the details of the design data
caphis.ddl<-make.design.data(capthist1.processed)

###run.models(model.list= NULL, type= "CJS")
### says no models need to be run?

Phi.dot_p.dot=mark(capthist1.processed)
# This output looks like it has been confused by sex

# Writing a new model that specifies interactive sex and time effects

Phi.sex.T=list(formula=~sex*time) 
p.sex.T=list(formula=~sex*time)
Phi.sex.T_p.sex.T=mark(capthist1.processed,model.parameters=list(Phi=Phi.sex.T,p=p.sex.T))
##still looks like the model is being confused by sex


Phi.sex=list(formula=~sex-1) 
p.sex=list(formula=~sex-1)
Phi.sex_p.sex=mark(capthist1.processed,model.parameters=list(Phi=Phi.sex,p=p.sex))
##still looks like the mo

Phi.sex=list(formula=~1) 
p.sex=list(formula=~sex)
Phi.1_p.sex=mark(capthist1.processed,model.parameters=list(Phi=Phi.sex,p=p.sex))
##still looks like the mo


Phi.t=list(formula=~time) 
p.1=list(formula=~1)
Phi.t_p.1=mark(capthist1.processed,model.parameters=list(Phi=Phi.t,p=p.1))


#This is the model that gives us the result we need
Phi.dot_p.dot=mark(capthist1.processed)

# exp(-0.44)/(1+exp(-0.44))
# #[1] 0.391741
# exp(-2.720142)/(1+exp(-2.720142))
# #[1] 0.06179523
# exp(1.8390031)/(1+exp(1.8390031))
# #[1] 0.8628308
# exp(-2.500573)/(1+exp(-2.500573))
# #[1] 0.07581802
# exp(0.2730623)/(1+exp(0.2730623))
# #[1] 0.5678445
# exp(-1.1137554)/(1+exp(-1.1137554))
# #0.2471714
# exp(0.707560)/(1+exp(0.707560))
# #0.6698618
# exp(1.163047)/(1+exp(1.163047))
# #0.7618859

#read.csv("mark.csv")
mark<-read.csv("mark.csv")
#plot(mark$Number ~ mark$Sex)
#read.csv("fen.csv")
fen<-read.csv("fen.csv")
plot(fen$Week, fen$Total, type = "o", pch = 16, xlab = "Week of the Year", ylab= "Number of Individuals Recorded")
# plot(fen$Week, fen$Female)
# plot(fen$Week, fen$Male)
# plot(fen$Week, fen$Teneral)
plot(fen$Week, fen$Total, xlab= "Week of the Year", ylab = "Total Number of L.dubia Individuals Recorded")
df <- read.csv("fen.csv",sep=",",head=T)
x <- cbind(df$Week,df$Week,df$Week)
y <- cbind(df$Male,df$Female,df$Teneral)
matplot(x,y,type="p", pch = 16, bty = "n", xlab = "Week of the Year", ylab = "Number of Individuals Recorded")
legend("topright", legend=c("Male", "Female", "Teneral"), pch=16, col=c("black","red","green"))
