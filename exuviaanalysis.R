#exuvia data analysis
setwd("C:/Users/rache/Desktop")
read.csv("wfdexuvia2.csv")
dataset <- read.csv("wfdexuvia2.csv")
head(dataset)
#____________________________________________________________________________________
#making boxplots
boxplot(dataset$Date, dataset$Exuvia.Found)
boxplot(dataset$Date, dataset$Sex)
# again graph doesn't show anything
boxplot(dataset$Exuvia.Found, dataset$X..Sphagnum, xlab = "exuvia found", ylab = "percentage of sphagnum")
boxplot(dataset$Exuvia.Found, dataset$Veg.Species, xlab = "exuvia found", ylab = "vegetation species")
boxplot(dataset$Exuvia.Found, dataset$X..emergent.veg, xlab = "exuvia found", ylab = "percentage of emergnet vegetation")
# possible that a high % of emergent veg often returns no exuvia as detectibility becomes more difficult
#____________________________________________________________________________________
#checking correlations
cor(dataset$estimate.no.of.emergent.stems, dataset$X..emergent.veg, use = 'complete.obs')
cor.test(dataset$estimate.no.of.emergent.stems, dataset$X..emergent.veg)
plot(dataset$estimate.no.of.emergent.stems, dataset$X..emergent.veg)
# this shows number of emergent stems is positively correlated with percentage of emergent veg
cor(dataset$X..Sphagnum, dataset$X..open.water,use = 'complete.obs')
cor.test(dataset$X..Sphagnum, dataset$X..open.water)
# no correlation
plot(dataset$X..Sphagnum, dataset$X..open.water)
# this shows that percentage of sphagnum is negatively correlated with percentage of open water

#Looking at habitats where exuvia where/where not found

boxplot(dataset$estimate.no.of.emergent.stems ~ dataset$Exuvia.Found, xlab = "exuvia found", ylab = "number of emergent stems")
#This box plot potentially shows an ideal amount of emergent vegetation
boxplot(dataset$X..emergent.veg ~ dataset$Exuvia.Found, xlab = "exuvia found", ylab = "percentage of emergnet vegetation")
#This boxplot also seems to suggest an ideal amount of vegetation, maybe due to detectability
# or the fact that inside dense vegetation it may be too shaded for larvae to emerge

boxplot(dataset$X..open.water ~ dataset$Exuvia.Found, xlab = "exuvia found", ylab = "percentage of open water")
boxplot(dataset$X..Sphagnum ~ dataset$Exuvia.Found, xlab = "exuvia found", ylab = "percentage of sphagnum")

boxplot(dataset$Veg.Species ~ dataset$Exuvia.Found, xlab = "exuvia found", ylab = "veg species")
str(dataset)
boxplot(dataset$Exuvia.Found[which(dataset$Veg.Species == "Soft Rush")], dataset$Exuvia.Found[which(dataset$Veg.Species == "No Vegetation")], dataset$Exuvia.Found[which(dataset$Veg.Species == "Common cotton grass")])
#subset(dataset, Veg.Species = "Soft Rush")
#subset(dataset, Veg.Species = "Common cotton grass")
#subset(dataset, Veg.Species = "No Vegetation")


#preforming linear model
##looking at the relationship between emergent vegetation and if exuvia were found or not
l.mod <- lm (dataset$X..emergent.veg ~ dataset$Exuvia.Found, data = TRUE)
#this violates assumption of simple linear regression as result is binary
str(dataset)
#number data seems to be stored as integer data
as.numeric("X..emergent.veg")
str(dataset)
dataset$X..emergent.veg<-as.numeric(dataset$X..emergent.veg) 
l.mod <- lm (dataset$Exuvia.Found ~ dataset$X..emergent.veg, data = dataset)
#this violates assumption of simple linear regression as result is binary
summary(l.mod)
plot(dataset$X..emergent.veg ~ dataset$Exuvia.Found)
abline(l.mod)
# Not plotting all of the exuvia found as there were 14 found and only 5 dots above 1
# possibly plotted on top of each other

plot(l.mod)
# qq looks a little odd
plot(l.mod$fitted.values, l.mod$residuals)
qqnorm(residuals(l.mod)) 
qqline(residuals(l.mod))

#doing a second linear model
##looking at the relationship between percentage of spgahnum and if exuvia were found or not
l.mod2 <- lm (dataset$Exuvia.Found ~ dataset$X..Sphagnum, data = dataset)
#this violates assumption of simple linear regression as result is binary
summary(l.mod2)
plot(dataset$X..Sphagnum ~ dataset$Exuvia.Found)
abline(l.mod2)
plot(l.mod2)
#normall qq looks nice
plot(l.mod$fitted.values, l.mod$residuals)

#fitting a multiple linear regression model to predict if exuvia will be found or not
#percentage of spahgnum and emergent veg are the predictors
#this makes sense based on the fact that wfd use sphagnum and that they need emergent veg for emergence
#veg species could also potentially be used

multi.mod<- lm(X..Sphagnum + X..emergent.veg ~ Exuvia.Found, data=dataset) 
summary(multi.mod)
mod.AIC <- AIC(multi.mod)
summary(mod.AIC)

#matt suggested logisitc regressions instead as response variable is binary
#looking at the relation between exuvia found and percentage of sphagnum
log.mod <- glm(dataset$Exuvia.Found ~ dataset$X..Sphagnum, family = "binomial")
summary(log.mod)
#AIC is 39.696
plot(log.mod)
plot(residuals(log.mod, type = c("pearson"))) 
plot(residuals(log.mod, type = c("pearson")), dataset$X..Sphagnum)
#graph wouldnt work as xy differ in length

plot(dataset$X..Sphagnum, dataset$Exuvia.Found, xlab = "percentage of sphagnum", ylab = "exuvia found (1 = TRUE, 0 = FALSE)")
#think graph would look better the other way around
plot(dataset$Exuvia.Found, dataset$X..Sphagnum, xlab = "percentage of sphagnum", ylab = "exuvia found (1 = TRUE, 0 = FALSE)")
#graphs look a bit rubbish
lines(dataset$X..Sphagnum[order(dataset$X..Sphagnum)], predict(log.mod, type="resp")[order(dataset$X..Sphagnum)], col = "red")
cdplot(factor(Exuvia.Found) ~ X..Sphagnum, data=dataset)
#i interpret this as the percentage of sphagnum has little affect

#looking at the relationship between exuvia found and percentage of emergent vegetation
log.mod2 <- glm(dataset$Exuvia.Found ~ dataset$X..emergent.veg, family = "binomial")
summary (log.mod2)
#AIC is 38.248
##it says 7 observations deleted- these are the days we surveyed and found nothing
plot(log.mod2)
plot(residuals(log.mod2, type = c("pearson"))) 
plot(residuals(log.mod2, type = c("pearson")), dataset$X..emergent.veg)
#graph wouldnt work as xy differ in length
?? "pearson"
length("Exuvia.Found")
length("X..emergent.veg")
#this tells me that x and y are both 1
cdplot(factor(Exuvia.Found) ~ X..emergent.veg, data=dataset, xlab = "Percentage of Emergent Vegetation", ylab= "Exuvia Found")
#interesting result, again it suggest that there is an optimum percentage of vegetation 
##for finding exuvia..may be a larval preference but could also indicate that they are easier to see
#if this was repeated for the number of stems, i would expect a similar result as it it positively correlated with
##percentage of emergent vegetation

plot(dataset$Exuvia.Found, dataset$X..emergent.veg, type = "l", xlab = "Week of the Year", ylab = "Number of Male Exuvia Recorded")
plot(dataset$X..emergent.veg, dataset$Exuvia.Found, type = "l")
plot(dataset$Exuvia.Found, dataset$X..emergent.veg, type = "", xlab = "Number of Exuvia Recorded", ylab = "Percentage of Emergent Vegetation")



#would like to look at vegetation species
boxplot(dataset$Exuvia.Found[which(dataset$Veg.Species == "Soft Rush")],dataset$Exuvia.Found[which(dataset$Veg.Species == "No Vegetation")], dataset$Exuvia.Found[which(dataset$Veg.Species == "Common cotton grass")])
#boxplot suggests equal numbers found on both soft rush and cotton grass
#i know from the data that exuvia were found in equal numbers on both veg species
#indicates no preference to cotton grass, as was found in Meredith, 2017
##but take into account the very small sample size, would need ot be investigated further

read.csv("peakemergence2.csv")
peak2<- read.csv("peakemergence2.csv")
barplot(peak2$Week, peak2$Exuvia.Found )
plot(peak2$Exuvia.Found, peak2$Week, type = "l", xlab = "Week of the Year", ylab = "Number of Exuvia Recorded")
plot(peak2$Week, peak2$Exuvia.Found, type = "o", xlab = "Week of the Year", ylab = "Number of Exuvia Recorded")
read.csv("emerg.csv")
em<-read.csv("emerg.csv")
plot(em$X..emergent.veg, em$Exuvia.Found, type = "h", lwd = 3, xlab = "Percentage of Emergent Vegetation", ylab = "Number of Exuvia Recorded")
plot(em$Exuvia.Found, em$X..emergent.veg)

read.csv("spag.csv")
spag<-read.csv("spag.csv")
plot(spag$X..Sphagnum, spag$Exuvia.Found, type = "h", lwd = 3, xlab = "Percentage of Sphagnum Moss", ylab = "Number of Exuvia Recorded")

read.csv("peaksex.csv")
sex<- read.csv("peaksex.csv")
barplot(peak2$Week, peak2$Exuvia.Found )
plot(peak2$Exuvia.Found ~ peak2$Week, type = "o", pch = 16, xlab = "Week of the Year", ylab = "Number of Exuvia Recorded")
plot(sex$Week, sex$Female.Exuvia, type = "o", pch = 16, xlab = "Week of the Year", ylab = "Number of Female Exuvia Recorded")
plot(sex$Week, sex$Male.Exuvia, type = "o", pch = 16, xlab = "Week of the Year", ylab = "Number of Male Exuvia Recorded")
plot(dataset$Date[which(dataset$Exuvia.Found == "1" )])
#date seems slightly wrong on graph, jumbled up
#peak in emergence on 18/05 and 31/05
plot(peak$Week, peak$Exuvia.Found)
barplot(peak$Week, peak$Exuvia.Found)
barplot(peak$Exuvia.Found, peak$Week)

as.Date
function (x, ...) 
  UseMethod("as.Date")
<bytecode: 0x0000000004f55b60>
  <environment: namespace:base>
  > ?as.Date
> date <- as.Date(peak$Date, format = "%d/%m/%Y")
> date
[1] "2017-05-10" "2017-05-12" "2017-05-15" "2017-05-18" "2017-05-23" "2017-05-26"
[7] "2017-05-31" "2017-06-02" "2017-06-05" "2017-06-12" "2017-06-16" "2017-06-21"
> 
  > plot(date, peak$Exuvia.Found )
> 
  #plot own x axis to include all dates

plot(dataset$Date[which(dataset$Exuvia.Found == "1")], dataset$Date[which(dataset$Sex == "m")])
plot(dataset$Date[which(dataset$Exuvia.Found == "1")], dataset$Date[which(dataset$Sex == "f")])
#no idea why these wont work
plot(dataset$Date[which(dataset$Sex == "f")])
plot(dataset$Date[which(dataset$Sex == "m")])
#seems to be a difference in peak for male v female
##once error is sorted with how the graph is displaying date, this can be read

boxplot(dataset$Exuvia.Found[which(dataset$Veg.Species == "Soft Rush")],dataset$Exuvia.Found[which(dataset$Veg.Species == "No Vegetation")], dataset$Exuvia.Found[which(dataset$Veg.Species == "Common cotton grass")])
#equal numbers found on cotton grass and soft rush

read.csv("rgraph.csv")
rgraph <- read.csv("rgraph.csv")
plot(rgraph$sex[which(rgraph$sex == "f")], rgraph$week)
plot(rgraph$week[which(rgraph$sex == "f")])
#grpah wrong way round

plot(rgraph$sex ~ rgraph$week)
#why wont it give me a bar plot
barplot(rgraph$sex[which(rgraph$sex =="2")], rgraph$week)
#changing graph so male is 1 and female is 2
barplot(rgraph$sex[which(rgraph$sex =="2")], rgraph$week)
#graph looks rubbish

read.csv("sex.csv")
r <- read.csv("r.csv")
barplot(r$exuvia ~ r$week)
#argggggh
View(sex)
x <- cbind(sex$Week,sex$Week)
y <- cbind(sex$Male.Exuvia,sex$Female.Exuvia)
matplot(x,y,type="p", pch = 16, bty = "n", xlab = "Week of the Year", ylab = "Number of Exuvia Recorded")
legend("topright", legend=c("Male", "Female"), pch=16, col=c("black","red"))
