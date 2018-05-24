#temperature/exuvia analysis
setwd("C:/Users/rache/Desktop")
read.csv("logdatar.csv")
dataset <- read.csv("logdatar.csv")
head(dataset)

#scatter plot minimum temp against emergence
plot(dataset$mintemp, dataset$exuviafound)
plot(dataset$exuviafound, dataset$mintemp)

#checking correlations
cor(dataset$mintemp, dataset$exuviafound, use = 'complete.obs')
#exuvia is not numeric so correlation will not work.
##changing yes/no to 1/0
cor(dataset$mintemp, dataset$exuviafound, use = 'complete.obs')
cor.test(dataset$exuviafound, dataset$mintemp)
#pvalaue is greater than 0.05 so suggests that minimum temperature is not a significant factor

#scatter plot maximum temp against emergence
plot(dataset$maxtemp ~ dataset$exuviafound)
plot(dataset$exuviafound ~ dataset$maxtemp)

#checking correlations
cor(dataset$maxtemp, dataset$exuviafound, use = 'complete.obs')
cor.test(dataset$exuviafound, dataset$maxtemp)
#pvalaue is greater than 0.05 so suggests that minimum temperature is not a significant

#scatter plot average temp
plot(dataset$averagetemp ~ dataset$exuviafound)
plot(dataset$exuviafound ~ dataset$averagetemp)

#checking correlations
cor(dataset$averagetemp, dataset$exuviafound, use = 'complete.obs')
cor.test(dataset$exuviafound, dataset$averagetemp)
#pvalaue is greater than 0.05 so suggests that minimum temperature is not a significant

#When using summary data from all dataloggers for each data,
##it appears that neither the min, max or average temp affects emergence

#We will now try using only the data loggers long the exuvia transect.
##These are numbers 5,6,7,8

read.csv("templogr.csv")
dataset2 <- read.csv("templogr.csv")
head(dataset2)

#scatter plot average temp
plot(dataset2$averagetemp ~ dataset2$exuviafound)
plot(dataset2$exuviafound ~ dataset2$averagetemp)

#checking correlations
cor(dataset2$averagetemp[-4], dataset2$exuviafound[-4], use = 'complete.obs')
cor.test(dataset2$exuviafound[-4], dataset2$averagetemp[-4])
#pvalaue is greater than 0.05 so suggests that minimum temperature is not a significant

#scatter plot maximum temp against emergence
plot(dataset2$maxtemp ~ dataset2$exuviafound)
plot(dataset2$exuviafound ~ dataset2$maxtemp)

#checking correlations
cor(dataset2$maxtemp, dataset2$exuviafound, use = 'complete.obs')
cor.test(dataset2$exuviafound, dataset2$maxtemp)
#pvalaue is greater than 0.05 so suggests that minimum temperature is not a significant

#scatter plot min temp
plot(dataset2$mintemp ~ dataset2$exuviafound)
plot(dataset2$exuviafound ~ dataset2$mintemp)

#checking correlations
cor(dataset2$mintemp, dataset2$exuviafound, use = 'complete.obs')
cor.test(dataset2$exuviafound, dataset2$mintemp)
#pvalaue is greater than 0.05 so suggests that minimum temperature is not a significant

> max(dataset$exuviafound)
[1] 2
> which.max(dataset$exuviafound)
[1] 4
> cor(dataset$mintemp[-4], dataset$exuviafound[-4], use = 'complete.obs')
[1] 0.1112342

#All analysis indicates that water temperature is not a siginificant factor for emergence
##However, we are working with a small number of exuvia
###More work would need to be done, preferably on a population with greater numbers
####More data= better accuracy in results