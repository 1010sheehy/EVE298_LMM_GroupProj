setwd("~/Desktop/EVE 298/FINAL Project")
library("rlang")
library("ggplot2")

infantagg <- read.csv("Infant_Conflict_Data_W_O_NAs.csv", header=T)
head(infantagg)
tail(infantagg)
str(infantagg)

infantagg$Name <- as.factor(infantagg$Name)
class(infantagg$Name)
infantagg$Matriline <- as.factor(infantagg$Matriline)
class(infantagg$Matriline)
infantagg$Sex <-as.factor(infantagg$Sex)
infantagg$Mom.mpvspp <- as.factor(infantagg$Mom.mpvspp)

#exploring outcome variables (initiator)

hist(infantagg$Inv.i.9)
hist(infantagg$Inv.i.13)
hist(infantagg$Inv.i.15)

dotchart(infantagg$Inv.i.9)
dotchart(infantagg$Inv.i.13) #looks like good spread here, seems to be the best 
dotchart(infantagg$Inv.i.15)

#exploring outcome variables (recipient)
#for now going to work with initiator and we can come back to recipient

hist(infantagg$Inv.r.9)
hist(infantagg$Inv.r.13)
hist(infantagg$Inv.r.15)

dotchart(infantagg$Inv.r.9)
dotchart(infantagg$Inv.r.13)
dotchart(infantagg$Inv.r.15)

#exploring predictor variables

#infant exposure to aggression on mom
hist(infantagg$Exp.1to4.i) #looks kinda normal withou the outlier
hist(infantagg$Exp.1to4.r)

dotchart(infantagg$Exp.1to4.i)
dotchart(infantagg$Exp.1to4.r)

#overall aggression of mom
hist(infantagg$Mom.inv.1to4.i) #looks pretty normal aside from the outlier
hist(infantagg$Mom.inv.1to4.r)

dotchart(infantagg$Mom.inv.1to4.i)
dotchart(infantagg$Mom.inv.1to4.r)

#fixed effects
#Sex
infantagg$Sex <- as.integer(infantagg$Sex)
boxplot(infantagg$Inv.i.13~infantagg$Sex) #looks like variance isn't separated, GOOD
#possibly different between sexes

#First time mom vs. Experienced mom
boxplot(infantagg$Inv.i.13~infantagg$Mom.mpvspp)
#similar deal, looks like variance overlaps, looks good (maybe not a sig. diff)

#Birthtime
plot(infantagg$Inv.i.13~infantagg$Birth.time) # maybe a positive relationship?
ggplot(infantagg, aes(x = Birth.time, y = Inv.i.13, col=Matriline)) +
  geom_point()

#Rank in Bins
hist(infantagg$Mom.rank.bins1to3)
hist(infantagg$Mom.rank.bins1to4)

hist(infantagg$Mom.DC.bins1to3)
hist(infantagg$Mom.DC.bins1to4)
