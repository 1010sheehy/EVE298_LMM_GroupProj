setwd("~/Desktop/EVE 298/FINAL Project")
library("rlang")
library("ggplot2")
library("nlme")
library("lme4")
library("MASS")

infantagg <- read.csv("Infant_Conflict_Data_W_O_NAs.csv", header=T)
head(infantagg)
tail(infantagg)
str(infantagg)

infantagg$Name <- as.factor(infantagg$Name)
infantagg$Matriline <- as.factor(infantagg$Matriline)
infantagg$Sex <-as.factor(infantagg$Sex)
infantagg$Mom.mpvspp <- as.factor(infantagg$Mom.mpvspp)

#exploring outcome variables (initiator)

hist(infantagg$Inv.i.9)
hist(infantagg$Inv.i.13.Count)
hist(infantagg$Inv.i.15.Count)

dotchart(infantagg$Inv.i.9)
dotchart(infantagg$Inv.i.13.Count) 
dotchart(infantagg$Inv.i.15.Count)

#exploring outcome variables (recipient)
#for now going to work with initiator and we can come back to recipient

hist(infantagg$Inv.r.9)
hist(infantagg$Inv.r.13.Count)
hist(infantagg$Inv.r.15.Count)

dotchart(infantagg$Inv.r.9)
dotchart(infantagg$Inv.r.13.Count)
dotchart(infantagg$Inv.r.15.Count)

#exploring predictor variables

#infant exposure to aggression on mom
hist(infantagg$Exp.1to4.i) 
hist(infantagg$Exp.1to4.r)

dotchart(infantagg$Exp.1to4.i)
dotchart(infantagg$Exp.1to4.r)

#overall aggression of mom
hist(infantagg$Mom.inv.1to4.i) 
hist(infantagg$Mom.inv.1to4.r)

dotchart(infantagg$Mom.inv.1to4.i)
dotchart(infantagg$Mom.inv.1to4.r)

#fixed effects
#Sex
boxplot(infantagg$Inv.i.15.Count~infantagg$Sex) #looks like variance isn't separated, GOOD
#possibly different between sexes

#First time mom vs. Experienced mom
boxplot(infantagg$Inv.i.15.Count~infantagg$Mom.mpvspp)
#similar deal, looks like variance overlaps, looks good (maybe not a sig. diff)

#Birthtime
plot(infantagg$Inv.i.15.Count~infantagg$Birth.time) 
ggplot(infantagg, aes(x = Birth.time, y = Inv.i.15.Count, col=Matriline)) +
  geom_point()

#Rank in Bins
hist(infantagg$Mom.rank.bins1to3)
hist(infantagg$Mom.rank.bins1to4)

hist(infantagg$Mom.DC.bins1to3)
hist(infantagg$Mom.DC.bins1to4)

#Raw rank and DC
hist(infantagg$MomPercRank)
hist(infantagg$MomPercDC)

#now lets try some models

mod.full <- lme(Inv.i.15.Count ~ Exp.1to4.i + Mom.inv.1to4.i + Sex 
                + Mom.mpvspp + Birth.time + MomPercRank + MomPercDC 
                + offset(Inv.Obs.Days.i.15), 
                random = ~1|Matriline, 
                data = infantagg)

summary(mod.full) #AIC: 112.86
plot(resid(mod.full)) #residuals dont look like a nice starry night to me


mod.full_glmer <- glmer(Inv.i.15.Count ~ Exp.1to4.i + Mom.inv.1to4.i + Sex 
                + Mom.mpvspp + Birth.time + MomPercRank + MomPercDC + (1|Matriline) 
                + offset(Inv.Obs.Days.i.15),
                family = "poisson",
                data = infantagg)
'got an error message here that the model failed to converge'
'rescale variables?'
summary(mod.full_glmer) #AIC - 112.86
plot(mod.full_glmer)
'again, residuals do not look like  a starry night to me'

#check to see if poisson model residuals are overdispersed
Res <- resid (mod.full_glm, type = "pearson")
N <- nrow (infantagg)
p <- length (coef(mod.full_glm))
Dispersion <- sum (Res^2)/(N-p)
Dispersion
#7.8 , outcome is overdispersed, move to a negative binomial model

library(MASS)

mod.full_glm.nb <- glm.nb(Inv.i.15.Count ~ Exp.1to4.i + Mom.inv.1to4.i + Sex 
                    + Mom.mpvspp + Birth.time + MomPercRank + MomPercDC
                    + offset(Inv.Obs.Days.i.15),
                    data = infantagg)
summary(mod.full_glm.nb)
plot(mod.full_glm.nb)
#AIC 138.45

Res <- resid (mod.full_glm.nb, type = "pearson")
N <- nrow (infantagg)
p <- length (coef(mod.full_glm.nb))
Dispersion <- sum (Res^2)/(N-p)
Dispersion
#2.311 - still overdispersed (!)


###### START OVER WITH MODELS ####
'start at the beginning and build models up to see 
effects/variance explained of each individual predictor'


#null models first with a random effect
NullModel <- lme(Inv.i.15.Count ~ 1 + offset(Inv.Obs.Days.i.15),
                 random = ~1|Matriline,
                 data=infantagg)
summary(NullModel) 
'AIC - 130.04'
plot(resid(NullModel))

#now a null model with no random effect
NullModelNoRandom <- lm(Inv.i.15.Count ~ 1 + offset(Inv.Obs.Days.i.15),
                        data=infantagg)
summary(NullModelNoRandom)
plot(resid(NullModelNoRandom))
'AIC - 128.18 - looks like random effect of matriline does not improve the model, makes it worse'
'do not include random effects in later models'
AIC(NullModelNoRandom)

# now start looking at fixed effects - first exposure to aggression on Mom
Exposure.initiator <- lm(Inv.i.15.Count ~ Exp.1to4.i + offset(Inv.Obs.Days.i.15),
                         data = infantagg)
summary(Exposure.initiator) 
'shows sig. effect of exposure with 0.03 pvalue and beta of 5.4'
plot(resid(Exposure.initiator))
AIC(Exposure.initiator) 
'125.318'

plot(infantagg$Exp.1to4.i~infantagg$Inv.i.15.Count)
'looks like an increasing trend plotting raw data without offset obs variable'

#now looking at next fixed effect, Mom overall involvement
Mom.involve <- lm(Inv.i.15.Count ~ Mom.inv.1to4.i + offset(Inv.Obs.Days.i.15),
                  data=infantagg)
summary(Mom.involve) 
'trend here (0.08) effect of overall involve with beta of 2.0'
plot(infantagg$Mom.inv.1to4.i~infantagg$Inv.i.15.Count)
plot(resid(Mom.involve))
AIC(Mom.involve) 
'126.95'

#next fixed effect of first-time mom or not
Mom.Status <- lm(Inv.i.15.Count ~Mom.mpvspp + offset(Inv.Obs.Days.i.15),
                 data=infantagg)
summary(Mom.Status) 
'no sig. effects'
boxplot(infantagg$Inv.i.15.Count~infantagg$Mom.mpvspp)
plot(resid(Mom.Status))
AIC(Mom.Status) 
'129.27'

#next fixed effect of sex
InfantSex <- lm(Inv.i.15.Count ~ Sex + offset(Inv.Obs.Days.i.15),
                data=infantagg)
summary(InfantSex) 
'no sig. effects'
boxplot(infantagg$Inv.i.15.Count ~ infantagg$Sex)
plot(resid(InfantSex))
AIC(InfantSex) 
'129.32'

#next fixed effect of birthtime 
InfantBirthTime <- lm(Inv.i.15.Count ~ Birth.time + offset(Inv.Obs.Days.i.15),
                      data=infantagg)
summary(InfantBirthTime)
plot(resid(InfantBirthTime))
plot(infantagg$Inv.i.15.Count ~ infantagg$Birth.time)
AIC(InfantBirthTime)     
'130.18' 

#next fixed effect of mom Perc rank
RankOfMom <- lm(Inv.i.15.Count ~ MomPercRank + offset(Inv.Obs.Days.i.15),
                data = infantagg)
summary(RankOfMom)
'sig results with a 0.013 p value and beta of 11.56'
plot(resid(RankOfMom))
plot(infantagg$Inv.i.15.Count ~ infantagg$MomPercRank)
AIC(RankOfMom)
'123.38'

#next fixed effect of mom Perc DC
DCOfMom <- lm(Inv.i.15.Count ~ MomPercDC + + offset(Inv.Obs.Days.i.15),
              data=infantagg)
summary(DCOfMom)
plot(resid(DCOfMom))
plot(infantagg$Inv.i.15.Count ~ infantagg$MomPercDC)
AIC(DCOfMom)
'127.73'

'after some basic linear model explorations, it seems like the variables 
that explain the most variation in the outcome variable (involvement at month 15)
are:
Exposure.initiator
RankOfMom

Null : 128.18
Exposure : 125.318
MomRank : 123.38
'

###lets create a model together to see if it is better (and if the two variables 
###are colinear)

CombinedModel <- lm(Inv.i.15.Count ~ Exp.1to4.i + Mom.inv.1to4.i + offset(Inv.Obs.Days.i.15),
                    data=infantagg)
summary(CombinedModel)
library(performance)
check_collinearity(CombinedModel) 
'states low collinearity'
plot(resid(CombinedModel))
'not sure what to do about this'
AIC(CombinedModel)
#124.54


#### NOW LETS TRY RECIPIENT

#null models first with a random effect
NullModel.recip <- lme(Inv.r.15.Count ~ 1 + offset(Inv.Obs.Days.r.15),
                 random = ~1|Matriline,
                 data=infantagg)
summary(NullModel.recip) 
'AIC - 156.90'
plot(resid(NullModel.recip))

#Now null model without random effect
NullModelNoRandom.recip <- lm(Inv.r.15.Count ~ 1 + offset(Inv.Obs.Days.r.15),
                       data=infantagg)
summary(NullModelNoRandom.recip) 
AIC(NullModelNoRandom.recip)
'159.82'
'weird, here it seems like the matriline random effect improves the model'

#Now try fixed effects along with the random effect
Exposure.recip <- lme(Inv.r.15.Count ~ Exp.1to4.r + offset(Inv.Obs.Days.r.15),
                      random = ~ 1|Matriline,
                         data = infantagg)
summary(Exposure.recip) 
'sig effect with p-value of 0.008 and a beta of 16.17'
plot(resid(Exposure.recip))
plot(infantagg$Inv.r.15.Count ~ infantagg$Exp.1to4.r)
AIC(Exposure.recip) 
'146.12'

Mom.involve.recip <- lme(Inv.r.15.Count ~ Mom.inv.1to4.r + offset(Inv.Obs.Days.r.15),
                        random = ~ 1|Matriline,
                  data=infantagg)
summary(Mom.involve.recip) 
'sig effect with p-value of (0.012) and beta of 6.4'
plot(infantagg$Mom.inv.1to4.r~infantagg$Inv.r.15.Count)
plot(resid(Mom.involve.recip))
AIC(Mom.involve.recip) 
'148.65'

#next fixed effect of first-time mom or not
Mom.Status.recip <- lme(Inv.r.15.Count ~Mom.mpvspp + offset(Inv.Obs.Days.r.15),
                        random = ~ 1|Matriline,
                 data=infantagg)
summary(Mom.Status.recip) 
'look significant in summary, but doesnt when plotted??'
boxplot(infantagg$Inv.i.15.Count~infantagg$Mom.mpvspp)
plot(resid(Mom.Status.recip))
AIC(Mom.Status.recip) 
'148.38'

#next fixed effect of sex
InfantSex.Recip <- lme(Inv.r.15.Count ~ Sex + offset(Inv.Obs.Days.r.15),
                      random = ~ 1|Matriline,
                data=infantagg)
summary(InfantSex.Recip) 
'no sig. effects'
boxplot(infantagg$Inv.i.15.Count ~ infantagg$Sex)
plot(resid(InfantSex.Recip))
AIC(InfantSex.Recip) 
'152.92'

#next fixed effect of birthtime 
InfantBirthTime.recip <- lme(Inv.r.15.Count ~ Birth.time + offset(Inv.Obs.Days.r.15),
                            random = ~ 1|Matriline,
                      data=infantagg)
summary(InfantBirthTime.recip)
plot(resid(InfantBirthTime.recip))
plot(infantagg$Inv.i.15.Count ~ infantagg$Birth.time)
AIC(InfantBirthTime.recip)     
'161.57' 

#next fixed effect of mom Perc rank
RankOfMom.Recip <- lme(Inv.r.15.Count ~ MomPercRank + offset(Inv.Obs.Days.r.15),
                      random = ~ 1|Matriline,
                data = infantagg)
summary(RankOfMom.Recip)
'no sig results'
plot(resid(RankOfMom.Recip))
plot(infantagg$Inv.r.15.Count ~ infantagg$MomPercRank)
AIC(RankOfMom.Recip)
'150.98'

#next fixed effect of mom Perc DC
DCOfMom.Recip <- lme(Inv.r.15.Count ~ MomPercDC + + offset(Inv.Obs.Days.r.15),
                     random = ~ 1|Matriline,
              data=infantagg)
summary(DCOfMom.Recip)
'no sig results'
plot(resid(DCOfMom.Recip))
plot(infantagg$Inv.r.15.Count ~ infantagg$MomPercDC)
AIC(DCOfMom.Recip)
'147.26'

'after doing some simple linear models with the recipient counts, I found the 
following: 
NullwithRandomEffect: 156.90
Exposure: 146.12
MomInvolve : 148.65
'