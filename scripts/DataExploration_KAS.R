###
### Infant - Aggression Data Exploration
###

#   Goals:
#   - Propose a general question
#   - Decide on response variable --> infant involvement in aggression at 13 or 15 months
#   - Identify and describe potential pitfalls

#   Question: Are there variables which predict the involvement of infants in aggression later in life?

'Packages'
library(tidyverse)
library(ggplot2)

'Import Data using GitHub permalink to "master" branch'
monkey_agg <- read.csv("~/Desktop/R_Projects/EVE298_LMM_GroupProj/data/Infant_Conflict_Data_W_O_NAs_20210519.csv", header = TRUE)
head(monkey_agg)
str(monkey_agg)
summary(monkey_agg)

# changing matriline to factor
monkey_agg$fMatriline <- as.factor(monkey_agg$Matriline)
monkey_agg$fInfant.ID <- as.factor(monkey_agg$Infant.ID)
monkey_agg$fMom.ID <- as.factor(monkey_agg$Mom.ID)
monkey_agg$fSex <- as.factor(monkey_agg$Sex)
monkey_agg$fMom.mpvspp <- as.factor(monkey_agg$Mom.mpvspp)

### Data Exploration ----
# > Predictor Variables ----
# .. Mom's aggressive interactions ---- 
# Looking at distribution of mom's involvement with aggression (w/ baby)
#  - Noticed that the mean ratio was below 1 for both instigating and receiving
#  - This means less than one (or two?) aggressive interactions per day (within observation period)

hist(monkey_agg$Mom.inv.1to4.i) # skewed because of one outlier at 3.5+ aggressive interactions per day
dotchart(monkey_agg$Mom.inv.1to4.i) # not a good starry night
hist(monkey_agg$Mom.inv.1to4.r) # also skewed, but not just because of one outlier (like with Mom initiating)
dotchart(monkey_agg$Mom.inv.1to4.r) # looks ok!
'the outliers for Mom.inv.1to4.i and Mom.inv.1to4.r are not the same
individuals: Tamar (mom of Terra) and Cambridge (mom of Camden)'
'T group is the highest ranked matriline, but Tamar was at the bottom of the
matriline heirarchy. Per Josie observations, she was a "real pain in the ass"
and was always starting aggression (and receiving it).'

# .. Infant exposure to aggression ----

# Initiator
hist(monkey_agg$Exp.1to4.i) # skewed because of one outlier at 1.5 aggressive interactions per day
dotchart(monkey_agg$Exp.1to4.i) # ok starry night, but one outlier

# Receiver
hist(monkey_agg$Exp.1to4.r) # also skewed, but not just because of one outlier, just descending frequencies from 0 to 1.2
dotchart(monkey_agg$Exp.1to4.r) # looks ok, except for that one outlier
'Outlier for Exp.1to4.i is Terra, child of Tamar
Outlier for Exp.1to4.r Quidditch, child of Queena'

'Per Josie: The Q matriline is the lowest ranked, but Queena was the daughter of the alpha
male, so she was hanging out with him often and recieved a lot of aggression
as a result.'

# .. Mom rank (MomPercRank) and Certainty of Rank (MomPerkDC) ----

# Rank
hist(monkey_agg$MomPercRank) # slightly skewed, but pretty ok looking
dotchart(monkey_agg$MomPercRank) # hmm kind of a reverse trumpet plot

# Rank Certainty
hist(monkey_agg$MomPercDC) # ok-ish? Kind of skewed.
dotchart(monkey_agg$MomPercDC) # ok-ish. Kind of a funny cluster at the higher end of certainty

# .. Outliers ----
'In order to deal with the outliers, run the model with outlier AND without to
see if outlier is driving changes. If no difference in parameters, outlier is
not driving trend. If different, we can say that outlier is driving the effect.'

# > Response Variables ----

# .. Infant involvement in aggression at 10, 13, and 15 months ----

# 15 months
hist(monkey_agg$Inv.i.15) # ok, slightly skewed
dotchart(monkey_agg$Inv.i.15) # also ok, but two outliers

# 13 months
hist(monkey_agg$Inv.i.13) # skewed
dotchart(monkey_agg$Inv.i.13) # good! Nice even distribution.
hist(monkey_agg$Inv.i.10) # skewed
dotchart(monkey_agg$Inv.i.10) # whaaat? Lots of zeros, then a kind of upward trend?
'Month 10 was pulled at a separate time, so something happened on the human end
that caused a ton of zero values.

MONTH 15 is the response variable of choice because it has a good spread and
less zero values.'

# > Other exploration ----
# .. Colinearity ----

pairs(monkey_agg[, c("fMatriline",
                     "MomPercRank",
                     "MomPercDC",
                     "Time.away.prop.M1toM4",
                     "Exp.1to4.i",
                     "Exp.1to4.r",
                     "Birth.time",
                     "Inv.Obs.Days.i.15",
                     "Inv.r.13.Count")])
'There are multiple pairs which show "clumping" or linear-looking trends.
Some make total sense, like Matriline-Rank, but others are more interesting.
For example, Birth.time-Exp.1to4.r seems to have a relationship.
Perhaps of most interest are the trends between Exp.1to4.i, rank, and rank
certianty. These patterns make sense since lower ranked, more uncertain moms 
are likely to initiate aggression. In other words, they are less certain of
their rank and are willing to initiate fights to test them.'

pairs(monkey_agg[,c("Exp.1to4.i",
                 "Mom.inv.1to4.i")])
'Based on these, we may want to actually consider path analysis. This is
because not ALL of these are necessarily predicting variance. In fact, one may
predict another, and that one another, and so on and so forth in a long chain.'

### Building the Model ----

'Because we have so few data points, our model can only have a few predictor
variables (minimum 3x more data points than predictors, 10x is more reasonable)
we can only include 1-2 fixed effects and one random effect.'

'Because matriline is a random effect, we only estimate one parameter (despite
having multiple levels within the matriline). It is ok that there are some
matrilines with multiple offspring and some with only one. This is because for
random effects '









