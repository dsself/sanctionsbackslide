#install.packages("Matching", dependencies=TRUE)
#install.packages("rgenoud")
setwd("C:/Users/Darin/Documents/sanctionsbackslide/Spectrum")
library(Matching)
library(rgenoud)
library(dplyr)
library(stargazer)
library(readr)

df <- read_csv("SanctionsFinal.csv") %>% 
  mutate(pop1 = log(pop1)) %>% 
  filter(!is.na(GDP_UN)) %>% 
  mutate(deliniation = ifelse(polity2 >= 6, 1, ifelse(polity2 >= 2 & polity2 < 6, 2, ifelse(polity2 < 2 & polity2 > -2, 3, ifelse(polity2 <= -2 & polity2 >= -5, 4, ifelse(polity2 <= -6, 5, NA)))))) 


#polity2 - measure of democracy from -10 to 10
#CCode - country identifier 
#sanctions - bianary for whether the country was sanctioned that year
#sanctionsl1 - 1 year lag of sanctions
#dpolity - difference of polity2
#S - measure of selectorate (Logic of Political Survival)
#W - measure of winning coalition with S
#ndpolity - binary if dpolity is negative
#ds - difference of S
#dw - difference of W
#lagus - lagged measure of sanction if sanctions were imposed by the U.S. Quasi continuous with 0 being no sanctions up to 3 being severe sanctions
#lagun - same as above but if the sanctioner is the UN
#lagpopgled - not my variable so somewhat unclear what it is
#laggdpchng - lagged % change in GDP
#GDP_UN - measure of GDP
#pop1 - population of country
#logpop - ln of pop1
#MEast - dummy for if in the Middle East
#demaut - unsure - need to track down which dataset this came from
#lGDP_UN ln GDP_UN
#dpolityb - binary for if there was any change in polity2
#Pdpolity - probability of change in polity2 given the polity score
#menergy - mean of imputed energy output
#senergy - std dev of imputed energy output
#mindustry - mean of imputed values for how much of the economic production is industrial
#sindustry - std dev of above
#murban - mean of imputed values of how much of the pop lives in urban areas
#surban - std dev of above
#deliniation - user selected segments of polity2 for how "democratic" a country is

df$murban[df$murban < 0] <- 0

####Use GenMatch to do matching on entire sample####
#need to create dataframe of what will be the covariates
X <- select(df, GDP_UN, pop1, menergy, mindustry, murban)

#create matrix of what the algorithm will use to balance
BalanceMatrix <- cbind(df$GDP_UN, df$pop1, df$menergy, df$mindustry, df$murban, I(df$GDP_UN*df$pop1), I(df$GDP_UN*df$menergy),
                       I(df$GDP_UN*df$mindustry), I(df$GDP_UN*df$murban), I(df$pop1*df$murban), I(df$murban*df$mindustry),I(df$pop1*df$mindustry))

#run algorith - pop.size here is very low in order to reduce computational time. 
#as pop.size increases the algorithm will iterate the # of times user specifies but at great time costs
#I found that the ATT doesn't really change much as we alter this but the p-value is reduced as it increases
#print.level just prevents it from printing the output of balancing
#unif.seed and int.seed sets the seed
gen1 <- GenMatch(Tr = df$sanctions, X = X, BalanceMatrix = BalanceMatrix, pop.size = 10, print.level = 0, unif.seed=3392, int.seed=8282)

#perform the estimation using the matched sample
mgen1 <- Match(Y = df$polity2, Tr = df$sanctions, X = X, Weight.matrix = gen1)

summary(mgen1)

#####Create QQ-plots to look at fit of covariates
   qqplot(df$GDP_UN[mgen1$index.treated], df$GDP_UN[mgen1$index.control],
         xlab = "Treated", ylab = "Control", main = "QQ Plot of Distribution of GDP")

qqplot(df$pop1[mgen1$index.treated], df$pop1[mgen1$index.control],
       xlab = "Treated", ylab = "Control", main = "QQ Plot of Distribution of GDP")

qqplot(df$menergy[mgen1$index.treated], df$menergy[mgen1$index.control],
       xlab = "Treated", ylab = "Control", main = "QQ Plot of Distribution of GDP")

qqplot(df$mindustry[mgen1$index.treated], df$mindustry[mgen1$index.control],
       xlab = "Treated", ylab = "Control", main = "QQ Plot of Distribution of GDP")

qqplot(df$murban[mgen1$index.treated], df$murban[mgen1$index.control],
       xlab = "Treated", ylab = "Control", main = "QQ Plot of Distribution of GDP")

#####Visualize distribution of probability of change in polity####
library(ggplot2)
library(scales)

dp <- df %>% 
  group_by(Year, polity2) %>% 
  filter(!is.na(dpolityb)) %>% 
  count(dpolityb, polity2) %>% 
  group_by(polity2) %>% 
  mutate(Pdpolity = n/sum(n)) %>% 
  filter(dpolityb == 1) %>% 
  select(polity2, Pdpolity)

ggplot(dp, aes(x = polity2, y = Pdpolity)) +
  geom_bar(stat="identity") +
  xlab("Polity Score") +
  ylab("Probability of Change in Polity Score") +
  ggtitle("Distribution of Probability of Change in Polity Score") +
  scale_y_continuous(labels=percent)

#####Use K-means to identify clusters in the probability of change in polity2####
k1 <- select(df, polity2, Pdpolity)
set.seed(2)
fit1 <- kmeans(k1, 5)
aggregate(k1,by=list(fit1$cluster),FUN=mean) %>% 
  arrange(-polity2) 

#link to main dataframe
k1 <- data.frame(k1, fit1$cluster) %>% 
  mutate(fit1.cluster = plyr::mapvalues(fit1.cluster, from = c(3, 1, 4, 5, 2), to = c(1, 2, 3, 4, 5))) %>% 
  select(cluster = fit1.cluster) 

df <- cbind(df, k1) 

  
#Select on highest (cluster == 1) level in k-means
d1 <- filter(df, cluster == 1)

X <- select(d1, GDP_UN, pop1, menergy, mindustry, murban) 

BalanceMatrix <- cbind(d1$GDP_UN, d1$pop1, d1$menergy, d1$mindustry, d1$murban, I(d1$GDP_UN*d1$pop1), I(d1$GDP_UN*d1$menergy),
                       I(d1$GDP_UN*d1$mindustry), I(d1$GDP_UN*d1$murban), I(d1$pop1*d1$murban), I(d1$murban*d1$mindustry),I(d1$pop1*d1$mindustry))

gen1 <- GenMatch(Tr = d1$sanctions, X = X, BalanceMatrix = BalanceMatrix, pop.size = 10, print.level = 0, unif.seed=3392, int.seed=8282)

mgen1 <- Match(Y = d1$polity2, Tr = d1$sanctions, X = X, Weight.matrix = gen1)
print(summary(mgen1))

#Second Highest (cluster == 2) level 
d1 <- filter(df, cluster == 2)

X <- select(d1, GDP_UN, pop1, menergy, mindustry, murban) 

BalanceMatrix <- cbind(d1$GDP_UN, d1$pop1, d1$menergy, d1$mindustry, d1$murban, I(d1$GDP_UN*d1$pop1), I(d1$GDP_UN*d1$menergy),
                       I(d1$GDP_UN*d1$mindustry), I(d1$GDP_UN*d1$murban), I(d1$pop1*d1$murban), I(d1$murban*d1$mindustry),I(d1$pop1*d1$mindustry))

gen1 <- GenMatch(Tr = d1$sanctions, X = X, BalanceMatrix = BalanceMatrix, pop.size = 10, print.level = 0, unif.seed=3392, int.seed=8282)

mgen1 <- Match(Y = d1$polity2, Tr = d1$sanctions, X = X, Weight.matrix = gen1)
print(summary(mgen1))

  
  #Mid Levels (cluster == 3) level 
  d1 <- filter(df, cluster == 3)

X <- select(d1, GDP_UN, pop1, menergy, mindustry, murban) 

BalanceMatrix <- cbind(d1$GDP_UN, d1$pop1, d1$menergy, d1$mindustry, d1$murban, I(d1$GDP_UN*d1$pop1), I(d1$GDP_UN*d1$menergy),
                       I(d1$GDP_UN*d1$mindustry), I(d1$GDP_UN*d1$murban), I(d1$pop1*d1$murban), I(d1$murban*d1$mindustry),I(d1$pop1*d1$mindustry))

gen1 <- GenMatch(Tr = d1$sanctions, X = X, BalanceMatrix = BalanceMatrix, pop.size = 10, print.level = 0, unif.seed=3392, int.seed=8282)

mgen1 <- Match(Y = d1$polity2, Tr = d1$sanctions, X = X, Weight.matrix = gen1)
print(summary(mgen1))

  
  #Second Lowest Levels (cluster == 4) level 
  d1 <- filter(df, cluster == 4)

X <- select(d1, GDP_UN, pop1, menergy, mindustry, murban) 

BalanceMatrix <- cbind(d1$GDP_UN, d1$pop1, d1$menergy, d1$mindustry, d1$murban, I(d1$GDP_UN*d1$pop1), I(d1$GDP_UN*d1$menergy),
                       I(d1$GDP_UN*d1$mindustry), I(d1$GDP_UN*d1$murban), I(d1$pop1*d1$murban), I(d1$murban*d1$mindustry),I(d1$pop1*d1$mindustry))

gen1 <- GenMatch(Tr = d1$sanctions, X = X, BalanceMatrix = BalanceMatrix, pop.size = 10, print.level = 0, unif.seed=3392, int.seed=8282)

mgen1 <- Match(Y = d1$polity2, Tr = d1$sanctions, X = X, Weight.matrix = gen1)
print(summary(mgen1))

  
  #Lowest Levels (cluster == 5) level 
d1 <- filter(df, cluster == 5)

X <- select(d1, GDP_UN, pop1, menergy, mindustry, murban) 

BalanceMatrix <- cbind(d1$GDP_UN, d1$pop1, d1$menergy, d1$mindustry, d1$murban, I(d1$GDP_UN*d1$pop1), I(d1$GDP_UN*d1$menergy),
                       I(d1$GDP_UN*d1$mindustry), I(d1$GDP_UN*d1$murban), I(d1$pop1*d1$murban), I(d1$murban*d1$mindustry),I(d1$pop1*d1$mindustry))

gen1 <- GenMatch(Tr = d1$sanctions, X = X, BalanceMatrix = BalanceMatrix, pop.size = 10, print.level = 0, unif.seed=3392, int.seed=8282)

mgen1 <- Match(Y = d1$polity2, Tr = d1$sanctions, X = X, Weight.matrix = gen1)
print(summary(mgen1))

####Split on polity = 0####
#Above 0
d1 <- filter(df, polity2 > 0)

X <- select(d1, GDP_UN, pop1, menergy, mindustry, murban) 

BalanceMatrix <- cbind(d1$GDP_UN, d1$pop1, d1$menergy, d1$mindustry, d1$murban, I(d1$GDP_UN*d1$pop1), I(d1$GDP_UN*d1$menergy),
                       I(d1$GDP_UN*d1$mindustry), I(d1$GDP_UN*d1$murban), I(d1$pop1*d1$murban), I(d1$murban*d1$mindustry),I(d1$pop1*d1$mindustry))

gen1 <- GenMatch(Tr = d1$sanctions, X = X, BalanceMatrix = BalanceMatrix, pop.size = 10, print.level = 0, unif.seed=3392, int.seed=8282)

mgen1 <- Match(Y = d1$polity2, Tr = d1$sanctions, X = X, Weight.matrix = gen1)
summary(mgen1)

#at or below 0
d1 <- filter(df, polity2 <= 0, polity2 > -6)

X <- select(d1, GDP_UN, pop1, menergy, mindustry, murban) 

BalanceMatrix <- cbind(d1$GDP_UN, d1$pop1, d1$menergy, d1$mindustry, d1$murban, I(d1$GDP_UN*d1$pop1), I(d1$GDP_UN*d1$menergy),
                       I(d1$GDP_UN*d1$mindustry), I(d1$GDP_UN*d1$murban), I(d1$pop1*d1$murban), I(d1$murban*d1$mindustry),I(d1$pop1*d1$mindustry))

gen1 <- GenMatch(Tr = d1$sanctions, X = X, BalanceMatrix = BalanceMatrix, pop.size = 100, print.level = 0, unif.seed=3392, int.seed=8282)

mgen1 <- Match(Y = d1$polity2, Tr = d1$sanctions, X = X, Weight.matrix = gen1)
summary(mgen1)

