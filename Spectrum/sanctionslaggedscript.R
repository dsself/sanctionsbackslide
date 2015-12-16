#install.packages("Matching", dependencies=TRUE)
#install.packages("rgenoud")
setwd("C:/Users/Darin/Documents/sanctionsl1backslide/Spectrum")
library(Matching)
library(rgenoud)
library(dplyr)
library(stargazer)
library(readr)
library(data.table)

df <- read_csv("SanctionsFinal.csv") %>% 
  mutate(pop1 = log(pop1)) %>% 
  filter(!is.na(GDP_UN)) %>% 
  
  mutate(deliniation = ifelse(polity2 >= 6, 1, 
                              ifelse(polity2 >= 2 & polity2 < 6, 2,
                                     ifelse(polity2 < 2 & polity2 > -2, 3, 
                                            ifelse(polity2 <= -2 & polity2 >= -5, 4, 
                                                   ifelse(polity2 <= -6, 5, NA)))))) %>%
  filter(!is.na(sanctionsl1))


df$murban[df$murban < 0] <- 0

####Full Sample####
X <- select(df, GDP_UN, pop1, menergy, mindustry, murban)

BalanceMatrix <- cbind(df$GDP_UN, df$pop1, df$menergy, 
                       df$mindustry, df$murban, I(df$GDP_UN*df$pop1),
                       I(df$GDP_UN*df$menergy), I(df$GDP_UN*df$mindustry), 
                       I(df$GDP_UN*df$murban), I(df$pop1*df$murban), 
                       I(df$murban*df$mindustry),I(df$pop1*df$mindustry))

#gen1 <- GenMatch(Tr = df$sanctionsl1, X = X, BalanceMatrix = BalanceMatrix, pop.size = 1000, 
#print.level = 0, unif.seed=3392, int.seed=8282)
load("gen1l.Rdata")

mgen1 <- Match(Y = df$polity2, Tr = df$sanctionsl1, X = X, Weight.matrix = gen1l)

#Strong democracy - theory####
dft1 <- filter(df, deliniation == 1)

X <- select(dft1, GDP_UN, pop1, menergy, mindustry, murban)

BalanceMatrix <- cbind(dft1$GDP_UN, dft1$pop1, dft1$menergy, 
                       dft1$mindustry, dft1$murban, I(dft1$GDP_UN*dft1$pop1),
                       I(dft1$GDP_UN*dft1$menergy), I(dft1$GDP_UN*dft1$mindustry), 
                       I(dft1$GDP_UN*dft1$murban), I(dft1$pop1*dft1$murban), 
                       I(dft1$murban*dft1$mindustry),I(dft1$pop1*dft1$mindustry))

#dt1 <- GenMatch(Tr = dft1$sanctionsl1, X = X, BalanceMatrix = BalanceMatrix, pop.size = 1000, print.level = 0, unif.seed=3392, int.seed=8282)
gendtl1 <- Match(Y = dft1$polity2, Tr = dft1$sanctionsl1, X = X, Weight.matrix = dt1)
save(gendtl1, file = "gendtl1.Rdata")

#Weak democracy - theory####
dft2 <- filter(df, deliniation == 2)

X <- select(dft2, GDP_UN, pop1, menergy, mindustry, murban)

BalanceMatrix <- cbind(dft2$GDP_UN, dft2$pop1, dft2$menergy, 
                       dft2$mindustry, dft2$murban, I(dft2$GDP_UN*dft2$pop1),
                       I(dft2$GDP_UN*dft2$menergy), I(dft2$GDP_UN*dft2$mindustry), 
                       I(dft2$GDP_UN*dft2$murban), I(dft2$pop1*dft2$murban), 
                       I(dft2$murban*dft2$mindustry),I(dft2$pop1*dft2$mindustry))

#dt2 <- GenMatch(Tr = dft2$sanctionsl1, X = X, BalanceMatrix = BalanceMatrix, pop.size = 1000, print.level = 0, unif.seed=3392, int.seed=8282)


gendtl2 <- Match(Y = dft2$polity2, Tr = dft2$sanctionsl1, X = X, Weight.matrix = dt2)
save(gendtl2, file = "gendtl2.Rdata")

#non democracy - theory####
dft3 <- filter(df, deliniation == 3)

X <- select(dft3, GDP_UN, pop1, menergy, mindustry, murban)

BalanceMatrix <- cbind(dft3$GDP_UN, dft3$pop1, dft3$menergy, 
                       dft3$mindustry, dft3$murban, I(dft3$GDP_UN*dft3$pop1),
                       I(dft3$GDP_UN*dft3$menergy), I(dft3$GDP_UN*dft3$mindustry), 
                       I(dft3$GDP_UN*dft3$murban), I(dft3$pop1*dft3$murban), 
                       I(dft3$murban*dft3$mindustry),I(dft3$pop1*dft3$mindustry))

#dt3 <- GenMatch(Tr = dft3$sanctionsl1, X = X, BalanceMatrix = BalanceMatrix, pop.size = 1000, print.level = 0, unif.seed=3392, int.seed=8282)

gendtl3 <- Match(Y = dft3$polity2, Tr = dft3$sanctionsl1, X = X, Weight.matrix = dt3)
save(gendtl3, file = "gendtl3.Rdata")

#weak authoritarian - theory####
dft4 <- filter(df, deliniation == 4)

X <- select(dft4, GDP_UN, pop1, menergy, mindustry, murban)

BalanceMatrix <- cbind(dft4$GDP_UN, dft4$pop1, dft4$menergy, 
                       dft4$mindustry, dft4$murban, I(dft4$GDP_UN*dft4$pop1),
                       I(dft4$GDP_UN*dft4$menergy), I(dft4$GDP_UN*dft4$mindustry), 
                       I(dft4$GDP_UN*dft4$murban), I(dft4$pop1*dft4$murban), 
                       I(dft4$murban*dft4$mindustry),I(dft4$pop1*dft4$mindustry))

#dt4 <- GenMatch(Tr = dft4$sanctionsl1, X = X, BalanceMatrix = BalanceMatrix, pop.size = 1000, print.level = 0, unif.seed=3392, int.seed=8282)

gendtl4 <- Match(Y = dft4$polity2, Tr = dft4$sanctionsl1, X = X, Weight.matrix = dt4)
save(gendtl4, file = "gendtl4.Rdata")

#strong authoritarian - theory####
dft5 <- filter(df, deliniation == 5)

X <- select(dft5, GDP_UN, pop1, menergy, mindustry, murban)

BalanceMatrix <- cbind(dft5$GDP_UN, dft5$pop1, dft5$menergy, 
                       dft5$mindustry, dft5$murban, I(dft5$GDP_UN*dft5$pop1),
                       I(dft5$GDP_UN*dft5$menergy), I(dft5$GDP_UN*dft5$mindustry), 
                       I(dft5$GDP_UN*dft5$murban), I(dft5$pop1*dft5$murban), 
                       I(dft5$murban*dft5$mindustry),I(dft5$pop1*dft5$mindustry))

#dt5 <- GenMatch(Tr = dft5$sanctionsl1, X = X, BalanceMatrix = BalanceMatrix, pop.size = 1000, print.level = 0, unif.seed=3392, int.seed=8282)

gendtl5 <- Match(Y = dft5$polity2, Tr = dft5$sanctionsl1, X = X, Weight.matrix = dt5)
save(gendtl5, file = "gendtl5.Rdata")

###non-parametric democracy deliniation####
k1 <- select(df, polity2, Pdpolity)
set.seed(2)
fit1 <- kmeans(k1, 5)
aggregate(k1,by=list(fit1$cluster),FUN=mean) %>% 
  arrange(-polity2) 

k1 <- data.frame(k1, fit1$cluster) %>% 
  mutate(fit1.cluster = plyr::mapvalues(fit1.cluster, from = c(3, 1, 4, 5, 2), to = c(1, 2, 3, 4, 5))) %>% 
  select(cluster = fit1.cluster) 

df <- cbind(df, k1) 

#Strong democracy - np####
dfnp1 <- filter(df, cluster == 1)

X <- select(dfnp1, GDP_UN, pop1, menergy, mindustry, murban)

BalanceMatrix <- cbind(dfnp1$GDP_UN, dfnp1$pop1, dfnp1$menergy, 
                       dfnp1$mindustry, dfnp1$murban, I(dfnp1$GDP_UN*dfnp1$pop1),
                       I(dfnp1$GDP_UN*dfnp1$menergy), I(dfnp1$GDP_UN*dfnp1$mindustry), 
                       I(dfnp1$GDP_UN*dfnp1$murban), I(dfnp1$pop1*dfnp1$murban), 
                       I(dfnp1$murban*dfnp1$mindustry),I(dfnp1$pop1*dfnp1$mindustry))

#dnp1 <- GenMatch(Tr = dfnp1$sanctionsl1, X = X, BalanceMatrix = BalanceMatrix, pop.size = 1000, print.level = 0, unif.seed=3392, int.seed=8282)

gennptl1 <- Match(Y = dfnp1$polity2, Tr = dfnp1$sanctionsl1, X = X, Weight.matrix = dnp1)
save(gennptl1, file = "gennptl1.Rdata")

#Weak democracy - np####
dfnp2 <- filter(df, cluster == 2)

X <- select(dfnp2, GDP_UN, pop1, menergy, mindustry, murban)

BalanceMatrix <- cbind(dfnp2$GDP_UN, dfnp2$pop1, dfnp2$menergy, 
                       dfnp2$mindustry, dfnp2$murban, I(dfnp2$GDP_UN*dfnp2$pop1),
                       I(dfnp2$GDP_UN*dfnp2$menergy), I(dfnp2$GDP_UN*dfnp2$mindustry), 
                       I(dfnp2$GDP_UN*dfnp2$murban), I(dfnp2$pop1*dfnp2$murban), 
                       I(dfnp2$murban*dfnp2$mindustry),I(dfnp2$pop1*dfnp2$mindustry))

#dnp2 <- GenMatch(Tr = dfnp2$sanctionsl1, X = X, BalanceMatrix = BalanceMatrix, pop.size = 1000, print.level = 0, unif.seed=3392, int.seed=8282)

gennptl2 <- Match(Y = dfnp2$polity2, Tr = dfnp2$sanctionsl1, X = X, Weight.matrix = dnp2)
save(gennptl2, file = "gennptl2.Rdata")

#non democracy - np####
dfnp3 <- filter(df, cluster == 3)

X <- select(dfnp3, GDP_UN, pop1, menergy, mindustry, murban)

BalanceMatrix <- cbind(dfnp3$GDP_UN, dfnp3$pop1, dfnp3$menergy, 
                       dfnp3$mindustry, dfnp3$murban, I(dfnp3$GDP_UN*dfnp3$pop1),
                       I(dfnp3$GDP_UN*dfnp3$menergy), I(dfnp3$GDP_UN*dfnp3$mindustry), 
                       I(dfnp3$GDP_UN*dfnp3$murban), I(dfnp3$pop1*dfnp3$murban), 
                       I(dfnp3$murban*dfnp3$mindustry),I(dfnp3$pop1*dfnp3$mindustry))

#dnp3 <- GenMatch(Tr = dfnp3$sanctionsl1, X = X, BalanceMatrix = BalanceMatrix, pop.size = 1000, print.level = 0, unif.seed=3392, int.seed=8282)

gennptl3 <- Match(Y = dfnp3$polity2, Tr = dfnp3$sanctionsl1, X = X, Weight.matrix = dnp3)
save(gennptl3, file = "gennptl3.Rdata")

#weak authoritarian - np####
dfnp4 <- filter(df, cluster == 4)

X <- select(dfnp4, GDP_UN, pop1, menergy, mindustry, murban)

BalanceMatrix <- cbind(dfnp4$GDP_UN, dfnp4$pop1, dfnp4$menergy, 
                       dfnp4$mindustry, dfnp4$murban, I(dfnp4$GDP_UN*dfnp4$pop1),
                       I(dfnp4$GDP_UN*dfnp4$menergy), I(dfnp4$GDP_UN*dfnp4$mindustry), 
                       I(dfnp4$GDP_UN*dfnp4$murban), I(dfnp4$pop1*dfnp4$murban), 
                       I(dfnp4$murban*dfnp4$mindustry),I(dfnp4$pop1*dfnp4$mindustry))

#dnp4 <- GenMatch(Tr = dfnp4$sanctionsl1, X = X, BalanceMatrix = BalanceMatrix, pop.size = 1000, print.level = 0, unif.seed=3392, int.seed=8282)

gennptl4 <- Match(Y = dfnp4$polity2, Tr = dfnp4$sanctionsl1, X = X, Weight.matrix = dnp4)
save(gennptl4, file = "gennptl4.Rdata")

#strong authoritarian - np####
dfnp5 <- filter(df, cluster == 5)

X <- select(dfnp5, GDP_UN, pop1, menergy, mindustry, murban)

BalanceMatrix <- cbind(dfnp5$GDP_UN, dfnp5$pop1, dfnp5$menergy, 
                       dfnp5$mindustry, dfnp5$murban, I(dfnp5$GDP_UN*dfnp5$pop1),
                       I(dfnp5$GDP_UN*dfnp5$menergy), I(dfnp5$GDP_UN*dfnp5$mindustry), 
                       I(dfnp5$GDP_UN*dfnp5$murban), I(dfnp5$pop1*dfnp5$murban), 
                       I(dfnp5$murban*dfnp5$mindustry),I(dfnp5$pop1*dfnp5$mindustry))

#dnp5 <- GenMatch(Tr = dfnp5$sanctionsl1, X = X, BalanceMatrix = BalanceMatrix, pop.size = 1000, print.level = 0, unif.seed=3392, int.seed=8282)

gennptl5 <- Match(Y = dfnp5$polity2, Tr = dfnp5$sanctionsl1, X = X, Weight.matrix = dnp5)
save(gennptl5, file = "gennptl5.Rdata")

####Build Data frame####
load("gendtl1.Rdata")
load("gendtl2.Rdata")
load("gendtl3.Rdata")
load("gendtl4.Rdata")
load("gendtl5.Rdata")
load("gennptl1.Rdata")
load("gennptl2.Rdata")
load("gennptl3.Rdata")
load("gennptl4.Rdata")
load("gennptl5.Rdata")

full <- with(mgen1,
             as.data.table(data.frame(Estimate = est, `AI SE` = se, `T-statistic` = est/se, p.val = (1 - pnorm(abs(est/se))) *
                                        2, Original.n = orig.nobs, Matched.n = wnobs))) %>% 
  mutate(Delineation = "Full Sample", Polity = "-10 to 10") %>% 
  select(Delineation, Polity, Estimate, Original.n, Matched.n, AI.SE, T.statistic, p.val)

tr1 <- with(gendtl1,
            as.data.table(data.frame(Estimate = est, `AI SE` = se, `T-statistic` = est/se, p.val = (1 - pnorm(abs(est/se))) *
                                       2, Original.n = orig.nobs, Matched.n = wnobs))) %>% 
  mutate(Delineation = "Theoretical", Polity = "6 to 10") %>% 
  select(Delineation, Polity, Estimate, Original.n, Matched.n, AI.SE, T.statistic, p.val)
tr2 <- with(gendtl2,
            as.data.table(data.frame(Estimate = est, `AI SE` = se, `T-statistic` = est/se, p.val = (1 - pnorm(abs(est/se))) *
                                       2, Original.n = orig.nobs, Matched.n = wnobs))) %>% 
  mutate(Delineation = "-", Polity = "2 to 5") %>% 
  select(Delineation, Polity, Estimate, Original.n, Matched.n, AI.SE, T.statistic, p.val)
tr3 <- with(gendtl3,
            as.data.table(data.frame(Estimate = est, `AI SE` = se, `T-statistic` = est/se, p.val = (1 - pnorm(abs(est/se))) *
                                       2, Original.n = orig.nobs, Matched.n = wnobs))) %>% 
  mutate(Delineation = "-", Polity = "-1 to 1") %>% 
  select(Delineation, Polity, Estimate, Original.n, Matched.n, AI.SE, T.statistic, p.val)
tr4 <- with(gendtl4,
            as.data.table(data.frame(Estimate = est, `AI SE` = se, `T-statistic` = est/se, p.val = (1 - pnorm(abs(est/se))) *
                                       2, Original.n = orig.nobs, Matched.n = wnobs))) %>% 
  mutate(Delineation = "-", Polity = "-5 to -2") %>% 
  select(Delineation, Polity, Estimate, Original.n, Matched.n, AI.SE, T.statistic, p.val)
tr5 <- with(gendtl5,
            as.data.table(data.frame(Estimate = est, `AI SE` = se, `T-statistic` = est/se, p.val = (1 - pnorm(abs(est/se))) *
                                       2, Original.n = orig.nobs, Matched.n = wnobs))) %>% 
  mutate(Delineation = "-", Polity = "-10 to -6") %>% 
  select(Delineation, Polity, Estimate, Original.n, Matched.n, AI.SE, T.statistic, p.val)

npr1 <- with(gennptl1,
             as.data.table(data.frame(Estimate = est, `AI SE` = se, `T-statistic` = est/se, p.val = (1 - pnorm(abs(est/se))) *
                                        2, Original.n = orig.nobs, Matched.n = wnobs))) %>% 
  mutate(Delineation = "Non-parametric", Polity = "8 to 10") %>% 
  select(Delineation, Polity, Estimate, Original.n, Matched.n, AI.SE, T.statistic, p.val)
npr2 <- with(gennptl2,
             as.data.table(data.frame(Estimate = est, `AI SE` = se, `T-statistic` = est/se, p.val = (1 - pnorm(abs(est/se))) *
                                        2, Original.n = orig.nobs, Matched.n = wnobs))) %>% 
  mutate(Delineation = "-", Polity = "3 to 7") %>% 
  select(Delineation, Polity, Estimate, Original.n, Matched.n, AI.SE, T.statistic, p.val)
npr3 <- with(gennptl3,
             as.data.table(data.frame(Estimate = est, `AI SE` = se, `T-statistic` = est/se, p.val = (1 - pnorm(abs(est/se))) *
                                        2, Original.n = orig.nobs, Matched.n = wnobs))) %>% 
  mutate(Delineation = "-", Polity = "-2 to 2") %>% 
  select(Delineation, Polity, Estimate, Original.n, Matched.n, AI.SE, T.statistic, p.val)
npr4 <- with(gennptl4,
             as.data.table(data.frame(Estimate = est, `AI SE` = se, `T-statistic` = est/se, p.val = (1 - pnorm(abs(est/se))) *
                                        2, Original.n = orig.nobs, Matched.n = wnobs))) %>% 
  mutate(Delineation = "-", Polity = "-6 to -3") %>% 
  select(Delineation, Polity, Estimate, Original.n, Matched.n, AI.SE, T.statistic, p.val)
npr5 <- with(gennptl5,
             as.data.table(data.frame(Estimate = est, `AI SE` = se, `T-statistic` = est/se, p.val = (1 - pnorm(abs(est/se))) *
                                        2, Original.n = orig.nobs, Matched.n = wnobs))) %>% 
  mutate(Delineation = "-", Polity = "-10 to -7") %>% 
  select(Delineation, Polity, Estimate, Original.n, Matched.n, AI.SE, T.statistic, p.val)

table2 <- rbind(full, tr1, tr2, tr3, tr4, tr5, npr1, npr2, npr3, npr4, npr5)

stargazer(table2, summary = F, rownames = FALSE)


save(table2, file = "Table2.Rdata")
