#setup for US sanctions####
df <- read_csv("SanctionsFinal.csv") %>% 
  mutate(pop1 = log(pop1)) %>% 
  filter(!is.na(GDP_UN), !is.na(lagus)) %>% 
  
  mutate(deliniation = ifelse(polity2 >= 6, 1, 
                              ifelse(polity2 >= 2 & polity2 < 6, 2,
                                     ifelse(polity2 < 2 & polity2 > -2, 3, 
                                            ifelse(polity2 <= -2 & polity2 >= -5, 4, 
                                                   ifelse(polity2 <= -6, 5, NA)))))) %>% 
  mutate(lagus0 = ifelse(lagus == 0, 1, 0)) %>% 
  mutate(lagus1 = ifelse(lagus == 1, 1, 0)) %>% 
  mutate(lagus2 = ifelse(lagus == 2, 1, 0)) %>% 
  mutate(lagus3 = ifelse(lagus == 3, 1, 0)) 
  
#US == 0####
X <- select(df, GDP_UN, pop1, menergy, mindustry, murban)

BalanceMatrix <- cbind(df$GDP_UN, df$pop1, df$menergy, 
                       df$mindustry, df$murban, I(df$GDP_UN*df$pop1),
                       I(df$GDP_UN*df$menergy), I(df$GDP_UN*df$mindustry), 
                       I(df$GDP_UN*df$murban), I(df$pop1*df$murban), 
                       I(df$murban*df$mindustry),I(df$pop1*df$mindustry))

lagus0 <- GenMatch(Tr = df$lagus0, X = X, BalanceMatrix = BalanceMatrix, pop.size = 1000, print.level = 0, unif.seed=3392, int.seed=8282)
save(lagus0, file = "lagus0.Rdata")

lgus0 <- Match(Y = df$polity2, Tr = df$lagus0, X = X, Weight.matrix = lagus0)

#US == 1####
X <- select(df, GDP_UN, pop1, menergy, mindustry, murban)

BalanceMatrix <- cbind(df$GDP_UN, df$pop1, df$menergy, 
                       df$mindustry, df$murban, I(df$GDP_UN*df$pop1),
                       I(df$GDP_UN*df$menergy), I(df$GDP_UN*df$mindustry), 
                       I(df$GDP_UN*df$murban), I(df$pop1*df$murban), 
                       I(df$murban*df$mindustry),I(df$pop1*df$mindustry))

lagus1 <- GenMatch(Tr = df$lagus1, X = X, BalanceMatrix = BalanceMatrix, pop.size = 1000, print.level = 0, unif.seed=3392, int.seed=8282)
save(lagus1, file = "lagus1.Rdata")

lgus1 <- Match(Y = df$polity2, Tr = df$lagus1, X = X, Weight.matrix = lagus1)

#US == 2####
X <- select(df, GDP_UN, pop1, menergy, mindustry, murban)

BalanceMatrix <- cbind(df$GDP_UN, df$pop1, df$menergy, 
                       df$mindustry, df$murban, I(df$GDP_UN*df$pop1),
                       I(df$GDP_UN*df$menergy), I(df$GDP_UN*df$mindustry), 
                       I(df$GDP_UN*df$murban), I(df$pop1*df$murban), 
                       I(df$murban*df$mindustry),I(df$pop1*df$mindustry))

lagus2 <- GenMatch(Tr = df$lagus2, X = X, BalanceMatrix = BalanceMatrix, pop.size = 1000, print.level = 0, unif.seed=3392, int.seed=8282)
save(lagus2, file = "lagus2.Rdata")

lgus2 <- Match(Y = df$polity2, Tr = df$lagus2, X = X, Weight.matrix = lagus2)

#US == 3####
X <- select(df, GDP_UN, pop1, menergy, mindustry, murban)

BalanceMatrix <- cbind(df$GDP_UN, df$pop1, df$menergy, 
                       df$mindustry, df$murban, I(df$GDP_UN*df$pop1),
                       I(df$GDP_UN*df$menergy), I(df$GDP_UN*df$mindustry), 
                       I(df$GDP_UN*df$murban), I(df$pop1*df$murban), 
                       I(df$murban*df$mindustry),I(df$pop1*df$mindustry))

lagus3 <- GenMatch(Tr = df$lagus3, X = X, BalanceMatrix = BalanceMatrix, pop.size = 1000, print.level = 0, unif.seed=3392, int.seed=8282)
save(lagus3, file = "lagus3.Rdata")

lgus3 <- Match(Y = df$polity2, Tr = df$lagus3, X = X, Weight.matrix = lagus3)

#setup for UN sanctions####
df <- read_csv("SanctionsFinal.csv") %>% 
  mutate(pop1 = log(pop1)) %>% 
  filter(!is.na(GDP_UN), !is.na(lagun)) %>% 
  
  mutate(deliniation = ifelse(polity2 >= 6, 1, 
                              ifelse(polity2 >= 2 & polity2 < 6, 2,
                                     ifelse(polity2 < 2 & polity2 > -2, 3, 
                                            ifelse(polity2 <= -2 & polity2 >= -5, 4, 
                                                   ifelse(polity2 <= -6, 5, NA)))))) %>% 
  mutate(lagun0 = ifelse(lagun == 0, 1, 0)) %>% 
  mutate(lagun1 = ifelse(lagun == 1, 1, 0)) %>% 
  mutate(lagun2 = ifelse(lagun == 2, 1, 0)) %>%
  mutate(lagun3 = ifelse(lagun == 3, 1, 0)) 

df$murban[df$murban < 0] <- 0

k1 <- select(df, polity2, Pdpolity)
set.seed(2)
fit1 <- kmeans(k1, 5)
aggregate(k1,by=list(fit1$cluster),FUN=mean) %>% 
  arrange(-polity2) 

k1 <- data.frame(k1, fit1$cluster) %>% 
  mutate(fit1.cluster = plyr::mapvalues(fit1.cluster, from = c(3, 1, 4, 5, 2), to = c(1, 2, 3, 4, 5))) %>% 
  select(cluster = fit1.cluster) 

df <- cbind(df, k1) 

#UN == 0####
X <- select(df, GDP_UN, pop1, menergy, mindustry, murban)

BalanceMatrix <- cbind(df$GDP_UN, df$pop1, df$menergy, 
                       df$mindustry, df$murban, I(df$GDP_UN*df$pop1),
                       I(df$GDP_UN*df$menergy), I(df$GDP_UN*df$mindustry), 
                       I(df$GDP_UN*df$murban), I(df$pop1*df$murban), 
                       I(df$murban*df$mindustry),I(df$pop1*df$mindustry))

lagun0 <- GenMatch(Tr = df$lagun0, X = X, BalanceMatrix = BalanceMatrix, pop.size = 1000, print.level = 0, unif.seed=3392, int.seed=8282)
save(lagun0, file = "lagun0.Rdata")

lgun0 <- Match(Y = df$polity2, Tr = df$lagun0, X = X, Weight.matrix = lagun0)

#UN == 1####
X <- select(df, GDP_UN, pop1, menergy, mindustry, murban)

BalanceMatrix <- cbind(df$GDP_UN, df$pop1, df$menergy, 
                       df$mindustry, df$murban, I(df$GDP_UN*df$pop1),
                       I(df$GDP_UN*df$menergy), I(df$GDP_UN*df$mindustry), 
                       I(df$GDP_UN*df$murban), I(df$pop1*df$murban), 
                       I(df$murban*df$mindustry),I(df$pop1*df$mindustry))

lagun1 <- GenMatch(Tr = df$lagun1, X = X, BalanceMatrix = BalanceMatrix, pop.size = 1000, print.level = 0, unif.seed=3392, int.seed=8282)
save(lagun1, file = "lagun1.Rdata")

lgun1 <- Match(Y = df$polity2, Tr = df$lagun1, X = X, Weight.matrix = lagun1)

#UN == 2####
X <- select(df, GDP_UN, pop1, menergy, mindustry, murban)

BalanceMatrix <- cbind(df$GDP_UN, df$pop1, df$menergy, 
                       df$mindustry, df$murban, I(df$GDP_UN*df$pop1),
                       I(df$GDP_UN*df$menergy), I(df$GDP_UN*df$mindustry), 
                       I(df$GDP_UN*df$murban), I(df$pop1*df$murban), 
                       I(df$murban*df$mindustry),I(df$pop1*df$mindustry))

lagun2 <- GenMatch(Tr = df$lagun2, X = X, BalanceMatrix = BalanceMatrix, pop.size = 1000, print.level = 0, unif.seed=3392, int.seed=8282)
save(lagun2, file = "lagun2.Rdata")

lgun2 <- Match(Y = df$polity2, Tr = df$lagun2, X = X, Weight.matrix = lagun2)

#UN == 3####
X <- select(df, GDP_UN, pop1, menergy, mindustry, murban)

BalanceMatrix <- cbind(df$GDP_UN, df$pop1, df$menergy, 
                       df$mindustry, df$murban, I(df$GDP_UN*df$pop1),
                       I(df$GDP_UN*df$menergy), I(df$GDP_UN*df$mindustry), 
                       I(df$GDP_UN*df$murban), I(df$pop1*df$murban), 
                       I(df$murban*df$mindustry),I(df$pop1*df$mindustry))

lagun3 <- GenMatch(Tr = df$lagun3, X = X, BalanceMatrix = BalanceMatrix, pop.size = 1000, print.level = 0, unif.seed=3392, int.seed=8282)
save(lagun3, file = "lagun3.Rdata")

lgun3 <- Match(Y = df$polity2, Tr = df$lagun3, X = X, Weight.matrix = lagun3)

#Dataframes####
tus0 <- with(lgus0,
            as.data.table(data.frame(Estimate = est, `AI SE` = se, `T-statistic` = est/se, p.val = (1 - pnorm(abs(est/se))) *
                                       2, Original.n = orig.nobs, Matched.n = wnobs))) %>% 
  mutate(Sender = "United States", Strength = 0) %>% 
  select(Sender, Strength, Estimate, Original.n, Matched.n, AI.SE, T.statistic, p.val)
tus1 <- with(lgus1,
            as.data.table(data.frame(Estimate = est, `AI SE` = se, `T-statistic` = est/se, p.val = (1 - pnorm(abs(est/se))) *
                                       2, Original.n = orig.nobs, Matched.n = wnobs))) %>% 
  mutate(Sender = "-", Strength = 1) %>% 
  select(Sender, Strength, Estimate, Original.n, Matched.n, AI.SE, T.statistic, p.val)
tus2 <- with(lgus2,
            as.data.table(data.frame(Estimate = est, `AI SE` = se, `T-statistic` = est/se, p.val = (1 - pnorm(abs(est/se))) *
                                       2, Original.n = orig.nobs, Matched.n = wnobs))) %>% 
  mutate(Sender = "-", Strength = 2) %>% 
  select(Sender, Strength, Estimate, Original.n, Matched.n, AI.SE, T.statistic, p.val)
tus3 <- with(lgus3,
            as.data.table(data.frame(Estimate = est, `AI SE` = se, `T-statistic` = est/se, p.val = (1 - pnorm(abs(est/se))) *
                                       2, Original.n = orig.nobs, Matched.n = wnobs))) %>% 
  mutate(Sender = "-", Strength = 3) %>% 
  select(Sender, Strength, Estimate, Original.n, Matched.n, AI.SE, T.statistic, p.val)

tun0 <- with(lgun0,
             as.data.table(data.frame(Estimate = est, `AI SE` = se, `T-statistic` = est/se, p.val = (1 - pnorm(abs(est/se))) *
                                        2, Original.n = orig.nobs, Matched.n = wnobs))) %>% 
  mutate(Sender = "United Nations", Strength = 0) %>% 
  select(Sender, Strength, Estimate, Original.n, Matched.n, AI.SE, T.statistic, p.val)
tun1 <- with(lgun1,
             as.data.table(data.frame(Estimate = est, `AI SE` = se, `T-statistic` = est/se, p.val = (1 - pnorm(abs(est/se))) *
                                        2, Original.n = orig.nobs, Matched.n = wnobs))) %>% 
  mutate(Sender = "-", Strength = 1) %>% 
  select(Sender, Strength, Estimate, Original.n, Matched.n, AI.SE, T.statistic, p.val)
tun2 <- with(lgun2,
             as.data.table(data.frame(Estimate = est, `AI SE` = se, `T-statistic` = est/se, p.val = (1 - pnorm(abs(est/se))) *
                                        2, Original.n = orig.nobs, Matched.n = wnobs))) %>% 
  mutate(Sender = "-", Strength = 2) %>% 
  select(Sender, Strength, Estimate, Original.n, Matched.n, AI.SE, T.statistic, p.val)
tun3 <- with(lgun3,
             as.data.table(data.frame(Estimate = est, `AI SE` = se, `T-statistic` = est/se, p.val = (1 - pnorm(abs(est/se))) *
                                        2, Original.n = orig.nobs, Matched.n = wnobs))) %>% 
  mutate(Sender = "-", Strength = 3) %>% 
  select(Sender, Strength, Estimate, Original.n, Matched.n, AI.SE, T.statistic, p.val)

table5 <- rbind(tus0, tus1, tus2, tus3, tun0, tun1, tun2, tun3)

save(table5, file = "Table5.Rdata")

#setup for US sanctions by high vs low####
df <- read_csv("SanctionsFinal.csv") %>% 
  mutate(pop1 = log(pop1)) %>% 
  filter(!is.na(GDP_UN), !is.na(lagus)) %>% 
  
  mutate(deliniation = ifelse(polity2 >= 6, 1, 
                              ifelse(polity2 >= 2 & polity2 < 6, 2,
                                     ifelse(polity2 < 2 & polity2 > -2, 3, 
                                            ifelse(polity2 <= -2 & polity2 >= -5, 4, 
                                                   ifelse(polity2 <= -6, 5, NA)))))) %>% 
  mutate(lagus0 = ifelse(lagus == 0, 1, 0)) %>% 
  mutate(lagus1 = ifelse(lagus == 1, 1, 0)) %>% 
  mutate(lagus2 = ifelse(lagus == 2, 1, 0)) %>% 
  mutate(lagus3 = ifelse(lagus == 3, 1, 0)) 

dh <- filter(df, polity2 < 8 & polity2 > -1)
dt <- filter(df, polity2 > -8 & polity2 < 0)

#US == 0 - high####
X <- select(dh, GDP_UN, pop1, menergy, mindustry, murban)

BalanceMatrix <- cbind(dh$GDP_UN, dh$pop1, dh$menergy, 
                       dh$mindustry, dh$murban, I(dh$GDP_UN*dh$pop1),
                       I(dh$GDP_UN*dh$menergy), I(dh$GDP_UN*dh$mindustry), 
                       I(dh$GDP_UN*dh$murban), I(dh$pop1*dh$murban), 
                       I(dh$murban*dh$mindustry),I(dh$pop1*dh$mindustry))

lagus0h <- GenMatch(Tr = dh$lagus0, X = X, BalanceMatrix = BalanceMatrix, pop.size = 1000, print.level = 0, unif.seed=3392, int.seed=8282)
save(lagus0h, file = "lagus0h.Rdata")

lgus0h <- Match(Y = dh$polity2, Tr = dh$lagus0, X = X, Weight.matrix = lagus0h)

#US == 1 - high####
X <- select(dh, GDP_UN, pop1, menergy, mindustry, murban)

BalanceMatrix <- cbind(dh$GDP_UN, dh$pop1, dh$menergy, 
                       dh$mindustry, dh$murban, I(dh$GDP_UN*dh$pop1),
                       I(dh$GDP_UN*dh$menergy), I(dh$GDP_UN*dh$mindustry), 
                       I(dh$GDP_UN*dh$murban), I(dh$pop1*dh$murban), 
                       I(dh$murban*dh$mindustry),I(dh$pop1*dh$mindustry))

lagus1h <- GenMatch(Tr = dh$lagus1, X = X, BalanceMatrix = BalanceMatrix, pop.size = 1000, print.level = 0, unif.seed=3392, int.seed=8282)
save(lagus1h, file = "lagus1h.Rdata")

lgus1h <- Match(Y = dh$polity2, Tr = dh$lagus1, X = X, Weight.matrix = lagus1h)

#US == 2 - high####
X <- select(dh, GDP_UN, pop1, menergy, mindustry, murban)

BalanceMatrix <- cbind(dh$GDP_UN, dh$pop1, dh$menergy, 
                       dh$mindustry, dh$murban, I(dh$GDP_UN*dh$pop1),
                       I(dh$GDP_UN*dh$menergy), I(dh$GDP_UN*dh$mindustry), 
                       I(dh$GDP_UN*dh$murban), I(dh$pop1*dh$murban), 
                       I(dh$murban*dh$mindustry),I(dh$pop1*dh$mindustry))

lagus2h <- GenMatch(Tr = dh$lagus2, X = X, BalanceMatrix = BalanceMatrix, pop.size = 1000, print.level = 0, unif.seed=3392, int.seed=8282)
save(lagus2h, file = "lagus2h.Rdata")

lgus2h <- Match(Y = dh$polity2, Tr = dh$lagus2, X = X, Weight.matrix = lagus2h)

#US == 3 - high####
X <- select(dh, GDP_UN, pop1, menergy, mindustry, murban)

BalanceMatrix <- cbind(dh$GDP_UN, dh$pop1, dh$menergy, 
                       dh$mindustry, dh$murban, I(dh$GDP_UN*dh$pop1),
                       I(dh$GDP_UN*dh$menergy), I(dh$GDP_UN*dh$mindustry), 
                       I(dh$GDP_UN*dh$murban), I(dh$pop1*dh$murban), 
                       I(dh$murban*dh$mindustry),I(dh$pop1*dh$mindustry))

lagus3h <- GenMatch(Tr = dh$lagus3, X = X, BalanceMatrix = BalanceMatrix, pop.size = 1000, print.level = 0, unif.seed=3392, int.seed=8282)
save(lagus3h, file = "lagus3h.Rdata")

lgus3h <- Match(Y = dh$polity2, Tr = dh$lagus3, X = X, Weight.matrix = lagus3h)

#US == 0 - low####
X <- select(dt, GDP_UN, pop1, menergy, mindustry, murban)

BalanceMatrix <- cbind(dt$GDP_UN, dt$pop1, dt$menergy, 
                       dt$mindustry, dt$murban, I(dt$GDP_UN*dt$pop1),
                       I(dt$GDP_UN*dt$menergy), I(dt$GDP_UN*dt$mindustry), 
                       I(dt$GDP_UN*dt$murban), I(dt$pop1*dt$murban), 
                       I(dt$murban*dt$mindustry),I(dt$pop1*dt$mindustry))

lagus0t <- GenMatch(Tr = dt$lagus0, X = X, BalanceMatrix = BalanceMatrix, pop.size = 1000, print.level = 0, unif.seed=3392, int.seed=8282)
save(lagus0t, file = "lagus0t.Rdata")

lgus0t <- Match(Y = dt$polity2, Tr = dt$lagus0, X = X, Weight.matrix = lagus0t)

#US == 1 - low####
X <- select(dt, GDP_UN, pop1, menergy, mindustry, murban)

BalanceMatrix <- cbind(dt$GDP_UN, dt$pop1, dt$menergy, 
                       dt$mindustry, dt$murban, I(dt$GDP_UN*dt$pop1),
                       I(dt$GDP_UN*dt$menergy), I(dt$GDP_UN*dt$mindustry), 
                       I(dt$GDP_UN*dt$murban), I(dt$pop1*dt$murban), 
                       I(dt$murban*dt$mindustry),I(dt$pop1*dt$mindustry))

lagus1t <- GenMatch(Tr = dt$lagus1, X = X, BalanceMatrix = BalanceMatrix, pop.size = 1000, print.level = 0, unif.seed=3392, int.seed=8282)
save(lagus1t, file = "lagus1t.Rdata")

lgus1t <- Match(Y = dt$polity2, Tr = dt$lagus1, X = X, Weight.matrix = lagus1t)

#US == 2 - low####
X <- select(dt, GDP_UN, pop1, menergy, mindustry, murban)

BalanceMatrix <- cbind(dt$GDP_UN, dt$pop1, dt$menergy, 
                       dt$mindustry, dt$murban, I(dt$GDP_UN*dt$pop1),
                       I(dt$GDP_UN*dt$menergy), I(dt$GDP_UN*dt$mindustry), 
                       I(dt$GDP_UN*dt$murban), I(dt$pop1*dt$murban), 
                       I(dt$murban*dt$mindustry),I(dt$pop1*dt$mindustry))

lagus2t <- GenMatch(Tr = dt$lagus2, X = X, BalanceMatrix = BalanceMatrix, pop.size = 1000, print.level = 0, unif.seed=3392, int.seed=8282)
save(lagus2t, file = "lagus2t.Rdata")

lgus2t <- Match(Y = dt$polity2, Tr = dt$lagus2, X = X, Weight.matrix = lagus2t)

#US == 3 - low####
X <- select(dt, GDP_UN, pop1, menergy, mindustry, murban)

BalanceMatrix <- cbind(dt$GDP_UN, dt$pop1, dt$menergy, 
                       dt$mindustry, dt$murban, I(dt$GDP_UN*dt$pop1),
                       I(dt$GDP_UN*dt$menergy), I(dt$GDP_UN*dt$mindustry), 
                       I(dt$GDP_UN*dt$murban), I(dt$pop1*dt$murban), 
                       I(dt$murban*dt$mindustry),I(dt$pop1*dt$mindustry))

lagus3t <- GenMatch(Tr = dt$lagus3, X = X, BalanceMatrix = BalanceMatrix, pop.size = 1000, print.level = 0, unif.seed=3392, int.seed=8282)
save(lagus3t, file = "lagus3t.Rdata")

lgus3t <- Match(Y = dt$polity2, Tr = dt$lagus3, X = X, Weight.matrix = lagus3t)

#Dataframes low vs high####
tus0 <- with(lgus0h,
             as.data.table(data.frame(Estimate = est, `AI SE` = se, `T-statistic` = est/se, p.val = (1 - pnorm(abs(est/se))) *
                                        2, Original.n = orig.nobs, Matched.n = wnobs))) %>% 
  mutate(Polity = "0 through 7", Strength = 0) %>% 
  select(Polity, Strength, Estimate, Original.n, Matched.n, AI.SE, T.statistic, p.val)
tus1 <- with(lgus1h,
             as.data.table(data.frame(Estimate = est, `AI SE` = se, `T-statistic` = est/se, p.val = (1 - pnorm(abs(est/se))) *
                                        2, Original.n = orig.nobs, Matched.n = wnobs))) %>% 
  mutate(Polity = "-", Strength = 1) %>% 
  select(Polity, Strength, Estimate, Original.n, Matched.n, AI.SE, T.statistic, p.val)
tus2 <- with(lgus2h,
             as.data.table(data.frame(Estimate = est, `AI SE` = se, `T-statistic` = est/se, p.val = (1 - pnorm(abs(est/se))) *
                                        2, Original.n = orig.nobs, Matched.n = wnobs))) %>% 
  mutate(Polity = "-", Strength = 2) %>% 
  select(Polity, Strength, Estimate, Original.n, Matched.n, AI.SE, T.statistic, p.val)
tus3 <- with(lgus3h,
             as.data.table(data.frame(Estimate = est, `AI SE` = se, `T-statistic` = est/se, p.val = (1 - pnorm(abs(est/se))) *
                                        2, Original.n = orig.nobs, Matched.n = wnobs))) %>% 
  mutate(Polity = "-", Strength = 3) %>% 
  select(Polity, Strength, Estimate, Original.n, Matched.n, AI.SE, T.statistic, p.val)

tun0 <- with(lgus0t,
             as.data.table(data.frame(Estimate = est, `AI SE` = se, `T-statistic` = est/se, p.val = (1 - pnorm(abs(est/se))) *
                                        2, Original.n = orig.nobs, Matched.n = wnobs))) %>% 
  mutate(Polity = "-7 through -1", Strength = 0) %>% 
  select(Polity, Strength, Estimate, Original.n, Matched.n, AI.SE, T.statistic, p.val)
tun1 <- with(lgus1t,
             as.data.table(data.frame(Estimate = est, `AI SE` = se, `T-statistic` = est/se, p.val = (1 - pnorm(abs(est/se))) *
                                        2, Original.n = orig.nobs, Matched.n = wnobs))) %>% 
  mutate(Polity = "-", Strength = 1) %>% 
  select(Polity, Strength, Estimate, Original.n, Matched.n, AI.SE, T.statistic, p.val)
tun2 <- with(lgus2t,
             as.data.table(data.frame(Estimate = est, `AI SE` = se, `T-statistic` = est/se, p.val = (1 - pnorm(abs(est/se))) *
                                        2, Original.n = orig.nobs, Matched.n = wnobs))) %>% 
  mutate(Polity = "-", Strength = 2) %>% 
  select(Polity, Strength, Estimate, Original.n, Matched.n, AI.SE, T.statistic, p.val)
tun3 <- with(lgus3t,
             as.data.table(data.frame(Estimate = est, `AI SE` = se, `T-statistic` = est/se, p.val = (1 - pnorm(abs(est/se))) *
                                        2, Original.n = orig.nobs, Matched.n = wnobs))) %>% 
  mutate(Polity = "-", Strength = 3) %>% 
  select(Polity, Strength, Estimate, Original.n, Matched.n, AI.SE, T.statistic, p.val)

table6 <- rbind(tus0, tus1, tus2, tus3, tun0, tun1, tun2, tun3)

save(table6, file = "Table6.Rdata")
