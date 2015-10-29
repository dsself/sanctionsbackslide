setwd("C:/Users/darin/Documents/sanctionsbackslide/Spectrum")
library(haven)
library(dplyr)
library(readr)
library(standard)
library(ggplot2)
library(scales)

#Load, join, and write csv####
load("marinov_ldpaper.RData")
dfm <-x
load("sanctionsrepdata.RData")
dfw <- x

dfw <- dfw %>%
  filter(year >= 1971, year < 2005) %>%
  select(Year = year, CCode = idcode, lagus, lagun, lagpopgled, laggdpgpc, laggdpchng)

dfm <- dfm %>%
  filter(year >= 1971, year < 2005) %>%
  select(Year = year, CCode = ccode, sanctions, sanctionsl1, hsesum, hseisum, fail, ot3, X_spline1, X_spline3,
         growthpc, lngdppc, democl1, mixedl1, democlnt, mixedlnt, xdemocl1, xcost, xlngdptl1, xinstl1,
         dispsum, hostsum, maxhost, vio, viofor)

dfs <- read_dta("Selectorate.dta") %>%
  filter(year >=1971, year < 2008) %>%
  select(Year = year, CCode = ccode, demaut, S, W)

dfp <- read_dta("Pippa.dta") %>%
  filter(Year >= 1971, Year < 2008) %>%
  mutate(energy2 = log(energy2))%>%
  select(Year, Nation, Natcode, CCode, polity2, industry1, GDP_UN, energy2, pop1, logpop, urban01, MEast, xrreg, xrcomp, xconst) %>% 
  mutate(Year = as.numeric(Year), CCode = as.numeric(CCode))

df <- right_join(dfw, dfm, by = c("Year", "CCode")) %>% 
  left_join(dfp, by = c("Year", "CCode")) %>% 
  left_join(dfs, by = c("Year", "CCode")) %>% 
  filter(!is.na(CCode)) %>%
  group_by(CCode) %>%
  mutate(dpolity = as.vector(polity2 - lag(polity2)))  %>%
  mutate(ds = lag(S)- lag(lag(S))) %>%
  mutate(dw = lag(W) - lag(lag(W))) %>%
  mutate(ndpolity = as.numeric(ifelse(dpolity < 0,1,0)))  %>%
  mutate(ndw = as.numeric(ifelse(dw < 0,1,0))) %>%
  select(Year, CCode, sanctions, sanctionsl1, polity2, dpolity, S, W, ndpolity, ds, dw, lagus, lagun, lagpopgled, laggdpchng, industry1, GDP_UN, energy2, pop1, 
         logpop, urban01, MEast, demaut) %>% 
  mutate(lGDP_UN = log(GDP_UN)) %>% 
  mutate(dpolityb = as.numeric(ifelse(dpolity != 0, 1, 0))) 

dp <- df %>% 
  group_by(Year, polity2) %>% 
  filter(!is.na(dpolityb)) %>% 
  count(dpolityb, polity2) %>% 
  group_by(polity2) %>% 
  mutate(Pdpolity = n/sum(n)) %>% 
  filter(dpolityb == 1) %>% 
  select(polity2, Pdpolity)

df <- merge(df, dp, by = "polity2", all = T) %>% 
  arrange(CCode, Year) %>% 
  filter(!is.na(polity2))

write_csv(df, "SanctionsMerged.csv")

#Multiple Imputations####
df <- read_csv("SanctionsMerged.csv") %>% 
  filter(!is.na(polity2)) 
df <- df[-2466,]

dfe <- select(df, Year, CCode, energy2)
dfi <- select(df, Year, CCode, industry1)
dfu <- select(df, Year, CCode, urban01)

dfe$energy2[dfe$energy2 == "-Inf"] <- NA

library(Amelia)
library(matrixStats)

energy <- amelia(x = dfe, m = 15, ts = "Year", cs = "CCode", polytime = 1)

de <- do.call(cbind,lapply(energy$imputations,function(x) x[x$Year > 1970,])) %>% 
  select(imp1.Year, imp1.CCode, contains("energy2")) %>% 
  mutate(menergy = rowMeans(.[,3:17])) %>% 
  mutate(senergy = rowSds(as.matrix(.[,3:17]))) %>% 
  select(Year = imp1.Year, CCode = imp1.CCode, menergy, senergy)

industry <- amelia(x = dfi, m = 15, ts = "Year", cs = "CCode", polytime = 1)

di <- do.call(cbind,lapply(industry$imputations,function(x) x[x$Year > 1970,])) %>% 
  select(imp1.Year, imp1.CCode, contains("industry1")) %>% 
  mutate(mindustry = rowMeans(.[,3:17])) %>% 
  mutate(sindustry = rowSds(as.matrix(.[,3:17]))) %>% 
  select(Year = imp1.Year, CCode = imp1.CCode, mindustry, sindustry)

urban <- amelia(x = dfu, m = 15, ts = "Year", cs = "CCode", polytime = 1)

du <- do.call(cbind,lapply(urban$imputations,function(x) x[x$Year > 1970,])) %>% 
  select(imp1.Year, imp1.CCode, contains("urban01")) %>% 
  mutate(murban = rowMeans(.[,3:17])) %>% 
  mutate(surban = rowSds(as.matrix(.[,3:17]))) %>% 
  select(Year = imp1.Year, CCode = imp1.CCode, murban, surban)

df <- inner_join(df, de, by = c("Year", "CCode")) %>% 
  inner_join(di, by = c("Year", "CCode")) %>% 
  inner_join(du, by = c("Year", "CCode")) %>% 
  select(-urban01, -energy2, -industry1) 

write_csv(df, "SanctionsFinal.csv")
  