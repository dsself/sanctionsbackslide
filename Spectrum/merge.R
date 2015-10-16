setwd("C:/Users/darin/Documents/sanctionsbackslide/Spectrum")
library(haven)
library(dplyr)
library(standard)
library(ggplot2)
library(scales)

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
  mutate(dpolity = polity2 - lag(polity2))  %>%
  mutate(ds = lag(S)- lag(lag(S))) %>%
  mutate(dw = lag(W) - lag(lag(W))) %>%
  mutate(ndpolity = as.numeric(ifelse(dpolity < 0,1,0)))  %>%
  mutate(ndw = as.numeric(ifelse(dw < 0,1,0))) %>%
  select(Year, CCode, sanctions, sanctionsl1, polity2, dpolity, S, W, ndpolity, ds, dw, lagus, lagun, lagpopgled, laggdpchng, industry1, GDP_UN, energy2, pop1, 
         logpop, urban01, MEast, demaut) %>% 
  mutate(lGDP_UN = log(GDP_UN)) %>% 
  mutate(dpolityb = as.numeric(ifelse(dpolity != 0, 1, 0)))

write_csv(df, "SanctionsFinal.csv")

df <- df %>% 
  group_by(Year, polity2) %>% 
  mutate(sum = sum(dpolityb)) %>% 
  select(CCode, Year, polity2, dpolityb, sum) %>% 
  group_by(polity2) 

dp <- df %>% 
  group_by(Year, polity2) %>% 
  filter(!is.na(dpolityb)) %>% 
  count(dpolityb, polity2) %>% 
  group_by(polity2) %>% 
  mutate(P = n/sum(n)) %>% 
  filter(dpolityb == 1)

ggplot(dp, aes(x=polity2, y = P)) +
  geom_bar(stat = "identity") +
  ggtitle("Probability of Change in Polity")+
  scale_y_continuous(labels = percent)

