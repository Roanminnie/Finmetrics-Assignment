if (!require("rmsfuns")) install.packages("rmsfuns")
if (!require("devtools")) install.packages("devtools")
if (!require("xts")) install.packages("xts")
library(rmsfuns)
library(tidyverse)
library(xts)
load_pkg("tidyverse")
load_pkg("ggplot2")
load_pkg("PerformanceAnalytics")

Practical.loc.root <- file.path(getwd())
Practical.loc.subdirs <- c("data", "code", "bin")
PracLoc <- build_path(glue::glue("{Practical.loc.root}/{Practical.loc.subdirs}"))

library(rmsfuns)
load_pkg("readr")
data <- read_rds("data/Fin_Data_SA_US_NKY.rds")

Tidydata <- data %>% 
  mutate(Year_Month = format(date, "%Y%B")) %>% 
  group_by(Year_Month, Market.Cap) %>% 
  mutate(N_Obs = n(), N_NA = sum(is.na(Market.Cap)) ) %>% filter( N_Obs != N_NA) %>% ungroup() %>% select(date, Ticker,TRI, Short.Name, Market.Cap, BICS_LEVEL_1_SECTOR_NAME, Universe, Year_Month)

monthly_returns <- Tidydata %>% 
  arrange(date) %>% 
  group_by(BICS_LEVEL_1_SECTOR_NAME, Universe, date) %>% 
  #calculate market cap shares per sector 
  mutate(Tot_MC = sum(Market.Cap)) %>% 
  mutate(MC_weight = Market.Cap/Tot_MC) %>% 
  ungroup() %>% 
  #calculate monthly returns per ticker
  group_by(Year_Month, Ticker) %>% 
  filter(date == last(date)) %>% 
  ungroup() %>% 
  group_by(Ticker) %>% 
  mutate(Monthly_return = TRI/lag(TRI) - 1) %>%  ungroup() %>% 
  #now calculate cap weighted return for every sector and universe
  group_by(Year_Month, Ticker) %>% 
  mutate(Weighted_Monthly_Ret = Monthly_return*MC_weight) %>% ungroup() %>% 
  group_by(Year_Month, BICS_LEVEL_1_SECTOR_NAME, Universe) %>% 
  mutate(Index_monthly_return = sum(Weighted_Monthly_Ret)) %>% 
  mutate(N_Obs = n(), N_NA = sum(is.na(Index_monthly_return)) ) %>%
  filter( N_Obs != N_NA) %>% ungroup() %>% 
  select(Year_Month,BICS_LEVEL_1_SECTOR_NAME,Universe,Index_monthly_return) %>% 
  arrange(BICS_LEVEL_1_SECTOR_NAME) %>% 
  unique.data.frame()

Sharpe <- Tidydata %>% 
  arrange(date) %>% 
  group_by(BICS_LEVEL_1_SECTOR_NAME, Universe, date) %>% 
  #calculate market cap shares per sector 
  mutate(Tot_MC = sum(Market.Cap)) %>% 
  mutate(MC_weight = Market.Cap/Tot_MC) %>% 
  ungroup() %>% 
  #calculate market cap weighted daily returns
  group_by(Ticker) %>% 
  mutate(Daily_ret =  TRI/lag(TRI) - 1) %>% 
  mutate(Weighted_Daily_ret = Daily_ret*MC_weight) %>% 
  ungroup() %>% 
  group_by(Year_Month, BICS_LEVEL_1_SECTOR_NAME) %>% 
  summarise(Sharpe = mean(Weighted_Daily_ret, na.rm =TRUE)/ sd(Weighted_Daily_ret, na.rm = TRUE)) %>% 
  ungroup()
print(Sharpe)

#Start by caculating the financial returns per country
Financial <- Tidydata %>%
  filter(BICS_LEVEL_1_SECTOR_NAME %in% "Financials") %>% 
  group_by(Ticker) %>% 
  mutate(Returns = TRI/lag(TRI) - 1) %>% ungroup() %>% 
  mutate(Returns = coalesce(Returns, 0)) %>% 
  group_by(Universe) %>% 
  mutate(Cum_Return = cumprod(1 + Returns)) %>%  ungroup()
#plot the returns
rmsfuns::load_pkg("ggthemes") 
Cumreturns <- Financial %>% select(date, Cum_Return, Universe) 

ggplot(data = Cumreturns) + geom_line(aes(x = date, y = Cum_Return, colour = Universe))


jalsh <- Tidydata %>% 
  filter(Universe %in% "JALSHAll") %>% 
  filter(date == "2017-01-02") %>% 
  #calculate market cap shares per ticker
  mutate(Tot_MC = sum(Market.Cap)) %>% 
  mutate(MC_weight = Market.Cap/Tot_MC) %>% 
  #filter out the top 95%
  arrange(desc(MC_weight)) %>% 
  mutate(CumSum = cumsum(MC_weight)) %>% 
  filter(CumSum <= 0.95)
#create a list with the relevant tickers
tickers <- c(jalsh$Ticker)
#create a new dataframe containing the tickers identified above  
jalsh_ret <- Tidydata %>% 
  filter(Universe %in% "JALSHAll") %>% 
  filter(Ticker %in% tickers) %>% 
  #calculate the sum of the daily returns in the index 
  group_by(date) %>% 
  mutate(Daily_indexsum = sum(TRI)) %>% 
  arrange(date) %>% 
  select(date,Universe, Daily_indexsum, Year_Month) %>% ungroup() %>% 
  unique.data.frame() %>% 
  #calculate the cumalative index returns
  mutate(Returns = Daily_indexsum/lag(Daily_indexsum) - 1) %>%
  mutate(Returns = coalesce(Returns, 0)) %>% 
  mutate(Cum_Return = cumprod(1 + Returns)) 


SD <- Tidydata %>% 
  filter(BICS_LEVEL_1_SECTOR_NAME == "Materials") %>% 
  group_by(Universe, date) %>% 
  mutate(Daily_sum = sum(TRI)) %>% 
  select(date, Universe, Daily_sum) %>% 
  unique.data.frame() %>% ungroup()


rmsfuns::load_pkg("tbl2xts")
SD_xts <- tbl_xts(tblData = SD, spread_by = "Universe")

chart.RollingPerformance(R = SD_xts,FUN = "sd", width = 60, main = "Rolling 60 day Standard Deviation", 
                         legend.loc = "bottomleft")