
library(lattice)
library(dplyr)
library(tidyverse)
library(plotly)
library(plm)
library(gplots)
library(timetk)
library(corrr)
library(corrplot)
library(caret)
library(lmtest)
library(pcse)
library(sjPlot)
library(quantmod)
library(tidyquant)
library(gfer)
library(devtools)
library(gtrendsR)
library(lubridate)
library(regclass)
library(widyr)
library(imputeTS)
library(bdscale)
library(scales)
library(clipr)
library(vtable)

###########################################################################
########################### Prep the data #################################
###########################################################################
#### 1. Get sentiment data and filter ####
wsb_reg <- read.csv('wsb_regression_data_v2.csv', header = T)

#### 2. Get Market cap ####
market_cap <- read.csv('BB_Marketcap_data.csv', header = T, sep = ';')
market_cap <- market_cap %>% 
  filter(!(Ticker %in% c('PTON', 'GNUS'))) %>%
  select(c('Ticker', 'MktCap_AVG', 'MktCap_BEG', 'MktCap_END')) %>%
  mutate(AVG_quartile = ntile(MktCap_AVG, 4),
         BEG_quartile = ntile(MktCap_BEG, 4),
         END_quartile = ntile(MktCap_END, 4)
  )
  
#### 3. Get Google Trends data ####
#### 3.1 Define function ####

get_daily_gtrend <- function(keyword = keyword, geo = geo, category = category, from = '2013-01-01', to = '2019-08-15') {
  if (ymd(to) >= floor_date(Sys.Date(), 'month')) {
    to <- floor_date(ymd(to), 'month') - days(1)
    
    if (to < from) {
      stop("Specifying \'to\' date in the current month is not allowed")
    }
  }
  
  aggregated_data <- gtrends(keyword = keyword, geo = geo, category = category, time = paste(from, to))
  if(is.null(aggregated_data$interest_over_time)) {
    print('There is no data in Google Trends!')
    return()
  }
  
  mult_m <- aggregated_data$interest_over_time %>%
    mutate(hits = as.integer(ifelse(hits == '<1', '0', hits))) %>%
    group_by(month = floor_date(date, 'month'), keyword) %>%
    summarise(hits = sum(hits)) %>%
    ungroup() %>%
    mutate(ym = format(month, '%Y-%m'),
           mult = hits / max(hits)) %>%
    select(month, ym, keyword, mult) %>%
    as_tibble()
  
  pm <- tibble(s = seq(ymd(from), ymd(to), by = 'month'), 
               e = seq(ymd(from), ymd(to), by = 'month') + months(1) - days(1))
  
  raw_trends_m <- tibble()
  
  for (i in seq(1, nrow(pm), 1)) {
    curr <- gtrends(keyword, geo = geo, time = paste(pm$s[i], pm$e[i]))
    if(is.null(curr$interest_over_time)) next
    print(paste('for', pm$s[i], pm$e[i], 'retrieved', count(curr$interest_over_time), 'days of data (all keywords)'))
    raw_trends_m <- rbind(raw_trends_m,
                          curr$interest_over_time)
  }
  
  trend_m <- raw_trends_m %>%
    select(date, keyword, hits) %>%
    mutate(ym = format(date, '%Y-%m'),
           hits = as.integer(ifelse(hits == '<1', '0', hits))) %>%
    as_tibble()
  
  trend_res <- trend_m %>%
    left_join(mult_m) %>%
    mutate(est_hits = hits * mult) %>%
    select(date, keyword, est_hits) %>%
    as_tibble() %>%
    mutate(date = as.Date(date))
  
  return(trend_res)
}

#### 3.2 Get the data ####


gtrend_gme  <- get_daily_gtrend(keyword = 'GME', geo = 'US', category = 7, from = '2020-01-01', to = '2021-12-31')
gtrend_amc  <- get_daily_gtrend(keyword = 'AMC', geo = 'US', category = 7, from = '2020-01-01', to = '2021-12-31')
gtrend_nok  <- get_daily_gtrend(keyword = 'NOK', geo = 'US', category = 7, from = '2020-01-01', to = '2021-12-31')
gtrend_pltr <- get_daily_gtrend(keyword = 'PLTR', geo = 'US', category = 7, from = '2020-08-01', to = '2021-12-31')
gtrend_tsla <- get_daily_gtrend(keyword = 'TSLA', geo = 'US', category = 7, from = '2020-01-01', to = '2021-12-31')
gtrend_sndl <- get_daily_gtrend(keyword = 'SNDL', geo = 'US', category = 7, from = '2020-08-01', to = '2021-12-31')
gtrend_clov <- get_daily_gtrend(keyword = 'CLOV', geo = 'US', category = 7, from = '2021-01-01', to = '2021-12-31')
gtrend_amd  <- get_daily_gtrend(keyword = 'AMD', geo = 'US', category = 7, from = '2020-01-01', to = '2021-12-31')
gtrend_nio  <- get_daily_gtrend(keyword = 'NIO', geo = 'US', category = 7, from = '2020-01-01', to = '2021-12-31')
gtrend_rkt  <- get_daily_gtrend(keyword = 'RKT', geo = 'US', category = 7, from = '2020-07-01', to = '2021-12-31')
gtrend_spce <- get_daily_gtrend(keyword = 'SPCE', geo = 'US', category = 7, from = '2020-01-01', to = '2021-12-31')
gtrend_uwmc <- get_daily_gtrend(keyword = 'UWMC', geo = 'US', category = 7, from = '2020-12-01', to = '2021-12-31')
gtrend_tlry <- get_daily_gtrend(keyword = 'TLRY', geo = 'US', category = 7, from = '2020-01-01', to = '2021-12-31')
gtrend_msft <- get_daily_gtrend(keyword = 'MSFT', geo = 'US', category = 7, from = '2020-01-01', to = '2021-12-31')
gtrend_aapl <- get_daily_gtrend(keyword = 'AAPL', geo = 'US', category = 7, from = '2020-01-01', to = '2021-12-31')
gtrend_wkhs <- get_daily_gtrend(keyword = 'WKHS', geo = 'US', category = 7, from = '2020-01-01', to = '2021-12-31')
gtrend_bb   <- get_daily_gtrend(keyword = 'BB', geo = 'US', category = 7, from = '2020-01-01', to = '2021-12-31')
gtrend_clne <- get_daily_gtrend(keyword = 'CLNE', geo = 'US', category = 7, from = '2020-06-01', to = '2021-12-31')
gtrend_mvis <- get_daily_gtrend(keyword = 'MVIS', geo = 'US', category = 7, from = '2020-01-01', to = '2021-12-31')
gtrend_nkla <- get_daily_gtrend(keyword = 'NKLA', geo = 'US', category = 7, from = '2020-05-01', to = '2021-12-31')
gtrend_amzn <- get_daily_gtrend(keyword = 'AMZN', geo = 'US', category = 7, from = '2020-01-01', to = '2021-12-31')
gtrend_aal  <- get_daily_gtrend(keyword = 'AAL', geo = 'US', category = 7, from = '2020-01-01', to = '2021-12-31')
gtrend_baba <- get_daily_gtrend(keyword = 'BABA', geo = 'US', category = 7, from = '2020-01-01', to = '2021-12-31')
gtrend_crsr <- get_daily_gtrend(keyword = 'CRSR', geo = 'US', category = 7, from = '2020-09-01', to = '2021-12-31')
gtrend_bbby <- get_daily_gtrend(keyword = 'BBBY', geo = 'US', category = 7, from = '2020-01-01', to = '2021-12-31')
gtrend_nvda <- get_daily_gtrend(keyword = 'NVDA', geo = 'US', category = 7, from = '2020-01-01', to = '2021-12-31')
gtrend_dkng <- get_daily_gtrend(keyword = 'DKNG', geo = 'US', category = 7, from = '2020-01-01', to = '2021-12-31')
gtrend_ctrm <- get_daily_gtrend(keyword = 'CTRM', geo = 'US', category = 7, from = '2020-06-01', to = '2021-12-31')
gtrend_fubo <- get_daily_gtrend(keyword = 'FUBO', geo = 'US', category = 7, from = '2020-01-01', to = '2021-12-31')
gtrend_sofi <- get_daily_gtrend(keyword = 'SOFI', geo = 'US', category = 7, from = '2020-01-01', to = '2021-12-31')
gtrend_ocgn <- get_daily_gtrend(keyword = 'OCGN', geo = 'US', category = 7, from = '2020-01-01', to = '2021-12-31')
gtrend_clf  <- get_daily_gtrend(keyword = 'CLF', geo = 'US', category = 7, from = '2020-01-01', to = '2021-12-31')
gtrend_acb  <- get_daily_gtrend(keyword = 'ACB', geo = 'US', category = 7, from = '2020-01-01', to = '2021-12-31')
gtrend_plug <- get_daily_gtrend(keyword = 'PLUG', geo = 'US', category = 7, from = '2020-01-01', to = '2021-12-31')
gtrend_zom  <- get_daily_gtrend(keyword = 'ZOM', geo = 'US', category = 7, from = '2020-01-01', to = '2021-12-31')

gtrend_tsla$keyword <- 'TSLA'
gtrend_aapl$keyword <- 'Apple'
gtrend_wkhs$keyword <- 'WKHS'
gtrend_baba$keyword <- 'BABA'
gtrend_dkng$keyword <- 'DKNG'
gtrend_nvda$keyword <- 'NVDA'
gtrend_plug$keyword <- 'PLUG'

gtrend_data <- do.call('rbind', list(gtrend_aal, gtrend_aapl, gtrend_acb, gtrend_amc, gtrend_amd, gtrend_amzn,
                                     gtrend_baba, gtrend_bb, gtrend_bbby, gtrend_clf, gtrend_clne, gtrend_clov,
                                     gtrend_crsr, gtrend_ctrm, gtrend_dkng, gtrend_fubo, gtrend_gme, gtrend_msft,
                                     gtrend_mvis, gtrend_nio, gtrend_nkla, gtrend_nok, gtrend_nvda, gtrend_ocgn,
                                     gtrend_pltr, gtrend_plug, gtrend_rkt, gtrend_sndl, gtrend_sofi, gtrend_spce, 
                                     gtrend_tlry, gtrend_tsla, gtrend_uwmc, gtrend_wkhs, gtrend_zom))

gtrend_data <- gtrend_data %>% rename(identity = keyword)
  
write_csv(gtrend_data, 'GoogleTrends_data.csv')

#### 3.3 Read csv ####
gtrend_data <- read.csv('GoogleTrends_data.csv', header = T)

#### 4. Get Mediacloud data ####
mediacloud <- read.csv('Mediacloud_newscount.csv', header = T, sep = ';')
mcloud_long <- mediacloud %>% gather(identity, mc_count, -c(Date))
#### 5. Get Bloomberg data ####
bb_data <- read.csv('BB_twittersentiment.csv', header = T, sep = ';')

###########################################################################
####################### Regression for returns ############################
###########################################################################
#### 1. Filter for relevant vars ####
return_vars = c(
             'Date', 'identity', 'Log_return', 'Rus_log_return',
             'Volume', 'vad_comp_sub', 'vad_comp_mean_sub', 'vad_count_sent_sub',
             'vad_sent_sub', 'lm_pol_sub', 'lm_pol_mean_sub', 'lm_count_sent_sub',
             'lm_sent_sub','ncomments_sub','nsubmissions','vad_comp_com',
             'vad_comp_mean_com','vad_count_sent_com','vad_sent_com','lm_pol_com', 
             'lm_pol_mean_com', 'lm_count_sent_com', 'lm_sent_com', 'ncomments'
)

wsb_return_filt <- wsb_reg %>%
  filter(!is.na(Log_return)) %>%
  na_replace(0) %>%
  select(return_vars)

#### 2. Create additional vars ####

wsb_return <- wsb_return_filt %>%
  group_by(identity) %>%
  mutate(log_nsubmissions         = log(nsubmissions + 1),
         log_ncomments_sub        = log(ncomments_sub + 1),
         log_ncomments            = log(ncomments + 1),
         logscl_nsubmissions      = (log_nsubmissions - min(log_nsubmissions, na.rm = T)) / (max(log_nsubmissions, na.rm = T) - min(log_nsubmissions, na.rm = T)),
         logscl_ncomments         = (log_ncomments - min(log_ncomments, na.rm = T)) / (max(log_ncomments, na.rm = T) - min(log_ncomments, na.rm = T)),
         logscl_ncomments_sub     = (log_ncomments_sub - min(log_ncomments_sub, na.rm = T)) / (max(log_ncomments_sub, na.rm = T) - min(log_ncomments_sub, na.rm = T)),
         scl_nsubmissions         = (nsubmissions - min(nsubmissions, na.rm = T)) / (max(nsubmissions, na.rm = T) - min(nsubmissions, na.rm = T)),
         scl_ncomments            = (ncomments - min(ncomments, na.rm = T)) / (max(ncomments, na.rm = T) - min(ncomments, na.rm = T)),
         scl_ncomments_sub        = (ncomments_sub - min(ncomments_sub, na.rm = T)) / (max(ncomments_sub, na.rm = T) - min(ncomments_sub, na.rm = T)),
         log_vad_comp_sub         = log(vad_comp_sub + 1 - min(vad_comp_sub, na.rm = T)),
         log_vad_comp_com         = log(vad_comp_com + 1 - min(vad_comp_com, na.rm = T)),
         log_vad_count_sent_sub   = log(vad_count_sent_sub + 1),
         log_vad_count_sent_com   = log(vad_count_sent_com + 1),
         log_lm_pol_sub           = log(lm_pol_sub + 1 - min(lm_pol_sub, na.rm = T)),
         log_lm_pol_com           = log(lm_pol_com + 1- min(lm_pol_com, na.rm = T)),
         log_lm_count_sent_sub    = log(lm_count_sent_sub + 1),
         log_lm_count_sent_com    = log(lm_count_sent_com + 1),
         logscl_vad_comp_sub      = (log_vad_comp_sub - min(log_vad_comp_sub, na.rm = T)) / (max(log_vad_comp_sub, na.rm = T) - min(log_vad_comp_sub, na.rm = T)),
         logscl_vad_comp_com      = (log_vad_comp_com - min(log_vad_comp_com, na.rm = T)) / (max(log_vad_comp_com, na.rm = T) - min(log_vad_comp_com, na.rm = T)),
         logscl_vad_count_sent_sub= (log_vad_count_sent_sub - min(log_vad_count_sent_sub, na.rm = T)) / (max(log_vad_count_sent_sub, na.rm = T) - min(log_vad_count_sent_sub, na.rm = T)),
         logscl_vad_count_sent_com= (log_vad_count_sent_com - min(log_vad_count_sent_com, na.rm = T)) / (max(log_vad_count_sent_com, na.rm = T) - min(log_vad_count_sent_com, na.rm = T)),
         scl_vad_comp_sub         = (vad_comp_sub - min(vad_comp_sub, na.rm = T)) / (max(vad_comp_sub, na.rm = T) - min(vad_comp_sub, na.rm = T)),
         scl_vad_comp_com         = (vad_comp_com - min(vad_comp_com, na.rm = T)) / (max(vad_comp_com, na.rm = T) - min(vad_comp_com, na.rm = T)),
         logscl_lm_pol_sub        = (log_lm_pol_sub - min(log_lm_pol_sub, na.rm = T)) / (max(log_lm_pol_sub, na.rm = T) - min(log_lm_pol_sub, na.rm = T)),
         logscl_lm_pol_com        = (log_lm_pol_com - min(log_lm_pol_com, na.rm = T)) / (max(log_lm_pol_com, na.rm = T) - min(log_lm_pol_com, na.rm = T)),
         logscl_lm_count_sent_sub = (log_lm_count_sent_sub - min(log_lm_count_sent_sub, na.rm = T)) / (max(log_lm_count_sent_sub, na.rm = T) - min(log_lm_count_sent_sub, na.rm = T)),
         logscl_lm_count_sent_com = (log_lm_count_sent_com - min(log_lm_count_sent_com, na.rm = T)) / (max(log_lm_count_sent_com, na.rm = T) - min(log_lm_count_sent_com, na.rm = T)),
         scl_lm_pol_sub           = (lm_pol_sub - min(lm_pol_sub, na.rm = T)) / (max(lm_pol_sub, na.rm = T) - min(lm_pol_sub, na.rm = T)),
         scl_lm_pol_com           = (lm_pol_com - min(lm_pol_com, na.rm = T)) / (max(lm_pol_com, na.rm = T) - min(lm_pol_com, na.rm = T)),
         log_volume               = log(Volume + 1),
         scl_volume               = (Volume - min(Volume, na.rm = T)) / (max(Volume, na.rm = T) - min(Volume, na.rm = T)),
         logscl_volume            = (log_volume - min(log_volume, na.rm = T)) / (max(log_volume, na.rm = T) - min(log_volume, na.rm = T))
         )

#### 3. Analyze and visualize data ####

#### 3.1 General analysis ####
str(wsb_return)
sum(is.na(wsb_return))
summary(wsb_return)

#### 3.2 Summarize variables by ticker ####
ticker_attention <- wsb_return %>%
                      group_by(identity) %>%
                      filter(nsubmissions >= 1) %>%
                      summarise(n = n(), 
                                sum_com = sum(ncomments_sub, na.rm = T),
                                sum_sub = sum(nsubmissions, na.rm = T),
                                mean_com = mean(ncomments_sub, na.rm = T),
                                mean_sub = mean(nsubmissions, na.rm = T)) 

#### 3.3 Visualize number submissions/comments  and sentiment variables (boxplots) ####

# Number submissions
wsb_return %>% filter(nsubmissions >= 1) %>% ggplot(aes(x = factor(identity), y = log_nsubmissions)) + geom_boxplot()
# Number comments
wsb_return %>% filter(ncomments >= 1) %>% ggplot(aes(x = factor(identity), y = log_ncomments)) + geom_boxplot()
# Vad scl submissions
wsb_return %>% filter(nsubmissions >= 5) %>% ggplot(aes(x = factor(identity), y =scl_vad_comp_sub)) + geom_boxplot()
# Vad scl comments
wsb_return %>% filter(ncomments >= 5) %>% ggplot(aes(x = factor(identity), y =scl_vad_comp_com)) + geom_boxplot()
# Vad logscl submissions
wsb_return %>% filter(nsubmissions >= 5) %>% ggplot(aes(x = factor(identity), y =logscl_vad_comp_sub)) + geom_boxplot()
# Vad logscl comments
wsb_return %>% filter(ncomments >= 5) %>% ggplot(aes(x = factor(identity), y =logscl_vad_comp_com)) + geom_boxplot()
# LM scl submisssions
wsb_return %>% filter(nsubmissions >= 5) %>% ggplot(aes(x = factor(identity), y =scl_lm_pol_sub)) + geom_boxplot()
# LM scl comments
wsb_return %>% filter(ncomments >= 5) %>% ggplot(aes(x = factor(identity), y = scl_lm_pol_com)) +geom_boxplot()
# LM logscl submisssions
wsb_return %>% filter(nsubmissions >= 5) %>% ggplot(aes(x = factor(identity), y =logscl_lm_pol_sub)) + geom_boxplot()
# LM logscl comments
wsb_return %>% filter(ncomments >= 5) %>% ggplot(aes(x = factor(identity), y = logscl_lm_pol_com)) +geom_boxplot()
# Vad mean submissions
wsb_return %>% filter(nsubmissions >= 5) %>% ggplot(aes(x = factor(identity), y =vad_comp_mean_sub)) + geom_boxplot()
# Vad mean comments
wsb_return %>% filter(ncomments >= 5) %>% ggplot(aes(x = factor(identity), y =vad_comp_mean_com)) + geom_boxplot()
# LM mean submissions
wsb_return %>% filter(nsubmissions >= 5) %>% ggplot(aes(x = factor(identity), y =lm_pol_mean_sub)) + geom_boxplot()
# LM mean comments
wsb_return %>% filter(ncomments >= 5) %>% ggplot(aes(x = factor(identity), y =lm_pol_mean_com)) + geom_boxplot()
# Vad sent submissions
wsb_return %>% filter(nsubmissions >= 5) %>% ggplot(aes(x = factor(identity), y =vad_sent_sub)) + geom_boxplot()
# Vad sent comments
wsb_return %>% filter(ncomments >= 5) %>% ggplot(aes(x = factor(identity), y =vad_sent_com)) + geom_boxplot()
# LM sent submissions
wsb_return %>% filter(nsubmissions >= 5) %>% ggplot(aes(x = factor(identity), y =lm_sent_sub)) + geom_boxplot()
# LM sent comments
wsb_return %>% filter(ncomments >= 5) %>% ggplot(aes(x = factor(identity), y =lm_sent_com)) + geom_boxplot()


#### 3.4 Visualize returns (per stock) ####

wsb_return %>%
  ggplot(aes(Log_return)) +
    geom_histogram(aes(y=..density..),
                   bins = 30,
                   fill = "grey20",
                   alpha = 0.5) +
    facet_wrap(~identity)

#### 3.5 Visualize returns and sentiment by stock over time ####

stock_list = c('GME', 'BB', 'NOK', 'AMC')

# Sentiment
p <- wsb_return %>%
  filter(nsubmissions >= 1) %>%
  filter((identity %in% stock_list)) %>%
  filter(Date >= '2021-01-01' & Date < '2021-04-01') %>%
  mutate(Date = as.Date(Date)) %>%
  ggplot(aes(x = Date, y = log_nsubmissions, group = identity)) +
    geom_line(aes(color = identity))

ggplotly(p)


# Returns and Sentiment
wsb_return %>%
  filter(identity == 'BB') %>%
  filter(nsubmissions >= 5) %>%
  filter(Date >= '2021-02-01' & Date < '2022-01-01') %>%
  mutate(Date = as.Date(Date)) %>%
  ggplot(aes(x = logscl_vad_comp_sub, y =Log_return, group = identity)) +
    geom_point(aes(color = identity))


#### 4. Investigate correlations ####

#### 4.1 Corr for sentiment variables ####

# select vars
cor_vars <- c('logscl_vad_comp_sub','scl_vad_comp_sub', 'vad_comp_mean_sub',
              'vad_sent_sub','logscl_vad_count_sent_sub', 
              'logscl_lm_pol_sub','scl_lm_pol_sub', 'lm_pol_mean_sub',
              'lm_sent_sub','logscl_lm_count_sent_sub',
              'logscl_vad_comp_com','scl_vad_comp_com', 'vad_comp_mean_com',
              'vad_sent_com','logscl_vad_count_sent_com', 
              'logscl_lm_pol_com','scl_lm_pol_com', 'lm_pol_mean_com',
              'lm_sent_com','logscl_lm_count_sent_com'
)
# Create cor matrix
cormatrix <- wsb_return %>%
  filter(nsubmissions >= 5 & ncomments_sub >=5, ncomments >=25) %>%
  ungroup() %>%
  select(cor_vars) %>%
  cor(use = 'pairwise.complete.obs') %>%
  round(2)

# Create corplot
corrplot(cormatrix, method="color",   
         addCoef.col = "grey20",
         type="upper",
         tl.col="black", tl.srt=45,
         number.cex = 0.7)


#### 4.2 Corr all variables ungrouped ####
cormatrix <- wsb_return %>%
      ungroup() %>%
      select(!c('Date', 'identity')) %>%
      cor(use = 'pairwise.complete.obs') %>%
      round(2)

# High correlation
findCorrelation(cormatrix, cutoff = 0.7, verbose = T)
cor <- cormatrix[, findCorrelation(cormatrix, cutoff = 0.8, verbose = T)]

# As plot
corrplot(cor, method="color",   
         addCoef.col = "grey20",
         tl.col="black", tl.srt=45,
         number.cex = 0.7)

# As list
high_corr <- which(abs(cormatrix) > 0.7 & row(cormatrix) < col(cormatrix), arr.ind = T)
high_corr_list <- matrix(colnames(cormatrix)[high_corr], ncol = 2)


#### 5. Regression prep ####

#### 5.1 Unit root test ####

# Ungroup and omit NA's
wsb_unitr <- wsb_return %>% ungroup()
wsb_unitr <- na.omit(wsb_unitr)

# Create list of relevant variables to test
colname <- colnames(wsb_unitr)
colname <- colname[-c(1,2)] # Drop identity, Date and RF
Vars <- as.list(colname)

# Prepare different models for each variable
urModelsList <- lapply(paste(Vars, '~1'), as.formula)
ipsModelsResults <- lapply(urModelsList, 
                           function(x) purtest(x, data = wsb_unitr, lags = 'AIC',
                                               index = c('identity', 'Date'), 
                                               test = 'ips', pmax = 5))  

PMModelsResults <- lapply(urModelsList, 
                             function(x) purtest(x, data = wsb_unitr, lags = 'AIC',
                                                 index = c('identity', 'Date'), 
                                                 test = 'Pm', pmax = 5))  

InvModelsResults <- lapply(urModelsList, 
                          function(x) purtest(x, data = wsb_unitr, lags = 'AIC',
                                              index = c('identity', 'Date'), 
                                              test = 'invnormal', pmax = 5))  


# Assign colnames
names(ipsModelsResults) <- colname
names(PMModelsResults) <- colname
names(InvModelsResults) <- colname

# Print output
sapply(ipsModelsResults, '[[', 1) # IPS test
sapply(PMModelsResults, '[[', 1) # 
sapply(InvModelsResults, '[[', 1) #

ipsModelsResults
#### 5.2 Create lagged variables ####
colname2 <- colnames(wsb_return)
colname2 <- colname2[-c(1,2)]
colname2

wsb_return_lag <- wsb_return %>%
  group_by(identity) %>%
  tk_augment_leads(colname2, .lag = 1:5) %>%
  tk_augment_leads(Log_return, .lag = -1:-3)

#### 5.3 Check autocorrelation for sentiment ####

autocorrelation <- wsb_return_lag %>%
  filter(nsubmissions >= 5) %>%
  summarize(n = n(),
            vad_comp1 = cor(logscl_vad_comp_sub, logscl_vad_comp_sub_lag1, method = 'pearson', use = "complete.obs"),
            vad_comp2 = cor(logscl_vad_comp_sub, logscl_vad_comp_sub_lag2, method = 'pearson', use = "complete.obs"),
            vad_comp3 = cor(logscl_vad_comp_sub, logscl_vad_comp_sub_lag3, method = 'pearson', use = "complete.obs"),
            vad_comp_mean1 = cor(vad_comp_mean_sub, vad_comp_mean_sub_lag1, method = 'pearson', use = "complete.obs"),
            vad_comp_mean2 = cor(vad_comp_mean_sub, vad_comp_mean_sub_lag2, method = 'pearson', use = "complete.obs"),
            vad_comp_mean3 = cor(vad_comp_mean_sub, vad_comp_mean_sub_lag3, method = 'pearson', use = "complete.obs"),
            vad_sent1 = cor(vad_sent_sub, vad_sent_sub_lag1, method = 'pearson', use = "complete.obs"),
            vad_sent2 = cor(vad_sent_sub, vad_sent_sub_lag2, method = 'pearson', use = "complete.obs"),
            vad_sent3 = cor(vad_sent_sub, vad_sent_sub_lag3, method = 'pearson', use = "complete.obs"),
            vad_count_sent1 = cor(vad_count_sent_sub, vad_count_sent_sub_lag1, method = 'pearson', use = "complete.obs"),
            vad_count_sent2 = cor(vad_count_sent_sub, vad_count_sent_sub_lag2, method = 'pearson', use = "complete.obs"),
            vad_count_sent3 = cor(vad_count_sent_sub, vad_count_sent_sub_lag3, method = 'pearson', use = "complete.obs"),
            lm_pol1 = cor(logscl_lm_pol_sub, logscl_lm_pol_sub_lag1, method = 'pearson', use = "complete.obs"),
            lm_pol2 = cor(logscl_lm_pol_sub, logscl_lm_pol_sub_lag2, method = 'pearson', use = "complete.obs"),
            lm_pol3 = cor(logscl_lm_pol_sub, logscl_lm_pol_sub_lag3, method = 'pearson', use = "complete.obs"),
            lm_pol_mean1 = cor(lm_pol_mean_sub, lm_pol_mean_sub_lag1, method = 'pearson', use = "complete.obs"),
            lm_pol_mean2= cor(lm_pol_mean_sub, lm_pol_mean_sub_lag2, method = 'pearson', use = "complete.obs"),
            lm_pol_mean3 = cor(lm_pol_mean_sub, lm_pol_mean_sub_lag3, method = 'pearson', use = "complete.obs"),
            lm_sent1 = cor(lm_sent_sub, lm_sent_sub_lag1, method = 'pearson', use = "complete.obs"),
            lm_sent2 = cor(lm_sent_sub, lm_sent_sub_lag2, method = 'pearson', use = "complete.obs"),
            lm_sent3 = cor(lm_sent_sub, lm_sent_sub_lag3, method = 'pearson', use = "complete.obs")
            )

summary(autocorrelation)

#### 6. Regression models for returns ####
#### 6.1 General ####

wsb_sub_return <- wsb_return_lag %>%
  group_by(identity) %>%
  filter(!(identity %in% c('PTON', 'GNUS'))) %>%
  filter(nsubmissions >= 5 & ncomments_sub >= 5)

wsb_com_return <- wsb_return_lag %>%
  group_by(identity) %>%
  filter(!(identity %in% c('PTON', 'GNUS'))) %>%
  filter(ncomments >= 25) 

psub_return <- pdata.frame(wsb_sub_return, index = c('identity', 'Date'))
pcom_return <- pdata.frame(wsb_com_return, index = c('identity', 'Date'))


#### 6.1.1 Model diagnostics ####

## Compare FE with Pooled OLS and RE ##
pooled_sub <- plm(Log_return~logscl_vad_comp_sub+logscl_vad_comp_sub_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1, 
              data = psub_return, model = 'pooling')
fixed_sub  <- plm(Log_return~logscl_vad_comp_sub_lag1+logscl_vad_comp_sub_lag2+Rus_log_return+Rus_log_return_lag1+Log_return_lag1, 
              data = psub_return, model = 'within', effect = 'twoways')
random_sub <- plm(Log_return~logscl_vad_comp_sub+logscl_vad_comp_sub_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1, 
              data = psub_return, model = 'random')

pooled_com <- plm(Log_return~logscl_vad_comp_com+logscl_vad_comp_com_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1, 
                 data = pcom_return, model = 'pooling')
fixed_com  <- plm(Log_return~logscl_vad_comp_com+logscl_vad_comp_com_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1, 
                  data = pcom_return, model = 'within', effect = 'tl')
random_com <- plm(Log_return~logscl_vad_comp_com+logscl_vad_comp_com_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1, 
                  data = pcom_return, model = 'random')

pFtest(fixed_sub, pooled_sub)
pFtest(fixed_com, pooled_com)
phtest(fixed_sub, random_sub)
phtest(fixed_com, random_com)
# Continue with fixed effect model

summary(pooled_sub)
summary(fixed_sub) 
fixef(fixed_sub)


## Determine optimal lag length ##

#Create lag models
lagtest1_models <- as.list(c(
  'logscl_vad_comp_sub+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'logscl_vad_comp_sub+logscl_vad_comp_sub_lag1+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'logscl_vad_comp_sub+logscl_vad_comp_sub_lag1+logscl_vad_comp_sub_lag2+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'logscl_vad_comp_sub+logscl_vad_comp_sub_lag1+logscl_vad_comp_sub_lag2+logscl_vad_comp_sub_lag3+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'logscl_vad_comp_sub+logscl_vad_comp_sub_lag1+logscl_vad_comp_sub_lag2+logscl_vad_comp_sub_lag3+logscl_vad_comp_sub_lag4+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'scl_vad_comp_sub+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'scl_vad_comp_sub+scl_vad_comp_sub_lag1+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'scl_vad_comp_sub+scl_vad_comp_sub_lag1+scl_vad_comp_sub_lag2+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'scl_vad_comp_sub+scl_vad_comp_sub_lag1+scl_vad_comp_sub_lag2+scl_vad_comp_sub_lag3+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'scl_vad_comp_sub+scl_vad_comp_sub_lag1+scl_vad_comp_sub_lag2+scl_vad_comp_sub_lag3+scl_vad_comp_sub_lag4+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'vad_comp_mean_sub+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'vad_comp_mean_sub+vad_comp_mean_sub_lag1+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'vad_comp_mean_sub+vad_comp_mean_sub_lag1+vad_comp_mean_sub_lag2+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'vad_comp_mean_sub+vad_comp_mean_sub_lag1+vad_comp_mean_sub_lag2+vad_comp_mean_sub_lag3+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'vad_comp_mean_sub+vad_comp_mean_sub_lag1+vad_comp_mean_sub_lag2+vad_comp_mean_sub_lag3+vad_comp_mean_sub_lag4+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'vad_sent_sub+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'vad_sent_sub+vad_sent_sub_lag1+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'vad_sent_sub+vad_sent_sub_lag1+vad_sent_sub_lag2+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'vad_sent_sub+vad_sent_sub_lag1+vad_sent_sub_lag2+vad_sent_sub_lag3+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'vad_sent_sub+vad_sent_sub_lag1+vad_sent_sub_lag2+vad_sent_sub_lag3+vad_sent_sub_lag4+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'logscl_vad_count_sent_sub+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'logscl_vad_count_sent_sub+logscl_vad_count_sent_sub_lag1+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'logscl_vad_count_sent_sub+logscl_vad_count_sent_sub_lag1+logscl_vad_count_sent_sub_lag2+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'logscl_vad_count_sent_sub+logscl_vad_count_sent_sub_lag1+logscl_vad_count_sent_sub_lag2+logscl_vad_count_sent_sub_lag3+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'logscl_vad_count_sent_sub+logscl_vad_count_sent_sub_lag1+logscl_vad_count_sent_sub_lag2+logscl_vad_count_sent_sub_lag3+logscl_vad_count_sent_sub_lag4+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'logscl_lm_pol_sub+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'logscl_lm_pol_sub+logscl_lm_pol_sub_lag1+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'logscl_lm_pol_sub+logscl_lm_pol_sub_lag1+logscl_lm_pol_sub_lag2+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'logscl_lm_pol_sub+logscl_lm_pol_sub_lag1+logscl_lm_pol_sub_lag2+logscl_lm_pol_sub_lag3+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'logscl_lm_pol_sub+logscl_lm_pol_sub_lag1+logscl_lm_pol_sub_lag2+logscl_lm_pol_sub_lag3+logscl_lm_pol_sub_lag4+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'scl_lm_pol_sub+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'scl_lm_pol_sub+scl_lm_pol_sub_lag1+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'scl_lm_pol_sub+scl_lm_pol_sub_lag1+scl_lm_pol_sub_lag2+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'scl_lm_pol_sub+scl_lm_pol_sub_lag1+scl_lm_pol_sub_lag2+scl_lm_pol_sub_lag3+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'scl_lm_pol_sub+scl_lm_pol_sub_lag1+scl_lm_pol_sub_lag2+scl_lm_pol_sub_lag3+scl_lm_pol_sub_lag4+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'lm_pol_mean_sub+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'lm_pol_mean_sub+lm_pol_mean_sub_lag1+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'lm_pol_mean_sub+lm_pol_mean_sub_lag1+lm_pol_mean_sub_lag2+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'lm_pol_mean_sub+lm_pol_mean_sub_lag1+lm_pol_mean_sub_lag2+lm_pol_mean_sub_lag3+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'lm_pol_mean_sub+lm_pol_mean_sub_lag1+lm_pol_mean_sub_lag2+lm_pol_mean_sub_lag3+lm_pol_mean_sub_lag4+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'lm_sent_sub+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'lm_sent_sub+lm_sent_sub_lag1+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'lm_sent_sub+lm_sent_sub_lag1+lm_sent_sub_lag2+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'lm_sent_sub+lm_sent_sub_lag1+lm_sent_sub_lag2+lm_sent_sub_lag3+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'lm_sent_sub+lm_sent_sub_lag1+lm_sent_sub_lag2+lm_sent_sub_lag3+lm_sent_sub_lag4+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'logscl_lm_count_sent_sub+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'logscl_lm_count_sent_sub+logscl_lm_count_sent_sub_lag1+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'logscl_lm_count_sent_sub+logscl_lm_count_sent_sub_lag1+logscl_lm_count_sent_sub_lag2+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'logscl_lm_count_sent_sub+logscl_lm_count_sent_sub_lag1+logscl_lm_count_sent_sub_lag2+logscl_lm_count_sent_sub_lag3+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'logscl_lm_count_sent_sub+logscl_lm_count_sent_sub_lag1+logscl_lm_count_sent_sub_lag2+logscl_lm_count_sent_sub_lag3+logscl_lm_count_sent_sub_lag4+Rus_log_return+Rus_log_return_lag1+Log_return_lag1'
  ))

lagtest2_models <- as.list(c(
  'logscl_vad_comp_com+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'logscl_vad_comp_com+logscl_vad_comp_com_lag1+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'logscl_vad_comp_com+logscl_vad_comp_com_lag1+logscl_vad_comp_com_lag2+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'logscl_vad_comp_com+logscl_vad_comp_com_lag1+logscl_vad_comp_com_lag2+logscl_vad_comp_com_lag3+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'logscl_vad_comp_com+logscl_vad_comp_com_lag1+logscl_vad_comp_com_lag2+logscl_vad_comp_com_lag3+logscl_vad_comp_com_lag4+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'scl_vad_comp_com+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'scl_vad_comp_com+scl_vad_comp_com_lag1+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'scl_vad_comp_com+scl_vad_comp_com_lag1+scl_vad_comp_com_lag2+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'scl_vad_comp_com+scl_vad_comp_com_lag1+scl_vad_comp_com_lag2+scl_vad_comp_com_lag3+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'scl_vad_comp_com+scl_vad_comp_com_lag1+scl_vad_comp_com_lag2+scl_vad_comp_com_lag3+scl_vad_comp_com_lag4+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'vad_comp_mean_com+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'vad_comp_mean_com+vad_comp_mean_com_lag1+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'vad_comp_mean_com+vad_comp_mean_com_lag1+vad_comp_mean_com_lag2+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'vad_comp_mean_com+vad_comp_mean_com_lag1+vad_comp_mean_com_lag2+vad_comp_mean_com_lag3+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'vad_comp_mean_com+vad_comp_mean_com_lag1+vad_comp_mean_com_lag2+vad_comp_mean_com_lag3+vad_comp_mean_com_lag4+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'vad_sent_com+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'vad_sent_com+vad_sent_com_lag1+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'vad_sent_com+vad_sent_com_lag1+vad_sent_com_lag2+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'vad_sent_com+vad_sent_com_lag1+vad_sent_com_lag2+vad_sent_com_lag3+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'vad_sent_com+vad_sent_com_lag1+vad_sent_com_lag2+vad_sent_com_lag3+vad_sent_com_lag4+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'logscl_vad_count_sent_com+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'logscl_vad_count_sent_com+logscl_vad_count_sent_com_lag1+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'logscl_vad_count_sent_com+logscl_vad_count_sent_com_lag1+logscl_vad_count_sent_com_lag2+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'logscl_vad_count_sent_com+logscl_vad_count_sent_com_lag1+logscl_vad_count_sent_com_lag2+logscl_vad_count_sent_com_lag3+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'logscl_vad_count_sent_com+logscl_vad_count_sent_com_lag1+logscl_vad_count_sent_com_lag2+logscl_vad_count_sent_com_lag3+logscl_vad_count_sent_com_lag4+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'logscl_lm_pol_com+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'logscl_lm_pol_com+logscl_lm_pol_com_lag1+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'logscl_lm_pol_com+logscl_lm_pol_com_lag1+logscl_lm_pol_com_lag2+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'logscl_lm_pol_com+logscl_lm_pol_com_lag1+logscl_lm_pol_com_lag2+logscl_lm_pol_com_lag3+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'logscl_lm_pol_com+logscl_lm_pol_com_lag1+logscl_lm_pol_com_lag2+logscl_lm_pol_com_lag3+logscl_lm_pol_com_lag4+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'scl_lm_pol_com+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'scl_lm_pol_com+scl_lm_pol_com_lag1+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'scl_lm_pol_com+scl_lm_pol_com_lag1+scl_lm_pol_com_lag2+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'scl_lm_pol_com+scl_lm_pol_com_lag1+scl_lm_pol_com_lag2+scl_lm_pol_com_lag3+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'scl_lm_pol_com+scl_lm_pol_com_lag1+scl_lm_pol_com_lag2+scl_lm_pol_com_lag3+scl_lm_pol_com_lag4+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'lm_pol_mean_com+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'lm_pol_mean_com+lm_pol_mean_com_lag1+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'lm_pol_mean_com+lm_pol_mean_com_lag1+lm_pol_mean_com_lag2+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'lm_pol_mean_com+lm_pol_mean_com_lag1+lm_pol_mean_com_lag2+lm_pol_mean_com_lag3+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'lm_pol_mean_com+lm_pol_mean_com_lag1+lm_pol_mean_com_lag2+lm_pol_mean_com_lag3+lm_pol_mean_com_lag4+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'lm_sent_com+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'lm_sent_com+lm_sent_com_lag1+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'lm_sent_com+lm_sent_com_lag1+lm_sent_com_lag2+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'lm_sent_com+lm_sent_com_lag1+lm_sent_com_lag2+lm_sent_com_lag3+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'lm_sent_com+lm_sent_com_lag1+lm_sent_com_lag2+lm_sent_com_lag3+lm_sent_com_lag4+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'logscl_lm_count_sent_com+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'logscl_lm_count_sent_com+logscl_lm_count_sent_com_lag1+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'logscl_lm_count_sent_com+logscl_lm_count_sent_com_lag1+logscl_lm_count_sent_com_lag2+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'logscl_lm_count_sent_com+logscl_lm_count_sent_com_lag1+logscl_lm_count_sent_com_lag2+logscl_lm_count_sent_com_lag3+Rus_log_return+Rus_log_return_lag1+Log_return_lag1',
  'logscl_lm_count_sent_com+logscl_lm_count_sent_com_lag1+logscl_lm_count_sent_com_lag2+logscl_lm_count_sent_com_lag3+logscl_lm_count_sent_com_lag4+Rus_log_return+Rus_log_return_lag1+Log_return_lag1'
))

# Model list
lagtest1_modellist <- lapply(paste('Log_return ~', lagtest1_models), as.formula)
lagtest2_modellist <- lapply(paste('Log_return ~', lagtest2_models), as.formula)

# Create regression models
lagtest1_modelresults <- lapply(lagtest1_modellist, function(x) plm(x, data = psub_return, model = 'within', effect = 'individual'))
lagtest2_modelresults <- lapply(lagtest2_modellist, function(x) plm(x, data = pcom_return, model = 'within', effect = 'individual'))

# Run AIC and BIC
lagtest1_AIC <- lapply(lagtest1_modelresults, function(x) AIC(x))
lagtest1_BIC <- lapply(lagtest1_modelresults, function(x) BIC(x))
lagtest2_AIC <- lapply(lagtest2_modelresults, function(x) AIC(x))
lagtest2_BIC <- lapply(lagtest2_modelresults, function(x) BIC(x))
# lag1 optimal for submissions
# lag2 optimal for comments
# --> But show regression with up to 2 lags


#### 6.1.2 General submissions ####

# Create test models
sub_models <- as.list(c(
  'Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_vad_comp_sub+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_vad_comp_sub_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_vad_comp_sub+logscl_vad_comp_sub_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_vad_comp_sub+logscl_vad_comp_sub_lag1+logscl_vad_comp_sub_lag2+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_vad_comp_sub+logscl_vad_comp_sub_lag1+logscl_vad_comp_sub_lag2+logscl_vad_comp_sub_lag3+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_vad_comp_sub+logscl_vad_comp_sub_lag1+logscl_vad_comp_sub_lag2+logscl_vad_comp_sub_lag3+Log_return_lag1',
  'logscl_vad_comp_sub+logscl_vad_comp_sub_lag1+logscl_vad_comp_sub_lag2+Log_return_lag1',
  'logscl_vad_comp_sub+logscl_vad_comp_sub_lag1+Log_return_lag1',
  'logscl_vad_comp_sub+Log_return_lag1',
  'logscl_vad_comp_sub_lag1+Log_return_lag1',
  'logscl_vad_comp_sub_lag1+logscl_vad_comp_sub_lag2+Log_return_lag1',
  'logscl_vad_comp_sub_lag1+logscl_vad_comp_sub_lag2+logscl_vad_comp_sub_lag3+Log_return_lag1',
  'scl_vad_comp_sub+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'scl_vad_comp_sub_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'scl_vad_comp_sub+scl_vad_comp_sub_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'scl_vad_comp_sub+scl_vad_comp_sub_lag1+scl_vad_comp_sub_lag2+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'scl_vad_comp_sub+scl_vad_comp_sub_lag1+scl_vad_comp_sub_lag2+scl_vad_comp_sub_lag3+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'scl_vad_comp_sub+scl_vad_comp_sub_lag1+scl_vad_comp_sub_lag2+scl_vad_comp_sub_lag3+Log_return_lag1',
  'scl_vad_comp_sub+scl_vad_comp_sub_lag1+scl_vad_comp_sub_lag2+Log_return_lag1',
  'scl_vad_comp_sub+scl_vad_comp_sub_lag1+Log_return_lag1',
  'scl_vad_comp_sub+Log_return_lag1',
  'scl_vad_comp_sub_lag1+Log_return_lag1',
  'scl_vad_comp_sub_lag1+scl_vad_comp_sub_lag2+Log_return_lag1',
  'scl_vad_comp_sub_lag1+scl_vad_comp_sub_lag2+scl_vad_comp_sub_lag3+Log_return_lag1',
  'vad_comp_mean_sub+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'vad_comp_mean_sub_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'vad_comp_mean_sub+vad_comp_mean_sub_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'vad_comp_mean_sub+vad_comp_mean_sub_lag1+vad_comp_mean_sub_lag2+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'vad_comp_mean_sub+vad_comp_mean_sub_lag1+vad_comp_mean_sub_lag2+vad_comp_mean_sub_lag3+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'vad_comp_mean_sub+vad_comp_mean_sub_lag1+vad_comp_mean_sub_lag2+vad_comp_mean_sub_lag3+Log_return_lag1',
  'vad_comp_mean_sub+vad_comp_mean_sub_lag1+vad_comp_mean_sub_lag2+Log_return_lag1',
  'vad_comp_mean_sub+vad_comp_mean_sub_lag1+Log_return_lag1',
  'vad_comp_mean_sub+Log_return_lag1',
  'vad_comp_mean_sub_lag1+Log_return_lag1',
  'vad_comp_mean_sub_lag1+vad_comp_mean_sub_lag2+Log_return_lag1',
  'vad_comp_mean_sub_lag1+vad_comp_mean_sub_lag2+vad_comp_mean_sub_lag3+Log_return_lag1',
  'vad_sent_sub+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'vad_sent_sub_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'vad_sent_sub+vad_sent_sub_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'vad_sent_sub+vad_sent_sub_lag1+vad_sent_sub_lag2+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'vad_sent_sub+vad_sent_sub_lag1+vad_sent_sub_lag2+vad_sent_sub_lag3+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'vad_sent_sub+vad_sent_sub_lag1+vad_sent_sub_lag2+vad_sent_sub_lag3+Log_return_lag1',
  'vad_sent_sub+vad_sent_sub_lag1+vad_sent_sub_lag2+Log_return_lag1',
  'vad_sent_sub+vad_sent_sub_lag1+Log_return_lag1',
  'vad_sent_sub+Log_return_lag1',
  'vad_sent_sub_lag1+Log_return_lag1',
  'vad_sent_sub_lag1+vad_sent_sub_lag2+Log_return_lag1',
  'vad_sent_sub_lag1+vad_sent_sub_lag2+vad_sent_sub_lag3+Log_return_lag1',
  'logscl_vad_count_sent_sub+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_vad_count_sent_sub_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_vad_count_sent_sub+logscl_vad_count_sent_sub_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_vad_count_sent_sub+logscl_vad_count_sent_sub_lag1+logscl_vad_count_sent_sub_lag2+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_vad_count_sent_sub+logscl_vad_count_sent_sub_lag1+logscl_vad_count_sent_sub_lag2+logscl_vad_count_sent_sub_lag3+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_vad_count_sent_sub+logscl_vad_count_sent_sub_lag1+logscl_vad_count_sent_sub_lag2+logscl_vad_count_sent_sub_lag3+Log_return_lag1',
  'logscl_vad_count_sent_sub+logscl_vad_count_sent_sub_lag1+logscl_vad_count_sent_sub_lag2+Log_return_lag1',
  'logscl_vad_count_sent_sub+logscl_vad_count_sent_sub_lag1+Log_return_lag1',
  'logscl_vad_count_sent_sub+Log_return_lag1',
  'logscl_vad_count_sent_sub_lag1+Log_return_lag1',
  'logscl_vad_count_sent_sub_lag1+logscl_vad_count_sent_sub_lag2+Log_return_lag1',
  'logscl_vad_count_sent_sub_lag1+logscl_vad_count_sent_sub_lag2+logscl_vad_count_sent_sub_lag3+Log_return_lag1',
  'logscl_lm_pol_sub+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_lm_pol_sub_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_lm_pol_sub+logscl_lm_pol_sub_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_lm_pol_sub+logscl_lm_pol_sub_lag1+logscl_lm_pol_sub_lag2+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_lm_pol_sub+logscl_lm_pol_sub_lag1+logscl_lm_pol_sub_lag2+logscl_lm_pol_sub_lag3+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_lm_pol_sub+logscl_lm_pol_sub_lag1+logscl_lm_pol_sub_lag2+logscl_lm_pol_sub_lag3+Log_return_lag1',
  'logscl_lm_pol_sub+logscl_lm_pol_sub_lag1+logscl_lm_pol_sub_lag2+Log_return_lag1',
  'logscl_lm_pol_sub+logscl_lm_pol_sub_lag1+Log_return_lag1',
  'logscl_lm_pol_sub+Log_return_lag1',
  'logscl_lm_pol_sub_lag1+Log_return_lag1',
  'logscl_lm_pol_sub_lag1+logscl_lm_pol_sub_lag2+Log_return_lag1',
  'logscl_lm_pol_sub_lag1+logscl_lm_pol_sub_lag2+logscl_lm_pol_sub_lag3+Log_return_lag1',
  'scl_lm_pol_sub+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'scl_lm_pol_sub_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'scl_lm_pol_sub+scl_lm_pol_sub_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'scl_lm_pol_sub+scl_lm_pol_sub_lag1+scl_lm_pol_sub_lag2+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'scl_lm_pol_sub+scl_lm_pol_sub_lag1+scl_lm_pol_sub_lag2+scl_lm_pol_sub_lag3+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'scl_lm_pol_sub+scl_lm_pol_sub_lag1+scl_lm_pol_sub_lag2+scl_lm_pol_sub_lag3+Log_return_lag1',
  'scl_lm_pol_sub+scl_lm_pol_sub_lag1+scl_lm_pol_sub_lag2+Log_return_lag1',
  'scl_lm_pol_sub+scl_lm_pol_sub_lag1+Log_return_lag1',
  'scl_lm_pol_sub+Log_return_lag1',
  'scl_lm_pol_sub_lag1+Log_return_lag1',
  'scl_lm_pol_sub_lag1+scl_lm_pol_sub_lag2+Log_return_lag1',
  'scl_lm_pol_sub_lag1+scl_lm_pol_sub_lag2+scl_lm_pol_sub_lag3+Log_return_lag1',
  'lm_pol_mean_sub+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'lm_pol_mean_sub_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'lm_pol_mean_sub+lm_pol_mean_sub_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'lm_pol_mean_sub+lm_pol_mean_sub_lag1+lm_pol_mean_sub_lag2+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'lm_pol_mean_sub+lm_pol_mean_sub_lag1+lm_pol_mean_sub_lag2+lm_pol_mean_sub_lag3+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'lm_pol_mean_sub+lm_pol_mean_sub_lag1+lm_pol_mean_sub_lag2+lm_pol_mean_sub_lag3+Log_return_lag1',
  'lm_pol_mean_sub+lm_pol_mean_sub_lag1+lm_pol_mean_sub_lag2+Log_return_lag1',
  'lm_pol_mean_sub+lm_pol_mean_sub_lag1+Log_return_lag1',
  'lm_pol_mean_sub+Log_return_lag1',
  'lm_pol_mean_sub_lag1+Log_return_lag1',
  'lm_pol_mean_sub_lag1+lm_pol_mean_sub_lag2+Log_return_lag1',
  'lm_pol_mean_sub_lag1+lm_pol_mean_sub_lag2+lm_pol_mean_sub_lag3+Log_return_lag1',
  'lm_sent_sub+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'lm_sent_sub_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'lm_sent_sub+lm_sent_sub_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'lm_sent_sub+lm_sent_sub_lag1+lm_sent_sub_lag2+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'lm_sent_sub+lm_sent_sub_lag1+lm_sent_sub_lag2+lm_sent_sub_lag3+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'lm_sent_sub+lm_sent_sub_lag1+lm_sent_sub_lag2+lm_sent_sub_lag3+Log_return_lag1',
  'lm_sent_sub+lm_sent_sub_lag1+lm_sent_sub_lag2+Log_return_lag1',
  'lm_sent_sub+lm_sent_sub_lag1+Log_return_lag1',
  'lm_sent_sub+Log_return_lag1',
  'lm_sent_sub_lag1+Log_return_lag1',
  'lm_sent_sub_lag1+lm_sent_sub_lag2+Log_return_lag1',
  'lm_sent_sub_lag1+lm_sent_sub_lag2+lm_sent_sub_lag3+Log_return_lag1',
  'logscl_lm_count_sent_sub+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_lm_count_sent_sub_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_lm_count_sent_sub+logscl_lm_count_sent_sub_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_lm_count_sent_sub+logscl_lm_count_sent_sub_lag1+logscl_lm_count_sent_sub_lag2+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_lm_count_sent_sub+logscl_lm_count_sent_sub_lag1+logscl_lm_count_sent_sub_lag2+logscl_lm_count_sent_sub_lag3+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_lm_count_sent_sub+logscl_lm_count_sent_sub_lag1+logscl_lm_count_sent_sub_lag2+logscl_lm_count_sent_sub_lag3+Log_return_lag1',
  'logscl_lm_count_sent_sub+logscl_lm_count_sent_sub_lag1+logscl_lm_count_sent_sub_lag2+Log_return_lag1',
  'logscl_lm_count_sent_sub+logscl_lm_count_sent_sub_lag1+Log_return_lag1',
  'logscl_lm_count_sent_sub+Log_return_lag1',
  'logscl_lm_count_sent_sub_lag1+Log_return_lag1',
  'logscl_lm_count_sent_sub_lag1+logscl_lm_count_sent_sub_lag2+Log_return_lag1',
  'logscl_lm_count_sent_sub_lag1+logscl_lm_count_sent_sub_lag2+logscl_lm_count_sent_sub_lag3+Log_return_lag1'
))

# Model names
submodel_names <- c('market',
                   'logscl_vad_comp', 'logscl_vad_comp', 'logscl_vad_comp', 'logscl_vad_comp', 'logscl_vad_comp', 'logscl_vad_comp',
                   'logscl_vad_comp', 'logscl_vad_comp', 'logscl_vad_comp', 'logscl_vad_comp', 'logscl_vad_comp', 'logscl_vad_comp',
                   'scl_vad_comp', 'scl_vad_comp', 'scl_vad_comp', 'scl_vad_comp', 'scl_vad_comp', 'scl_vad_comp',
                   'scl_vad_comp', 'scl_vad_comp', 'scl_vad_comp', 'scl_vad_comp', 'scl_vad_comp', 'scl_vad_comp',
                   'vad_comp_mean', 'vad_comp_mean', 'vad_comp_mean', 'vad_comp_mean', 'vad_comp_mean', 'vad_comp_mean',
                   'vad_comp_mean', 'vad_comp_mean', 'vad_comp_mean', 'vad_comp_mean', 'vad_comp_mean', 'vad_comp_mean',
                   'vad_sent', 'vad_sent', 'vad_sent', 'vad_sent', 'vad_sent', 'vad_sent',
                   'vad_sent', 'vad_sent', 'vad_sent', 'vad_sent', 'vad_sent', 'vad_sent',
                   'vad_count_sent', 'vad_count_sent', 'vad_count_sent', 'vad_count_sent', 'vad_count_sent', 'vad_count_sent',
                   'vad_count_sent', 'vad_count_sent', 'vad_count_sent', 'vad_count_sent', 'vad_count_sent', 'vad_count_sent',
                   'logscl_lm_pol', 'logscl_lm_pol', 'logscl_lm_pol', 'logscl_lm_pol', 'logscl_lm_pol', 'logscl_lm_pol',
                   'logscl_lm_pol', 'logscl_lm_pol', 'logscl_lm_pol', 'logscl_lm_pol', 'logscl_lm_pol', 'logscl_lm_pol',
                   'scl_lm_pol', 'scl_lm_pol', 'scl_lm_pol', 'scl_lm_pol', 'scl_lm_pol', 'scl_lm_pol',
                   'scl_lm_pol', 'scl_lm_pol', 'scl_lm_pol', 'scl_lm_pol', 'scl_lm_pol', 'scl_lm_pol',
                   'lm_pol_mean', 'lm_pol_mean', 'lm_pol_mean', 'lm_pol_mean', 'lm_pol_mean', 'lm_pol_mean',
                   'lm_pol_mean', 'lm_pol_mean', 'lm_pol_mean', 'lm_pol_mean', 'lm_pol_mean', 'lm_pol_mean',
                   'lm_sent', 'lm_sent', 'lm_sent', 'lm_sent', 'lm_sent', 'lm_sent',
                   'lm_sent', 'lm_sent', 'lm_sent', 'lm_sent', 'lm_sent', 'lm_sent',
                   'lm_count_sent', 'lm_count_sent', 'lm_count_sent', 'lm_count_sent', 'lm_count_sent', 'lm_count_sent',
                   'lm_count_sent', 'lm_count_sent', 'lm_count_sent', 'lm_count_sent', 'lm_count_sent', 'lm_count_sent'
                   )

# Create final model list
sub_modellist <- lapply(paste('Log_return ~', sub_models), as.formula)

# Run models
sub_modelresults <- lapply(sub_modellist, function(x) plm(x, data = psub_return, model = 'within', effect = 'individual'))
names(sub_modelresults) <- submodel_names
sub_modelsummary <- lapply(sub_modelresults, function(x) summary(x, vcov = vcovSCC))

# Run diagnostic tests
sub_pcdtest <- lapply(sub_modelresults, function(x) pcdtest(x, test = c('cd')))
sub_pbgtest <- lapply(sub_modelresults, function(x) pbgtest(x))
sub_bptest <- lapply(sub_modelresults, function(x) bptest(x))

#### 6.1.3 General comments ####

# Create models
com_models <- as.list(c(
  'Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_vad_comp_com+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_vad_comp_com_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_vad_comp_com+logscl_vad_comp_com_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_vad_comp_com+logscl_vad_comp_com_lag1+logscl_vad_comp_com_lag2+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_vad_comp_com+logscl_vad_comp_com_lag1+logscl_vad_comp_com_lag2+logscl_vad_comp_com_lag3+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_vad_comp_com+logscl_vad_comp_com_lag1+logscl_vad_comp_com_lag2+logscl_vad_comp_com_lag3+Log_return_lag1',
  'logscl_vad_comp_com+logscl_vad_comp_com_lag1+logscl_vad_comp_com_lag2+Log_return_lag1',
  'logscl_vad_comp_com+logscl_vad_comp_com_lag1+Log_return_lag1',
  'logscl_vad_comp_com+Log_return_lag1',
  'logscl_vad_comp_com_lag1+Log_return_lag1',
  'logscl_vad_comp_com_lag1+logscl_vad_comp_com_lag2+Log_return_lag1',
  'logscl_vad_comp_com_lag1+logscl_vad_comp_com_lag2+logscl_vad_comp_com_lag3+Log_return_lag1',
  'scl_vad_comp_com+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'scl_vad_comp_com_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'scl_vad_comp_com+scl_vad_comp_com_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'scl_vad_comp_com+scl_vad_comp_com_lag1+scl_vad_comp_com_lag2+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'scl_vad_comp_com+scl_vad_comp_com_lag1+scl_vad_comp_com_lag2+scl_vad_comp_com_lag3+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'scl_vad_comp_com+scl_vad_comp_com_lag1+scl_vad_comp_com_lag2+scl_vad_comp_com_lag3+Log_return_lag1',
  'scl_vad_comp_com+scl_vad_comp_com_lag1+scl_vad_comp_com_lag2+Log_return_lag1',
  'scl_vad_comp_com+scl_vad_comp_com_lag1+Log_return_lag1',
  'scl_vad_comp_com+Log_return_lag1',
  'scl_vad_comp_com_lag1+Log_return_lag1',
  'scl_vad_comp_com_lag1+scl_vad_comp_com_lag2+Log_return_lag1',
  'scl_vad_comp_com_lag1+scl_vad_comp_com_lag2+scl_vad_comp_com_lag3+Log_return_lag1',
  'vad_comp_mean_com+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'vad_comp_mean_com_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'vad_comp_mean_com+vad_comp_mean_com_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'vad_comp_mean_com+vad_comp_mean_com_lag1+vad_comp_mean_com_lag2+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'vad_comp_mean_com+vad_comp_mean_com_lag1+vad_comp_mean_com_lag2+vad_comp_mean_com_lag3+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'vad_comp_mean_com+vad_comp_mean_com_lag1+vad_comp_mean_com_lag2+vad_comp_mean_com_lag3+Log_return_lag1',
  'vad_comp_mean_com+vad_comp_mean_com_lag1+vad_comp_mean_com_lag2+Log_return_lag1',
  'vad_comp_mean_com+vad_comp_mean_com_lag1+Log_return_lag1',
  'vad_comp_mean_com+Log_return_lag1',
  'vad_comp_mean_com_lag1+Log_return_lag1',
  'vad_comp_mean_com_lag1+vad_comp_mean_com_lag2+Log_return_lag1',
  'vad_comp_mean_com_lag1+vad_comp_mean_com_lag2+vad_comp_mean_com_lag3+Log_return_lag1',
  'vad_sent_com+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'vad_sent_com_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'vad_sent_com+vad_sent_com_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'vad_sent_com+vad_sent_com_lag1+vad_sent_com_lag2+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'vad_sent_com+vad_sent_com_lag1+vad_sent_com_lag2+vad_sent_com_lag3+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'vad_sent_com+vad_sent_com_lag1+vad_sent_com_lag2+vad_sent_com_lag3+Log_return_lag1',
  'vad_sent_com+vad_sent_com_lag1+vad_sent_com_lag2+Log_return_lag1',
  'vad_sent_com+vad_sent_com_lag1+Log_return_lag1',
  'vad_sent_com+Log_return_lag1',
  'vad_sent_com_lag1+Log_return_lag1',
  'vad_sent_com_lag1+vad_sent_com_lag2+Log_return_lag1',
  'vad_sent_com_lag1+vad_sent_com_lag2+vad_sent_com_lag3+Log_return_lag1',
  'logscl_vad_count_sent_com+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_vad_count_sent_com_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_vad_count_sent_com+logscl_vad_count_sent_com_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_vad_count_sent_com+logscl_vad_count_sent_com_lag1+logscl_vad_count_sent_com_lag2+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_vad_count_sent_com+logscl_vad_count_sent_com_lag1+logscl_vad_count_sent_com_lag2+logscl_vad_count_sent_com_lag3+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_vad_count_sent_com+logscl_vad_count_sent_com_lag1+logscl_vad_count_sent_com_lag2+logscl_vad_count_sent_com_lag3+Log_return_lag1',
  'logscl_vad_count_sent_com+logscl_vad_count_sent_com_lag1+logscl_vad_count_sent_com_lag2+Log_return_lag1',
  'logscl_vad_count_sent_com+logscl_vad_count_sent_com_lag1+Log_return_lag1',
  'logscl_vad_count_sent_com+Log_return_lag1',
  'logscl_vad_count_sent_com_lag1+Log_return_lag1',
  'logscl_vad_count_sent_com_lag1+logscl_vad_count_sent_com_lag2+Log_return_lag1',
  'logscl_vad_count_sent_com_lag1+logscl_vad_count_sent_com_lag2+logscl_vad_count_sent_com_lag3+Log_return_lag1',
  'logscl_lm_pol_com+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_lm_pol_com_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_lm_pol_com+logscl_lm_pol_com_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_lm_pol_com+logscl_lm_pol_com_lag1+logscl_lm_pol_com_lag2+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_lm_pol_com+logscl_lm_pol_com_lag1+logscl_lm_pol_com_lag2+logscl_lm_pol_com_lag3+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_lm_pol_com+logscl_lm_pol_com_lag1+logscl_lm_pol_com_lag2+logscl_lm_pol_com_lag3+Log_return_lag1',
  'logscl_lm_pol_com+logscl_lm_pol_com_lag1+logscl_lm_pol_com_lag2+Log_return_lag1',
  'logscl_lm_pol_com+logscl_lm_pol_com_lag1+Log_return_lag1',
  'logscl_lm_pol_com+Log_return_lag1',
  'logscl_lm_pol_com_lag1+Log_return_lag1',
  'logscl_lm_pol_com_lag1+logscl_lm_pol_com_lag2+Log_return_lag1',
  'logscl_lm_pol_com_lag1+logscl_lm_pol_com_lag2+logscl_lm_pol_com_lag3+Log_return_lag1',
  'scl_lm_pol_com+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'scl_lm_pol_com_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'scl_lm_pol_com+scl_lm_pol_com_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'scl_lm_pol_com+scl_lm_pol_com_lag1+scl_lm_pol_com_lag2+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'scl_lm_pol_com+scl_lm_pol_com_lag1+scl_lm_pol_com_lag2+scl_lm_pol_com_lag3+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'scl_lm_pol_com+scl_lm_pol_com_lag1+scl_lm_pol_com_lag2+scl_lm_pol_com_lag3+Log_return_lag1',
  'scl_lm_pol_com+scl_lm_pol_com_lag1+scl_lm_pol_com_lag2+Log_return_lag1',
  'scl_lm_pol_com+scl_lm_pol_com_lag1+Log_return_lag1',
  'scl_lm_pol_com+Log_return_lag1',
  'scl_lm_pol_com_lag1+Log_return_lag1',
  'scl_lm_pol_com_lag1+scl_lm_pol_com_lag2+Log_return_lag1',
  'scl_lm_pol_com_lag1+scl_lm_pol_com_lag2+scl_lm_pol_com_lag3+Log_return_lag1',
  'lm_pol_mean_com+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'lm_pol_mean_com_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'lm_pol_mean_com+lm_pol_mean_com_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'lm_pol_mean_com+lm_pol_mean_com_lag1+lm_pol_mean_com_lag2+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'lm_pol_mean_com+lm_pol_mean_com_lag1+lm_pol_mean_com_lag2+lm_pol_mean_com_lag3+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'lm_pol_mean_com+lm_pol_mean_com_lag1+lm_pol_mean_com_lag2+lm_pol_mean_com_lag3+Log_return_lag1',
  'lm_pol_mean_com+lm_pol_mean_com_lag1+lm_pol_mean_com_lag2+Log_return_lag1',
  'lm_pol_mean_com+lm_pol_mean_com_lag1+Log_return_lag1',
  'lm_pol_mean_com+Log_return_lag1',
  'lm_pol_mean_com_lag1+Log_return_lag1',
  'lm_pol_mean_com_lag1+lm_pol_mean_com_lag2+Log_return_lag1',
  'lm_pol_mean_com_lag1+lm_pol_mean_com_lag2+lm_pol_mean_com_lag3+Log_return_lag1',
  'lm_sent_com+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'lm_sent_com_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'lm_sent_com+lm_sent_com_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'lm_sent_com+lm_sent_com_lag1+lm_sent_com_lag2+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'lm_sent_com+lm_sent_com_lag1+lm_sent_com_lag2+lm_sent_com_lag3+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'lm_sent_com+lm_sent_com_lag1+lm_sent_com_lag2+lm_sent_com_lag3+Log_return_lag1',
  'lm_sent_com+lm_sent_com_lag1+lm_sent_com_lag2+Log_return_lag1',
  'lm_sent_com+lm_sent_com_lag1+Log_return_lag1',
  'lm_sent_com+Log_return_lag1',
  'lm_sent_com_lag1+Log_return_lag1',
  'lm_sent_com_lag1+lm_sent_com_lag2+Log_return_lag1',
  'lm_sent_com_lag1+lm_sent_com_lag2+lm_sent_com_lag3+Log_return_lag1',
  'logscl_lm_count_sent_com+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_lm_count_sent_com_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_lm_count_sent_com+logscl_lm_count_sent_com_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_lm_count_sent_com+logscl_lm_count_sent_com_lag1+logscl_lm_count_sent_com_lag2+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_lm_count_sent_com+logscl_lm_count_sent_com_lag1+logscl_lm_count_sent_com_lag2+logscl_lm_count_sent_com_lag3+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_lm_count_sent_com+logscl_lm_count_sent_com_lag1+logscl_lm_count_sent_com_lag2+logscl_lm_count_sent_com_lag3+Log_return_lag1',
  'logscl_lm_count_sent_com+logscl_lm_count_sent_com_lag1+logscl_lm_count_sent_com_lag2+Log_return_lag1',
  'logscl_lm_count_sent_com+logscl_lm_count_sent_com_lag1+Log_return_lag1',
  'logscl_lm_count_sent_com+Log_return_lag1',
  'logscl_lm_count_sent_com_lag1+Log_return_lag1',
  'logscl_lm_count_sent_com_lag1+logscl_lm_count_sent_com_lag2+Log_return_lag1',
  'logscl_lm_count_sent_com_lag1+logscl_lm_count_sent_com_lag2+logscl_lm_count_sent_com_lag3+Log_return_lag1'
))

# Model names
commodel_names <- c('market',
                    'logscl_vad_comp', 'logscl_vad_comp', 'logscl_vad_comp', 'logscl_vad_comp', 'logscl_vad_comp', 'logscl_vad_comp',
                    'logscl_vad_comp', 'logscl_vad_comp', 'logscl_vad_comp', 'logscl_vad_comp', 'logscl_vad_comp', 'logscl_vad_comp',
                    'scl_vad_comp', 'scl_vad_comp', 'scl_vad_comp', 'scl_vad_comp', 'scl_vad_comp', 'scl_vad_comp',
                    'scl_vad_comp', 'scl_vad_comp', 'scl_vad_comp', 'scl_vad_comp', 'scl_vad_comp', 'scl_vad_comp',
                    'vad_comp_mean', 'vad_comp_mean', 'vad_comp_mean', 'vad_comp_mean', 'vad_comp_mean', 'vad_comp_mean',
                    'vad_comp_mean', 'vad_comp_mean', 'vad_comp_mean', 'vad_comp_mean', 'vad_comp_mean', 'vad_comp_mean',
                    'vad_sent', 'vad_sent', 'vad_sent', 'vad_sent', 'vad_sent', 'vad_sent',
                    'vad_sent', 'vad_sent', 'vad_sent', 'vad_sent', 'vad_sent', 'vad_sent',
                    'vad_count_sent', 'vad_count_sent', 'vad_count_sent', 'vad_count_sent', 'vad_count_sent', 'vad_count_sent',
                    'vad_count_sent', 'vad_count_sent', 'vad_count_sent', 'vad_count_sent', 'vad_count_sent', 'vad_count_sent',
                    'logscl_lm_pol', 'logscl_lm_pol', 'logscl_lm_pol', 'logscl_lm_pol', 'logscl_lm_pol', 'logscl_lm_pol',
                    'logscl_lm_pol', 'logscl_lm_pol', 'logscl_lm_pol', 'logscl_lm_pol', 'logscl_lm_pol', 'logscl_lm_pol',
                    'scl_lm_pol', 'scl_lm_pol', 'scl_lm_pol', 'scl_lm_pol', 'scl_lm_pol', 'scl_lm_pol',
                    'scl_lm_pol', 'scl_lm_pol', 'scl_lm_pol', 'scl_lm_pol', 'scl_lm_pol', 'scl_lm_pol',
                    'lm_pol_mean', 'lm_pol_mean', 'lm_pol_mean', 'lm_pol_mean', 'lm_pol_mean', 'lm_pol_mean',
                    'lm_pol_mean', 'lm_pol_mean', 'lm_pol_mean', 'lm_pol_mean', 'lm_pol_mean', 'lm_pol_mean',
                    'lm_sent', 'lm_sent', 'lm_sent', 'lm_sent', 'lm_sent', 'lm_sent',
                    'lm_sent', 'lm_sent', 'lm_sent', 'lm_sent', 'lm_sent', 'lm_sent',
                    'lm_count_sent', 'lm_count_sent', 'lm_count_sent', 'lm_count_sent', 'lm_count_sent', 'lm_count_sent',
                    'lm_count_sent', 'lm_count_sent', 'lm_count_sent', 'lm_count_sent', 'lm_count_sent', 'lm_count_sent'
)

# Final model list
com_modellist <- lapply(paste('Log_return ~', com_models), as.formula)

# Run model regressions
com_modelresults <- lapply(com_modellist, function(x) plm(x, data = pcom_return, model = 'within', effect = 'individual'))
names(com_modelresults) <- commodel_names
com_modelsummary <- lapply(com_modelresults, function(x) summary(x, vcov = vcovSCC))

# Run diagnostic tests
com_pcdtest <- lapply(com_modelresults, function(x) pcdtest(x, test = c('cd')))
com_pbgtest <- lapply(com_modelresults, function(x) pbgtest(x))
com_bptest <- lapply(com_modelresults, function(x) bptest(x))

#### 6.2 Split by time ####

# Create different perios
start_list <- list('2020-01-01', '2021-01-01', '2021-07-01')
end_list   <- list('2021-01-01', '2021-07-01', '2022-01-01')
names(start_list) <- c('2020', '2021H1', '2021H2')

# Create df subset for each period
date_dfs <- list()
for (i in 1:length(start_list)) {
  date_dfs[[names(start_list)[i]]] <- wsb_sub_return %>% filter((Date >= start_list[[i]]) & (Date < end_list[[i]])) %>% pdata.frame(index = c('identity', 'Date'))
}


# Create models
subtime_models <- as.list(c(
  'Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_vad_comp_sub+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_vad_comp_sub_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_vad_comp_sub+logscl_vad_comp_sub_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_vad_comp_sub+logscl_vad_comp_sub_lag1+logscl_vad_comp_sub_lag2+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_vad_comp_sub+logscl_vad_comp_sub_lag1+logscl_vad_comp_sub_lag2+Log_return_lag1',
  'logscl_vad_comp_sub+logscl_vad_comp_sub_lag1+Log_return_lag1',
  'logscl_vad_comp_sub+Log_return_lag1',
  'logscl_vad_comp_sub_lag1+Log_return_lag1',
  'logscl_vad_comp_sub_lag1+logscl_vad_comp_sub_lag2+Log_return_lag1',
  'scl_vad_comp_sub+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'scl_vad_comp_sub_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'scl_vad_comp_sub+scl_vad_comp_sub_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'scl_vad_comp_sub+scl_vad_comp_sub_lag1+scl_vad_comp_sub_lag2+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'scl_vad_comp_sub+scl_vad_comp_sub_lag1+scl_vad_comp_sub_lag2+Log_return_lag1',
  'scl_vad_comp_sub+scl_vad_comp_sub_lag1+Log_return_lag1',
  'scl_vad_comp_sub+Log_return_lag1',
  'scl_vad_comp_sub_lag1+Log_return_lag1',
  'scl_vad_comp_sub_lag1+scl_vad_comp_sub_lag2+Log_return_lag1'
  ))

# Final model list
subtime_modellist <- lapply(paste('Log_return ~', subtime_models), as.formula)

# Run regresion for each subset
subtime_modelresults <- list()
subtime_modelsummary <- list()
for (date in date_dfs){
  results <- lapply(subtime_modellist, function(x) plm(x, data = date, model = 'within', effect = 'individual'))
  modelsummary <- lapply(results, function(x) summary(x, vcovSCC))
  subtime_modelresults <- c(subtime_modelresults, list(results))
  subtime_modelsummary <- c(subtime_modelsummary, list(modelsummary))
}
names(subtime_modelresults) <- names(start_list)
names(subtime_modelsummary) <- names(start_list)

#### 6.3 Split by stocks ####
#### 6.3.1 Split by stocks - Market cap ####

# Create models
mktcap_models_sub <- as.list(c(
  'Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_vad_comp_sub+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_vad_comp_sub_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_vad_comp_sub+logscl_vad_comp_sub_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_vad_comp_sub+logscl_vad_comp_sub_lag1+logscl_vad_comp_sub_lag2+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_vad_comp_sub+logscl_vad_comp_sub_lag1+logscl_vad_comp_sub_lag2+Log_return_lag1',
  'logscl_vad_comp_sub+logscl_vad_comp_sub_lag1+Log_return_lag1',
  'logscl_vad_comp_sub+Log_return_lag1',
  'logscl_vad_comp_sub_lag1+Log_return_lag1',
  'logscl_vad_comp_sub_lag1+logscl_vad_comp_sub_lag2+Log_return_lag1'
))

# Final list
mktcap_modellist <- lapply(paste('Log_return ~', mktcap_models_sub), as.formula)

# Split into df subsets by Market cap
Mktcap_dfs <- list()
ticker_quantiles <- c(1,2,3,4)

for (i in 1:length(ticker_quantiles)) {
  Mktcap_dfs[[i]] <- wsb_sub_return %>% 
    filter(identity %in% subset(market_cap, BEG_quartile == i)$Ticker) %>% 
    pdata.frame(index = c('identity', 'Date'))
}

# Run regressions for each subset
Mktcap_modelresults <- list()
Mktcap_modelsummary <- list()
for (cap in Mktcap_dfs){
  results <- lapply(mktcap_modellist, function(x) plm(x, data = cap, model = 'within', effect = 'individual'))
  modelsummary <- lapply(results, function(x) summary(x, vcov = vcovSCC))
  Mktcap_modelresults <- c(Mktcap_modelresults, list(results))
  Mktcap_modelsummary <- c(Mktcap_modelsummary, list(modelsummary))
}
names(Mktcap_modelresults) <- c('Q1', 'Q2', 'Q3', 'Q4')
names(Mktcap_modelsummary) <- c('Q1', 'Q2', 'Q3', 'Q4')


#### 6.3.2 Split by stocks submissions - Attention ####

# Create models
attention_models_sub <- as.list(c(
  'Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_vad_comp_sub+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_vad_comp_sub_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_vad_comp_sub+logscl_vad_comp_sub_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_vad_comp_sub+logscl_vad_comp_sub_lag1+logscl_vad_comp_sub_lag2+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_vad_comp_sub+logscl_vad_comp_sub_lag1+logscl_vad_comp_sub_lag2+Log_return_lag1',
  'logscl_vad_comp_sub+logscl_vad_comp_sub_lag1+Log_return_lag1',
  'logscl_vad_comp_sub+Log_return_lag1',
  'logscl_vad_comp_sub_lag1+Log_return_lag1',
  'logscl_vad_comp_sub_lag1+logscl_vad_comp_sub_lag2+Log_return_lag1',
  'scl_vad_comp_sub+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'scl_vad_comp_sub_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'scl_vad_comp_sub+scl_vad_comp_sub_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'scl_vad_comp_sub+scl_vad_comp_sub_lag1+scl_vad_comp_sub_lag2+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'scl_vad_comp_sub+scl_vad_comp_sub_lag1+scl_vad_comp_sub_lag2+Log_return_lag1',
  'scl_vad_comp_sub+scl_vad_comp_sub_lag1+Log_return_lag1',
  'scl_vad_comp_sub+Log_return_lag1',
  'scl_vad_comp_sub_lag1+Log_return_lag1',
  'scl_vad_comp_sub_lag1+scl_vad_comp_sub_lag2+Log_return_lag1',
  'logscl_lm_count_sent_sub+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_lm_count_sent_sub_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_lm_count_sent_sub+logscl_lm_count_sent_sub_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_lm_count_sent_sub+logscl_lm_count_sent_sub_lag1+logscl_lm_count_sent_sub_lag2+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_lm_count_sent_sub+logscl_lm_count_sent_sub_lag1+logscl_lm_count_sent_sub_lag2+Log_return_lag1',
  'logscl_lm_count_sent_sub+logscl_lm_count_sent_sub_lag1+Log_return_lag1',
  'logscl_lm_count_sent_sub+Log_return_lag1',
  'logscl_lm_count_sent_sub_lag1+Log_return_lag1',
  'logscl_lm_count_sent_sub_lag1+logscl_lm_count_sent_sub_lag2+Log_return_lag1'
))

# Final model list
attentionsplit_modellist <- lapply(paste('Log_return ~', attention_models_sub), as.formula)


# Create quartiles for attention 
ticker_attention_adj <- ticker_attention %>%
            mutate(sub_quartile = ntile(mean_sub, 4))

# Splitting the data set
attention_dfs <- list()
ticker_quantiles <- c(1,2,3,4)

for (i in 1:length(ticker_quantiles)) {
  attention_dfs[[i]] <- wsb_sub_return %>% 
            filter(identity %in% subset(ticker_attention_adj, sub_quartile == i)$identity) %>% 
            pdata.frame(index = c('identity', 'Date'))
}

# Run regression for each subset
attention_modelresults <- list()
attention_modelsummary <- list()
for (quant in attention_dfs){
  results <- lapply(attentionsplit_modellist, function(x) plm(x, data = quant, model = 'within', effect = 'individual'))
  modelsummary <- lapply(results, function(x) summary(x, vcovSCC))
  attention_modelresults <- c(attention_modelresults, list(results))
  attention_modelsummary <- c(attention_modelsummary, list(modelsummary))
}
names(attention_modelresults) <- c('Q1', 'Q2', 'Q3', 'Q4')
names(attention_modelsummary) <- c('Q1', 'Q2', 'Q3', 'Q4')

attention_modelsummary[4]


#### 6.3.3 Split by stocks submissions - Attention without Large stocks ####

# Create models
attention_models_sub2 <- as.list(c(
  'Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_vad_comp_sub+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_vad_comp_sub_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_vad_comp_sub+logscl_vad_comp_sub_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_vad_comp_sub+logscl_vad_comp_sub_lag1+logscl_vad_comp_sub_lag2+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_vad_comp_sub+logscl_vad_comp_sub_lag1+logscl_vad_comp_sub_lag2+Log_return_lag1',
  'logscl_vad_comp_sub+logscl_vad_comp_sub_lag1+Log_return_lag1',
  'logscl_vad_comp_sub+Log_return_lag1',
  'logscl_vad_comp_sub_lag1+Log_return_lag1',
  'logscl_vad_comp_sub_lag1+logscl_vad_comp_sub_lag2+Log_return_lag1'
))

# Final model list
attentionsplit_modellist2 <- lapply(paste('Log_return ~', attention_models_sub2), as.formula)

# Split attention into quartiles exlusing large stocks
ticker_attention_adj2 <- ticker_attention %>%
  filter(!(identity %in% subset(market_cap, BEG_quartile == 4)$Ticker)) %>%
  mutate(sub_quartile = ntile(mean_sub, 4))

# Splitting the data set
attention_dfs2 <- list()
ticker_quantiles <- c(1,2,3,4)

for (i in 1:length(ticker_quantiles)) {
  attention_dfs2[[i]] <- wsb_sub_return %>% 
    filter(!(identity %in% subset(market_cap, BEG_quartile == 4)$Ticker)) %>%
    filter(identity %in% subset(ticker_attention_adj2, sub_quartile == i)$identity) %>% 
    pdata.frame(index = c('identity', 'Date'))
}

# Run regresion for each subset
attention_modelresults2 <- list()
attention_modelsummary2 <- list()
for (quant in attention_dfs2){
  results <- lapply(attentionsplit_modellist2, function(x) plm(x, data = quant, model = 'within', effect = 'individual'))
  modelsummary <- lapply(results, function(x) summary(x, vcovSCC))
  attention_modelresults2 <- c(attention_modelresults2, list(results))
  attention_modelsummary2 <- c(attention_modelsummary2, list(modelsummary))
}
names(attention_modelresults2) <- c('Q1', 'Q2', 'Q3', 'Q4')
names(attention_modelsummary2) <- c('Q1', 'Q2', 'Q3', 'Q4')

attention_modelsummary2[4]


#### 6.3.4 High Attention small and medium stocks####

HighA_nobig_df <- wsb_sub_return %>% 
  filter(!(identity %in% Large_stocks)) %>%
  filter(!(identity %in% c('AMC', 'GME'))) %>%
  filter(!(identity %in% subset(ticker_attention_adj2, sub_quartile %in% c(1))$identity)) %>%
  pdata.frame(index = c('identity', 'Date'))

HighA_nobig_results <- lapply(attentionsplit_modellist2, function(x) plm(x, data = HighA_nobig_df, model = 'within', effect = 'individual'))
HighA_nobig_modelsummary <- lapply(HighA_nobig_results, function(x) summary(x, vcov = vcovSCC))
HighA_nobig_modelsummary 



HighA_nobig_noga_df <- wsb_sub_return %>% 
  filter(!(identity %in% Large_stocks)) %>%
  filter(!(identity %in% c('AMC', 'GME'))) %>%
  filter(!(identity %in% subset(ticker_attention_adj2, sub_quartile %in% c(1))$identity)) %>%
  pdata.frame(index = c('identity', 'Date'))

HighA_nobig_noga_results <- lapply(attentionsplit_modellist2, function(x) plm(x, data = HighA_nobig_noga_df, model = 'within', effect = 'individual'))
HighA_nobig_noga_modelsummary <- lapply(HighA_nobig_no_ga_results, function(x) summary(x, vcov = vcovSCC))
HighA_nobgi_noga_modelsummary 


#### 7. Comparison with other data sources   ####
#### 7.1 Analyze data  ####

# Google trends
gtrend_data %>%ggplot(aes(x = identity, y = est_hits)) + geom_boxplot()

# Mediacloud
mcloud_long %>% ggplot(aes(x = identity, y = mc_count)) +geom_boxplot()

# Twitter 
bb_data %>% ggplot(aes(x = identity, y = tw_count)) + geom_boxplot() # count
bb_data %>% filter(tw_count >= 5) %>% ggplot(aes(x = identity, y = tw_sent)) + geom_boxplot() # sent
bb_data %>% ggplot(aes(x = identity, y = tw_pos)) + geom_boxplot() # pos
bb_data %>% ggplot(aes(x = identity, y = tw_neg)) + geom_boxplot() # neg

# Bloomberg news
bb_data %>% ggplot(aes(x = identity, y = bbnews_count)) + geom_boxplot() # count
bb_data %>% filter(tw_count >= 5) %>% ggplot(aes(x = identity, y = bbnews_sent)) + geom_boxplot() # sent
bb_data %>% ggplot(aes(x = identity, y = bbnews_pos)) + geom_boxplot() # pos
bb_data %>% ggplot(aes(x = identity, y = bbnews_neg)) + geom_boxplot() # neg

#### 7.1 Adjust data  ####

# Mediacloud
mcloud_adj <- mcloud_long %>%
  group_by(identity) %>%
  mutate(log_mc_count    = log(mc_count + 1),
         logscl_mc_count = (log_mc_count - min(log_mc_count)) / (max(log_mc_count) - min(log_mc_count)),
         scl_mc_count    = (mc_count - min(mc_count)) / (max(mc_count) - min(mc_count))
         )

# Google trend
gtrend_adj <- gtrend_data %>%
  rename(Date = date,
         gt_index = est_hits) %>%
  group_by(identity) %>%
  mutate(log_gt_index    = log(gt_index + 1),
         logscl_gt_index = (log_gt_index - min(log_gt_index)) / (max(log_gt_index) - min(log_gt_index)),
         scl_gt_index    = (gt_index - min(gt_index)) / (max(gt_index) - min(gt_index))
          )

# Twitter and Bloomberg news
bb_adj <- bb_data %>%
  group_by(identity) %>%
  mutate(log_tw_count        = log(1 + tw_count),
         tw_countsent        = (1 + tw_pos) / (1 + tw_neg),
         tw_meansent         = (tw_pos - tw_neg) / (tw_pos + tw_neg),
         log_tw_countsent    = log(tw_countsent),
         scl_tw_count        = (tw_count - min(tw_count)) / (max(tw_count) - min(tw_count)),
         logscl_tw_count     = (log_tw_count - min(log_tw_count)) / (max(log_tw_count) - min(log_tw_count)),
         logscl_tw_countsent = (log_tw_countsent - min(log_tw_countsent)) / (max(log_tw_countsent) - min(log_tw_countsent)),
         scl_tw_countsent    = (tw_countsent - min(tw_countsent)) / (max(tw_countsent) - min(tw_countsent)),
         log_bb_count        = log(1 + bbnews_count),
         scl_bb_count        = (bbnews_count - min(bbnews_count)) / (max(bbnews_count) - min(bbnews_count)),
         logscl_bb_count     = (log_bb_count - min(log_bb_count)) / (max(log_bb_count) - min(log_bb_count)),
         bb_countsent        = (1 + bbnews_pos) / (1 + bbnews_neg),
         bb_meansent         = (bbnews_pos - bbnews_neg) / (bbnews_pos + bbnews_neg),
         log_bb_countsent    = log(bb_countsent),
         logscl_bb_countsent = (log_bb_countsent - min(log_bb_countsent)) / (max(log_bb_countsent) - min(log_bb_countsent)),
         scl_bb_countsent    = (bb_countsent - min(bb_countsent)) / (max(bb_countsent) - min(bb_countsent))
         )

# Merge data frames
media_list <- list(gtrend_adj, mcloud_adj, bb_adj, wsb_return)
media_comb <- Reduce(function(x, y) merge(x, y, by = c('Date', 'identity')), media_list)



#### 7.3 Visualize data  ####
#### 7.3.1 Sentiment boxplots  ####

# Google trends
gtrend_adj %>%ggplot(aes(x = identity, y = logscl_gt_index)) + geom_boxplot() # Log index

# MediaCloud
mcloud_adj %>% ggplot(aes(x = identity, y = log_mc_count)) + geom_boxplot() # Log count

# Twitter
bb_adj %>% ggplot(aes(x = identity, y = tw_countsent)) + geom_boxplot() # countsent
bb_adj %>% ggplot(aes(x = identity, y = log_tw_countsent)) + geom_boxplot() # log countsent

# Bloomberg
bb_adj %>% ggplot(aes(x = identity, y = bb_countsent)) + geom_boxplot() # countsent
bb_adj %>% ggplot(aes(x = identity, y = log_bb_countsent)) + geom_boxplot() # log countsent

#### 7.3.2 Correlation between sources ####

cor_media <- media_comb %>%
  group_by(identity) %>%
  summarize(n = n(),
            mc_gt  = cor(scl_mc_count, scl_gt_index, method = 'pearson', use = "complete.obs"),
            mc_wsb = cor(scl_mc_count, scl_nsubmissions, method = 'pearson', use = "complete.obs"),
            mc_tw  = cor(scl_mc_count, scl_tw_count, method = 'pearson', use = "complete.obs"),
            mc_bb  = cor(scl_mc_count, scl_bb_count, method = 'pearson', use = "complete.obs"),
            gt_wsb = cor(scl_gt_index, scl_nsubmissions, method = 'pearson', use = "complete.obs"),
            gt_tw  = cor(scl_gt_index, scl_tw_count, method = 'pearson', use = "complete.obs"),
            gt_bb  = cor(scl_gt_index, scl_bb_count, method = 'pearson', use = "complete.obs"),
            wsb_tw = cor(scl_nsubmissions, scl_tw_count, method = 'pearson', use = "complete.obs"),
            wsb_bb = cor(scl_nsubmissions, scl_bb_count, method = 'pearson', use = "complete.obs"),
            tw_bb  = cor(scl_tw_count, scl_bb_count, method = 'pearson', use = "complete.obs"),
  ) %>%
  arrange(desc(n))
summary(cor_media)
cor_media


#### 7.3.3 Correlation between sent scores ####

corsent_media <- media_comb %>%
  group_by(identity) %>%
  summarize(n = n(),
            wsb_tw_sent      = cor(vad_comp_mean_sub, tw_sent, method = 'pearson', use = "complete.obs"),
            wsb_bb_sent      = cor(vad_comp_mean_sub, bbnews_count, method = 'pearson', use = "complete.obs"),
            wsb_tw_cousent   = cor(scl_vad_comp_sub, scl_tw_countsent, method = 'pearson', use = "complete.obs"),
            wsb_bb_cousent   = cor(scl_vad_comp_sub, scl_bb_countsent, method = 'pearson', use = "complete.obs"),
            wsb_tw_lcousent  = cor(logscl_vad_comp_sub, logscl_tw_countsent, method = 'pearson', use = "complete.obs"),
            wsb_bb_lcousent  = cor(logscl_vad_comp_sub, logscl_bb_countsent, method = 'pearson', use = "complete.obs"),
            wsb_mc_cousent   = cor(scl_vad_comp_sub, scl_mc_count, method = 'pearson', use = "complete.obs"),
            wsb_gt_cousent   = cor(scl_vad_comp_sub, scl_gt_index, method = 'pearson', use = "complete.obs"),
            wsb_mc_lcousent  = cor(logscl_vad_comp_sub, logscl_mc_count, method = 'pearson', use = "complete.obs"),
            wsb_gt_lcousent  = cor(logscl_vad_comp_sub, logscl_gt_index, method = 'pearson', use = "complete.obs"),
            mc_gt  = cor(logscl_mc_count, logscl_gt_index, method = 'pearson', use = "complete.obs"),
            mc_tw  = cor(logscl_mc_count, logscl_tw_countsent, method = 'pearson', use = "complete.obs"),
            mc_bb  = cor(logscl_mc_count, logscl_bb_countsent, method = 'pearson', use = "complete.obs"),
            gt_tw  = cor(logscl_gt_index, logscl_tw_countsent, method = 'pearson', use = "complete.obs"),
            gt_bb  = cor(logscl_gt_index, logscl_bb_countsent, method = 'pearson', use = "complete.obs")
            ) %>%
  arrange(desc(n))
summary(corsent_media)
corsent_media

#### 7.4 Event studies  ####

p <- media_comb %>%
  filter(identity == 'GME') %>%
  filter(Date >= '2021-01-10' & Date < '2021-02-10') %>%
  mutate(logscl_tw_count     = (log_tw_count - min(log_tw_count)) / (max(log_tw_count) - min(log_tw_count)),
         logscl_bb_count     = (log_bb_count - min(log_bb_count)) / (max(log_bb_count) - min(log_bb_count)),
         logscl_nsubmissions = (log_nsubmissions - min(log_nsubmissions, na.rm = T)) / (max(log_nsubmissions, na.rm = T) - min(log_nsubmissions, na.rm = T)),
         logscl_ncomments    = (log_ncomments - min(log_ncomments, na.rm = T)) / (max(log_ncomments, na.rm = T) - min(log_ncomments, na.rm = T)),
         logscl_ncomments_sub = (log_ncomments_sub - min(log_ncomments_sub, na.rm = T)) / (max(log_ncomments_sub, na.rm = T) - min(log_ncomments_sub, na.rm = T)),
         logscl_mc_count = (log_mc_count - min(log_mc_count)) / (max(log_mc_count) - min(log_mc_count)),
         logscl_gt_index = (log_gt_index - min(log_gt_index)) / (max(log_gt_index) - min(log_gt_index))
  ) %>%
  ggplot(aes(x = Date, group = identity)) +
    geom_line(aes(y = logscl_nsubmissions), color = 'deeppink') +
    geom_line(aes(y = logscl_ncomments_sub), color = 'grey') +
    geom_line(aes(y = logscl_tw_count), color = 'deepskyblue') +
    geom_line(aes(y = logscl_gt_index), color = 'darkseagreen') +
    geom_line(aes(y = logscl_mc_count), color = 'black') +
    geom_line(aes(y = logscl_bb_count), color = 'darkorange') +
  scale_x_date(date_breaks =  "3 day", date_labels =  "%b %d")

ggplotly(p)


pp <- media_comb %>%
  filter(identity == 'CLNE') %>%
  filter(Date >= '2021-05-30' & Date < '2021-07-01') %>%
  mutate(scl_tw_count        = (tw_count - min(tw_count)) / (max(tw_count) - min(tw_count)),
          scl_bb_count        = (bbnews_count - min(bbnews_count)) / (max(bbnews_count) - min(bbnews_count)),
          scl_nsubmissions    = (nsubmissions - min(nsubmissions, na.rm = T)) / (max(nsubmissions, na.rm = T) - min(nsubmissions, na.rm = T)),
          scl_ncomments       = (ncomments - min(ncomments, na.rm = T)) / (max(ncomments, na.rm = T) - min(ncomments, na.rm = T)),
          scl_ncomments_sub   = (ncomments_sub - min(ncomments_sub, na.rm = T)) / (max(ncomments_sub, na.rm = T) - min(ncomments_sub, na.rm = T)),
          scl_mc_count    = (mc_count - min(mc_count)) / (max(mc_count) - min(mc_count)),
          scl_gt_index    = (gt_index - min(gt_index)) / (max(gt_index) - min(gt_index))
  ) %>%
  ggplot(aes(x = Date, group = identity)) +
  geom_line(aes(y = scl_nsubmissions), color = 'deeppink') +
  geom_line(aes(y = scl_tw_count), color = 'deepskyblue') +
  geom_line(aes(y = scl_gt_index), color = 'darkseagreen') +
  geom_line(aes(y = scl_mc_count), color = 'black') +
  geom_line(aes(y = scl_bb_count), color = 'darkorange') +
  scale_x_date(date_breaks =  "3 day", date_labels =  "%b %d")

ggplotly(pp)

#### 7.5 Regression analysis####
#### 7.5.1 Unit root ####

# Create data frame
media_unitr <- media_comb %>% 
                  na_replace(0) %>%
                  group_by(identity) %>%
                  mutate(Date = as.Date(Date))
                  ungroup() %>% 
                  filter(nsubmissions >= 5 & ncomments_sub >= 5) %>%
                  select(c('identity', 'Date', 
                           'scl_gt_index', 'logscl_gt_index',
                           'scl_mc_count', 'logscl_mc_count',
                           'logscl_tw_countsent', 'logscl_tw_count', 'tw_meansent',
                           'logscl_bb_countsent', 'logscl_bb_count', 'bb_meansent'
                           ))

# Create column list
colname <- colnames(media_unitr)
colname <- colname[-c(1,2)] # Drop identity, Date and RF
Vars <- as.list(colname)

# Prepare different models for each variable
urModelsList <- lapply(paste(Vars, '~1'), as.formula)
ipsModelsResults <- lapply(urModelsList, 
                           function(x) purtest(x, data = media_unitr, lags = 'AIC',
                                               index = c('identity', 'Date'), 
                                               test = 'ips', pmax = 5))  

PMModelsResults <- lapply(urModelsList, 
                          function(x) purtest(x, data = media_unitr, lags = 'AIC',
                                              index = c('identity', 'Date'), 
                                              test = 'Pm', pmax = 5))  

InvModelsResults <- lapply(urModelsList, 
                           function(x) purtest(x, data = media_unitr, lags = 'AIC',
                                               index = c('identity', 'Date'), 
                                               test = 'invnormal', pmax = 5))  




#### 7.5.2 Prep data ####

# Create lagged variables
colname_media  <- colnames(media_comb)
colname_media  <- colname_media [-c(1,2)]
colname_media 

media_comb_lag <- media_comb %>%
  group_by(identity) %>%
  tk_augment_leads(colname_media, .lag = 1:3)

# Filter data
media_return <- media_comb_lag %>%
  group_by(identity) %>%
  filter(nsubmissions >= 5 & ncomments_sub >= 5)

# Create panel frame
pmedia_return <- pdata.frame(media_return, index = c('identity', 'Date'))

#### 7.5.3 Check Autocorrelation ####
media_ac <- media_return %>%
  summarize(n = n(),
            gt1          = cor(scl_gt_index, scl_gt_index_lag1, method = 'pearson', use = "complete.obs"),
            gt2          = cor(scl_gt_index, scl_gt_index_lag2, method = 'pearson', use = "complete.obs"),
            gt3          = cor(scl_gt_index, scl_gt_index_lag3, method = 'pearson', use = "complete.obs"),
            gtlog1       = cor(logscl_gt_index, logscl_gt_index_lag1, method = 'pearson', use = "complete.obs"),
            gtlog2       = cor(logscl_gt_index, logscl_gt_index_lag2, method = 'pearson', use = "complete.obs"),
            gtlog3       = cor(logscl_gt_index, logscl_gt_index_lag3, method = 'pearson', use = "complete.obs"),
            mc1          = cor(scl_mc_count, scl_mc_count_lag1, method = 'pearson', use = "complete.obs"),
            mc2          = cor(scl_mc_count, scl_mc_count_lag2, method = 'pearson', use = "complete.obs"),
            mc3          = cor(scl_mc_count, scl_mc_count_lag3, method = 'pearson', use = "complete.obs"),
            mclog1       = cor(logscl_mc_count, logscl_mc_count_lag1, method = 'pearson', use = "complete.obs"),
            mclog2       = cor(logscl_mc_count, logscl_mc_count_lag2, method = 'pearson', use = "complete.obs"),
            mclog3       = cor(logscl_mc_count, logscl_mc_count_lag3, method = 'pearson', use = "complete.obs"),
            tw_sent1     = cor(tw_sent, tw_sent_lag1, method = 'pearson', use = "complete.obs"),
            tw_sent2     = cor(tw_sent, tw_sent_lag2, method = 'pearson', use = "complete.obs"),
            tw_sent3     = cor(tw_sent, tw_sent_lag3, method = 'pearson', use = "complete.obs"),
            tw_lsentcnt1 = cor(logscl_tw_countsent, logscl_tw_countsent_lag1, method = 'pearson', use = "complete.obs"),
            tw_lsentcnt2 = cor(logscl_tw_countsent, logscl_tw_countsent_lag2, method = 'pearson', use = "complete.obs"),
            tw_lsentcnt3 = cor(logscl_tw_countsent, logscl_tw_countsent_lag3, method = 'pearson', use = "complete.obs"),
            bb_sent1     = cor(bbnews_sent, bbnews_sent_lag1, method = 'pearson', use = "complete.obs"),
            bb_sent2     = cor(bbnews_sent, bbnews_sent_lag2, method = 'pearson', use = "complete.obs"),
            bb_sent3     = cor(bbnews_sent, bbnews_sent_lag3, method = 'pearson', use = "complete.obs"),
            bb_lsentcnt1 = cor(logscl_bb_countsent, logscl_bb_countsent_lag1, method = 'pearson', use = "complete.obs"),
            bb_lsentcnt2 = cor(logscl_bb_countsent, logscl_bb_countsent_lag2, method = 'pearson', use = "complete.obs"),
            bb_lsentcnt3 = cor(logscl_bb_countsent, logscl_bb_countsent_lag3, method = 'pearson', use = "complete.obs")
  ) %>%
  as.data.frame()
summary(media_ac)


#### 7.5.4 General Regression ####

# Create model list (combined)
media_models <- as.list(c(
  'Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_vad_comp_sub+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_vad_comp_sub_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_vad_comp_sub+logscl_vad_comp_sub_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_gt_index + Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_gt_index_lag1 + Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_gt_index + logscl_gt_index_lag1 + Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_gt_index + logscl_gt_index_lag1 + logscl_gt_index_lag2 + Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'scl_gt_index + Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'scl_gt_index_lag1 + Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'scl_gt_index + scl_gt_index_lag1 + Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'scl_gt_index + scl_gt_index_lag1 + scl_gt_index_lag2 + Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_mc_count + Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_mc_count_lag1 + Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_mc_count + logscl_mc_count_lag1 + Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_mc_count + logscl_mc_count_lag1 + logscl_mc_count_lag2 + Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'scl_mc_count + Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'scl_mc_count_lag1 + Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'scl_mc_count + scl_mc_count_lag1 + Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'scl_mc_count + scl_mc_count_lag1 + scl_mc_count_lag2 + Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_tw_countsent + Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_tw_countsent_lag1 + Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_tw_countsent + logscl_tw_countsent_lag1 + Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_tw_countsent + logscl_tw_countsent_lag1 + logscl_tw_countsent_lag2 + Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_tw_count + Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_tw_count_lag1 + Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_tw_count + logscl_tw_count_lag1 + Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_tw_count + logscl_tw_count_lag1 + logscl_tw_count_lag2 + Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'tw_meansent + Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'tw_meansent_lag1 + Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'tw_meansent + tw_meansent_lag1 + Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'tw_meansent + tw_meansent_lag1 + tw_meansent_lag2 + Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_bb_countsent + Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_bb_countsent_lag1 + Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_bb_countsent + logscl_bb_countsent_lag1 + Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_bb_countsent + logscl_bb_countsent_lag1 + logscl_bb_countsent_lag2 +Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_bb_count + Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_bb_count_lag1 + Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_bb_count + logscl_bb_count_lag1 + Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_bb_count + logscl_bb_count_lag1 + logscl_bb_count_lag2 +Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'bb_meansent + Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'bb_meansent_lag1 + Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'bb_meansent + bb_meansent_lag1 + Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'bb_meansent + bb_meansent_lag1 + bb_meansent_lag2 +Log_return_lag1+Rus_log_return+Rus_log_return_lag1'
))

# Create model list (seperate)
media_models2 <- as.list(c(
  'logscl_vad_comp_sub+logscl_vad_comp_sub_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_vad_comp_sub+logscl_vad_comp_sub_lag1+
   logscl_gt_index + logscl_gt_index_lag1 + Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_vad_comp_sub + logscl_vad_comp_sub_lag1+
   logscl_mc_count + logscl_mc_count_lag1 + Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_vad_comp_sub + logscl_vad_comp_sub_lag1+
   logscl_tw_countsent + Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_vad_comp_sub + logscl_vad_comp_sub_lag1+
   logscl_bb_countsent + Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_vad_comp_sub+logscl_vad_comp_sub_lag1+
   logscl_gt_index + logscl_gt_index_lag1 + 
   logscl_mc_count + logscl_mc_count_lag1 + Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_vad_comp_sub+logscl_vad_comp_sub_lag1+
   logscl_gt_index + logscl_gt_index_lag1 + 
   logscl_mc_count + logscl_mc_count_lag1 + 
   logscl_tw_countsent + Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_vad_comp_sub+logscl_vad_comp_sub_lag1+
   logscl_gt_index + logscl_gt_index_lag1 + 
   logscl_mc_count + logscl_mc_count_lag1 + 
   logscl_bb_countsent + Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_vad_comp_sub+logscl_vad_comp_sub_lag1+
   logscl_gt_index + logscl_gt_index_lag1 + 
   logscl_mc_count + logscl_mc_count_lag1 + 
   logscl_tw_countsent + 
   logscl_bb_countsent + Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_vad_comp_sub+logscl_vad_comp_sub_lag1+
   logscl_mc_count + logscl_mc_count_lag1 + 
   logscl_tw_countsent + Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_vad_comp_sub+logscl_vad_comp_sub_lag1+
   logscl_mc_count + logscl_mc_count_lag1 + 
   logscl_bb_countsent + Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_vad_comp_sub+logscl_vad_comp_sub_lag1+
   logscl_gt_index + logscl_gt_index_lag1 + 
   logscl_tw_countsent + Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_vad_comp_sub+logscl_vad_comp_sub_lag1+
   logscl_gt_index + logscl_gt_index_lag1 + 
   logscl_bb_countsent + Log_return_lag1+Rus_log_return+Rus_log_return_lag1'
))

# Final list
media_modellist <- lapply(paste('Log_return ~', media_models), as.formula)

# Run regressions
media_modelresults <- lapply(media_modellist, function(x) plm(x, data = pmedia_return, model = 'within', effect = 'individual'))
media_modelsummary <- lapply(media_modelresults, function(x) summary(x, vcov = vcovSCC))


#### 7.5.5 General Regression - Attention split ####

# Create model list
media_models3 <- as.list(c(
  'Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_vad_comp_sub+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_vad_comp_sub+logscl_vad_comp_sub_lag1+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_vad_comp_sub+logscl_vad_comp_sub_lag1+logscl_vad_comp_sub_lag2+Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_gt_index + Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_gt_index + logscl_gt_index_lag1 + Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_gt_index + logscl_gt_index_lag1 + logscl_gt_index_lag2 + Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_mc_count + Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_mc_count_lag1 + Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_mc_count + logscl_mc_count_lag1 + Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_mc_count + logscl_mc_count_lag1 + logscl_mc_count_lag2 + Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_tw_countsent + Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_tw_countsent + logscl_tw_countsent_lag1 + Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_tw_countsent + logscl_tw_countsent_lag1 + logscl_tw_countsent_lag2 + Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_bb_countsent + Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_bb_countsent + logscl_bb_countsent_lag1 + Log_return_lag1+Rus_log_return+Rus_log_return_lag1',
  'logscl_bb_countsent + logscl_bb_countsent_lag1 + logscl_bb_countsent_lag2 +Log_return_lag1+Rus_log_return+Rus_log_return_lag1'
))

# Final model list
attention_media_modellist <- lapply(paste('Log_return ~', media_models3), as.formula)

# Splitting the data set
attention_media_dfs <- list()
ticker_quantiles <- c(1,2,3,4)

for (i in 1:length(ticker_quantiles)) {
  attention_media_dfs[[i]] <- media_return  %>% 
    filter(!(identity %in% subset(market_cap, BEG_quartile == 4)$Ticker)) %>%
    filter(identity %in% subset(ticker_attention_adj2, sub_quartile == i)$identity) %>% 
    pdata.frame(index = c('identity', 'Date'))
}

# Run regression for each subset
attention_media_results <- list()
attention_media_summary <- list()
for (quant in attention_media_dfs){
  results <- lapply(attention_media_modellist, function(x) plm(x, data = quant, model = 'within', effect = 'individual'))
  modelsummary <- lapply(results, function(x) summary(x, vcovSCC))
  attention_media_results <- c(attention_media_results, list(results))
  attention_media_summary <- c(attention_media_summary, list(modelsummary))
}
names(attention_media_results) <- c('Q1', 'Q2', 'Q3', 'Q4')
names(attention_media_summary) <- c('Q1', 'Q2', 'Q3', 'Q4')


###########################################################################
######################### Outputs for thesis ##############################
###########################################################################
#### 1. GME volume and stock price ####
gme_data <- wsb_reg %>%
  filter(identity == 'GME') %>%
  select(Date, Adj.Close, High, Volume) %>%
  filter(Date >= '2020-01-01') %>%
  mutate(Date = as.Date(Date))

VolumeColor <- '#ED7D31'
priceColor <- '#1E5FA5'
coeff = 0.5

gme_data %>%
  mutate(Volume2 = Volume/1000000) %>%
  ggplot(aes(x=Date))+
    geom_bar(aes(y = Volume2/coeff), stat = "identity", size = 0.03, 
             fill = VolumeColor, color = 'white', alpha = 0.5) +
    geom_line(aes(y = Adj.Close), size=0.5, color=priceColor) +
    scale_y_continuous(name = "Closing Price ($)", 
                       sec.axis = sec_axis(~.*coeff, name="Trading Volume (M shares)"),
                       expand = c(0,0), limits = c(0, 400)) +
    scale_x_bd(business.dates= gme_data$Date, max.major.breaks = 10,
               labels=date_format("%b-%y"), expand = c(0,0)) +
    theme(panel.background = element_rect(fill = "white",
                                        colour = "grey60",
                                        size = 0.3, linetype = "solid"),
          panel.grid.major = element_line(size = 0.2, color = "grey90"),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.line   = element_line(size = 0.15),
          axis.title.y = element_text(color = priceColor, size=15, vjust = 2),
          axis.title.y.right = element_text(color = VolumeColor, size=15, vjust = 2),
          axis.title.x = element_blank())
?scale_x_bd
ggplotly(p)



#### 2. Co-Movements of stocks ####

stock_list1 = c('GME', 'AMC', 'BB', 'NOK')

# Vader comp
wsb_reg %>%
  filter(identity %in% stock_list1) %>%
  mutate(Date = as.Date(Date)) %>%
  filter(Date >= '2021-01-15' & Date < '2021-02-15') %>%
  ggplot(aes(x = Date, y = Log_return, group = identity)) +
  geom_line(aes(color = identity)) +
  scale_y_continuous(name = "Log-Return") +
  scale_x_date(labels=date_format("%b-%d"), date_breaks = '4 day') +
  theme(panel.background = element_rect(fill = "white",
                                      colour = "grey60",
                                      size = 0.3, linetype = "solid"),
      panel.grid.major = element_line(size = 0.2, color = "grey90"),
      legend.title = element_blank(),
      legend.position = c(0.92,0.88),
      legend.key=element_blank(),
      legend.text = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.line   = element_line(size = 0.15),
      axis.title.y = element_text(size=15, vjust = 2),
      axis.title.x = element_blank()
      )


#### 3. Multicollinearity ####

#### 4  Corr for sentiment comments and submissions ####

cor_vars <- c('logscl_vad_comp_sub','scl_vad_comp_sub', 'vad_comp_mean_sub',
              'vad_sent_sub','logscl_vad_count_sent_sub', 
              'logscl_lm_pol_sub','scl_lm_pol_sub', 'lm_pol_mean_sub',
              'lm_sent_sub','logscl_lm_count_sent_sub',
              'logscl_vad_comp_com','scl_vad_comp_com', 'vad_comp_mean_com',
              'vad_sent_com','logscl_vad_count_sent_com', 
              'logscl_lm_pol_com','scl_lm_pol_com', 'lm_pol_mean_com',
              'lm_sent_com','logscl_lm_count_sent_com'
              )

cormatrix <- wsb_return %>%
  filter(nsubmissions >= 5 & ncomments_sub >=5, ncomments >=25) %>%
  ungroup() %>%
  select(cor_vars) %>%
  cor(use = 'pairwise.complete.obs') %>%
  round(2)

cormatrix

corrplot(cormatrix, method="color",   
         addCoef.col = "grey20",
         type="upper",
         tl.col="black", tl.srt=45,
         number.cex = 0.7)

#### 5. Descriptive statistics 1 ####
sub_data <- read.csv('sub_final_sentiment.csv', header = T)

str(sub_data)
sub_descrip <- sub_data %>%
   group_by(identity) %>%
   summarise(n = n(),
             Vad_comp    = mean(compound),
             LM_pol      = mean(lm_polarity),
             Vad_posneg  = sum(vad_positive)/sum(vad_negative),
             LM_posneg   = sum(lm_positive)/sum(lm_negative),
             Vad_neu     = mean(vad_neutral),
             LM_neu      = (n() - sum(lm_positive) - sum(lm_negative))/n()
   )

write_clip(sub_descrip)

com_data <- read.csv('com_final_sentiment_desc.csv', header = T)

com_descrip <- com_data %>%
  group_by(identity) %>%
  summarise(n = n(),
            Vad_comp    = mean(compound),
            LM_pol      = mean(lm_polarity),
            Vad_posneg  = sum(vad_positive)/sum(vad_negative),
            LM_posneg   = sum(lm_positive)/sum(lm_negative),
            Vad_neu     = mean(vad_neutral),
            LM_neu      = (n() - sum(lm_positive) - sum(lm_negative))/n()
  )

write_clip(com_descrip)


#### 6. Descriptive aggregated ####

vars_descr_sub <- c('Log_return','Rus_log_return', 'Volume', 
                    'vad_sent_sub', 'log_vad_count_sent_sub','vad_comp_mean_sub', 'vad_comp_sub', 'log_vad_comp_sub',
                    'lm_sent_sub', 'log_lm_count_sent_sub','lm_pol_mean_sub', 'lm_pol_sub', 'log_lm_pol_sub'
                    )

vars_descr_com <- c('vad_sent_com', 'log_vad_count_sent_com','vad_comp_mean_com', 'vad_comp_com', 'log_vad_comp_com',
                    'lm_sent_com', 'log_lm_count_sent_com','lm_pol_mean_com', 'lm_pol_com', 'log_lm_pol_com'
                    )



descr_data_sub_5 <- wsb_return %>% 
                    ungroup() %>% 
                    filter(nsubmissions >= 5 & ncomments_sub >= 5) %>%
                    mutate(Volume = Volume / 1000000) %>%
                    select(vars_descr_sub)

descr_data_sub <-   wsb_return %>% 
                    ungroup() %>% 
                    filter(nsubmissions >= 1 & ncomments_sub >= 1) %>%
                    mutate(Volume = Volume / 1000000) %>%
                    select(vars_descr_sub)

descr_data_com_25 <- wsb_return %>% 
                    ungroup() %>% 
                    filter(ncomments >=25) %>%
                    mutate(Volume = Volume / 1000000) %>%
                    select(vars_descr_com)

descr_data_com <-   wsb_return %>% 
                    ungroup() %>% 
                    filter(ncomments >=1) %>%
                    mutate(Volume = Volume / 1000000) %>%
                    select(vars_descr_com)

Stable_main <- st(descr_data_sub_5,
                   summ = c("notNA(x)", "mean(x)", "sd(x)", "min(x)","pctile(x)[25]",
                          "median(x)","pctile(x)[75]", "max(x)"),
                   summ.names =  c("N", "Mean", "SD", "Min", "1st Q",
                                  "Median","3rd Q", "Max"),
                   digits =2)

Stable_sub1_app <- st(descr_data_sub,
                    summ = c("notNA(x)", "mean(x)","sd(x)", "min(x)","pctile(x)[25]",
                             "median(x)","pctile(x)[75]", "max(x)"),
                    summ.names =  c("N", "Mean", "SD", "Min", "1st Q",
                                    "Median","3rd Q", "Max"),
                    digits =2)

Stable_com25_app <- st(descr_data_com_25,
                  summ = c("notNA(x)", "mean(x)", "sd(x)", "min(x)","pctile(x)[25]",
                           "median(x)","pctile(x)[75]", "max(x)"),
                  summ.names =  c("N", "Mean", "SD", "Min", "1st Q",
                                  "Median","3rd Q", "Max"),
                  digits =2)

Stable_com1_app <- st(descr_data_com,
                     summ = c("notNA(x)", "mean(x)", "sd(x)", "min(x)","pctile(x)[25]",
                              "median(x)","pctile(x)[75]", "max(x)"),
                     summ.names =  c("N", "Mean", "SD", "Min", "1st Q",
                                     "Median","3rd Q", "Max"),
                     digits =2)

#### 7. Descriptive aggregated by stock ####

x <- wsb_reg %>%
  filter(nsubmissions >= 1 & ncomments_sub >= 1)%>%
  ungroup() %>%
  group_by(identity) %>%
  summarise(N      = n())

aggstock_descr_sub <- wsb_return %>%
                    filter(nsubmissions >= 5 & ncomments_sub >= 5) %>%
                    ungroup() %>%
                    group_by(identity) %>%
                    summarise(N      = n(),
                              Mean   = mean(nsubmissions),
                              sd     = sd(nsubmissions),
                              Min    = min(nsubmissions),
                              Median = median(nsubmissions),
                              Max    = max(nsubmissions),
                    )
  
write_clip(aggstock_descr_sub)

aggstock_descr_com <- wsb_return %>%
  filter(ncomments >= 25) %>%
  ungroup() %>%
  group_by(identity) %>%
  summarise(N      = n(),
            Mean   = mean(ncomments),
            sd     = sd(ncomments),
            Min    = min(ncomments),
            Median = median(ncomments),
            Max    = max(ncomments),
  )

write_clip(aggstock_descr_com)


#### 8. Descriptive Media compared ####

media_list2 <- list(mcloud_adj, bb_adj)
media_comb2 <- Reduce(function(x, y) merge(x, y, by = c('Date', 'identity')), media_list2)

media_comb2 <- media_comb2 %>%
  na_replace(0) %>%
  group_by(identity) %>%
  mutate(Date = as.Date(Date))

media_descriptive <- media_comb2 %>%
  group_by(identity) %>%
  summarise(n = n(),
            MC = sum(mc_count),
            TW = sum(tw_count),
            BB = sum(bbnews_count)
  )

write_clip(media_descriptive)


#### 9. Correlation Media count ####
cor_vars <- c('scl_nsubmissions',  'scl_gt_index', 'scl_mc_count',
              'scl_tw_count',  'scl_bb_count'

)

cormatrix <- media_return %>%
  ungroup() %>%
  select(cor_vars) %>%
  cor(use = 'pairwise.complete.obs', method = "pearson") %>%
  round(2)

cormatrix

help(cor)
  
corrplot(cormatrix, method="color",   
         addCoef.col = "grey20",
         type="lower",
         tl.col="black", tl.srt=45,
         number.cex = 1.5)

#### 10. Correlation Media sent ####
cor_vars <- c('logscl_vad_comp_sub','logscl_gt_index', 'logscl_mc_count',
              'logscl_tw_countsent', 'logscl_bb_countsent'
              
)


cormatrix <- media_return %>%
  ungroup() %>%
  select(cor_vars) %>%
  cor(use = 'pairwise.complete.obs') %>%
  round(2)

cormatrix

corrplot(cormatrix, method="color",   
         addCoef.col = "grey20",
         type="upper",
         tl.col="black", tl.srt=45,
         number.cex = 0.7)

#### 11. Correlation Sub and Com ####
cor_vars <- c('logscl_vad_comp_sub','vad_comp_mean_sub', 'logscl_vad_count_sent_sub', 'vad_sent_sub',
              'logscl_vad_comp_com','vad_comp_mean_com', 'logscl_vad_count_sent_com', 'vad_sent_com',
              'logscl_lm_pol_sub','lm_pol_mean_sub', 'logscl_lm_count_sent_sub', 'lm_sent_sub',
              'logscl_lm_pol_com','lm_pol_mean_com', 'logscl_lm_count_sent_com', 'lm_sent_com'
)

cormatrix <- wsb_return %>%
  ungroup() %>%
  filter(nsubmissions >= 5 & ncomments >= 25) %>%
  select(cor_vars) %>%
  cor(use = 'pairwise.complete.obs') %>%
  round(2)

cormatrix

corrplot(cormatrix, method="color",   
         addCoef.col = "grey20",
         type="lower",
         tl.col="black", tl.srt=45,
         number.cex = 0.7)
