---
title: "Accessing Glassnode's API for Cryptocurrency Metadata"
author: "Chad Dulle"
date: "2023-07-26"
output: html_document
---
The primary purpose of this file is to preview some of the data available via Glassnode's API. Although this requires a paid subscription, Glassnode's API offer the most expansive set of endpoints that I have seen accross all cryptocurrency API services that I have interacted with. Let's take a look at what they have to offer

<small>
please note that this code will not run on it's own. You will need your own Glassnode API key. However, this will explore the entire suite of endpoints and compile summary stats which can be useful getting to know the data
</small>


```r
##### load packages #####
  # general
  library(tidyverse)
  # interacting with APIs
  library(httr)
  library(jsonlite)
  # character manipulation
  library(glue)



###################################################################
##### Set up data fields that won't change with each endpoint #####
###################################################################

  ### This API key is from Micheal Jones account ###
  key <- "YOUR-API-KEY-HERE"
  
  ### Key syntax to be used in every GET call ###
    # * change this one  
    crypto_ticker <- 'btc'  
    syn1<- '?a='
    crypto <- glue(syn1,crypto_ticker)
    # add API key syntax 
    API_key <- glue('&api_key=',key)
    # all endpoints begin with this URL
    base.url <- 'https://api.glassnode.com/v1/metrics/'

  
###########################################
##### Run a Loop to Get Summary Stats #####
###########################################  
    
# Each endpoint is in a specific category (e.g. "addresses", "blockchain"). Because 
#   there are a different number of endpoints within each category, it makes sense
#   to run an independent loop for each category.

##### Addresses Category #####
  
  # category
  category <- 'addresses/'
  # vector of endpoints to loop through   
  endpoints <- c('active_count','count','loos_count','min_1_count',
                 'min_1_usd_count','min_10_count','min_10_usd_count',
                 'min_100_count','min_10k_count','min_1k_count',
                 'min_point_1_count','min_point_zero_1_count',
                 'new_non_zero_count','non_zero_count','profit_count',
                 'profit_relative','receiving_count','receiving_from_exchanges_count',
                 'sending_count','sending_to_exchanges_count','supply_balance_0001_001',
                 'supply_balance_001_01','supply_balance_1_10','supply_balance_10_100',
  'supply_balance_100_1k','supply_balance_10k_100k','supply_balance_1k_10k',
  'supply_balance_less_0001','supply_balance_more_100k','supply_distribution_relative')
    
    
    
  # blank list
  l1 <- list()
  
  for(n in 1:length(endpoints)){  
    # GET call 
    get <- GET(glue(base.url,category,endpoints[n],crypto,API_key))
    # prepare data
    dat <- fromJSON(rawToChar(get$content)) %>% try()
    
    ### view data ###
      # date and number of addresses 
      ifelse(get$status_code!=200,
                        l1[[n]] <- data.frame(date=NA,value=NA),
             l1[[n]] <-  data.frame(date=as.POSIXct(dat$t,origin='1970-01-01'),
                                      value=dat$v) 
      )
    try(l1[[n]]$measure <- endpoints[n]) 
  }
  
  # get rid of endpoints that are tier 3 (and thus empty)
  x <- NA
  for(n in 1:length(l1)){
    x[n] <- nrow(l1[[n]])
  }
  y <- which(x==1)
  l1[y] <- NULL
  
  # which endpoints are tier 3?
  unavail.ends <- data.frame(endpoint=endpoints,tier=NA)
  unavail.ends[y,]$tier <- "tier3"
  unavail.ends[which(is.na(unavail.ends$tier)),]$tier <- "tier1or2"
  unavail.ends <- unavail.ends %>% mutate(available=ifelse(tier=='tier3','no','yes'))
  
  
  # save data frame of summary stats for addresses category
  # number of metrics to get sum stats for 
  ends <- length(l1)
  sum.address <- data.frame(metric=rep(NA,ends),obs=rep(NA,ends),mean=rep(NA,ends),
                            sd=rep(NA,ends),min=rep(NA,ends),P5=rep(NA,ends),P25=rep(NA,ends),
                            median=rep(NA,ends),P75=rep(NA,ends),P95=rep(NA,ends))
  
  
  # loop over all endpoints  
  for(n in 1:length(l1)){
    sum.address$metric[n] <- l1[[n]]$measure[1]
    sum.address$obs[n] <- l1[[n]] %>% nrow()
    sum.address$mean[n] <- l1[[n]]$value %>% mean()
    sum.address$sd[n] <- l1[[n]]$value %>% sd()
    sum.address$min[n] <- l1[[n]]$value %>% summary() %>% .[1]
    sum.address$P5[n] <- l1[[n]]$value %>% quantile(.05)
    sum.address$P25[n] <- l1[[n]]$value %>% summary() %>% .[2]
    sum.address$median[n] <- l1[[n]]$value %>% summary() %>% .[3]
    sum.address$P75[n] <- l1[[n]]$value %>% summary() %>% .[5]
    sum.address$P95[n] <- l1[[n]]$value %>% quantile(.95)
    sum.address$max[n] <- l1[[n]]$value %>% summary() %>% .[6]
  }  
  
  sum.address <- as.data.frame(sum.address)
  
  # export summary stats
  setwd("D:/Research/Data Scraping/Crypto/Glassnode API/Data Export/Bitcoin/Addresses")
  write.csv(sum.address,"addresses_sum_stats.csv")
  
  # export endpoint tier categories
  write.csv(unavail.ends,"addresses_endpoint_tiers.csv")
  
  # create loop to export all plots
  for(n in 1:length(l1)){
    # create and export graphs
    tmp.graph <- l1[[n]] %>% 
      ggplot(.,aes(x=date,value)) +
      geom_line() +
      ggtitle(paste(l1[[n]] %>% select(measure) %>% unique()))
  # save plot
  ggsave(paste(l1[[n]] %>% select(measure) %>% unique(),'.pdf',sep=''))
  }  

  
  
##### Blockchain Category #####
  
  # category
  category <- 'blockchain/'
  # vector of endpoints to loop through   
  endpoints <- c('block_count','block_height','block_interval_mean',
    'block_interval_median','block_size_mean','block_size_sum','utxo_count',
    'utxo_created_count','utxo_created_value_mean','utxo_created_value_median',
    'utxo_created_value_sum','utxo_loss_count','utxo_profit_count',
    'utxo_profit_relative','utxo_spent_count','utxo_spent_value_mean',
    'utxo_spent_value_median','utxo_spent_value_sum')
  
  
  
  # blank list
  l1 <- list()
  
  for(n in 1:length(endpoints)){  
    # GET call 
    get <- GET(glue(base.url,category,endpoints[n],crypto,API_key))
    # prepare data
    dat <- fromJSON(rawToChar(get$content)) %>% try()
    
    ### view data ###
    # date and number of addresses 
    ifelse(get$status_code!=200,
           l1[[n]] <- data.frame(date=NA,value=NA),
           l1[[n]] <-  data.frame(date=as.POSIXct(dat$t,origin='1970-01-01'),
                                  value=dat$v) 
    )
    try(l1[[n]]$measure <- endpoints[n]) 
  }
  
  
  # get rid of endpoints that are tier 3 (and thus empty)
  x <- NA
  for(n in 1:length(l1)){
    x[n] <- nrow(l1[[n]])
  }
  y <- which(x==1)
  l1[y] <- NULL
  
  # which endpoints are tier 3?
  unavail.ends <- data.frame(endpoint=endpoints,tier=NA)
  unavail.ends[y,]$tier <- "tier3"
  unavail.ends[which(is.na(unavail.ends$tier)),]$tier <- "tier1or2"
  unavail.ends <- unavail.ends %>% mutate(available=ifelse(tier=='tier3','no','yes'))
  
  
  # save data frame of summary stats for addresses category
  # number of metrics to get sum stats for 
  ends <- length(l1)
  sum.blockchain <- data.frame(metric=rep(NA,ends),obs=rep(NA,ends),mean=rep(NA,ends),
                            sd=rep(NA,ends),min=rep(NA,ends),P5=rep(NA,ends),P25=rep(NA,ends),
                            median=rep(NA,ends),P75=rep(NA,ends),P95=rep(NA,ends))
  
  
  # loop over all endpoints  
  for(n in 1:length(l1)){
    sum.blockchain$metric[n] <- l1[[n]]$measure[1]
    sum.blockchain$obs[n] <- l1[[n]] %>% nrow()
    sum.blockchain$mean[n] <- l1[[n]]$value %>% mean()
    sum.blockchain$sd[n] <- l1[[n]]$value %>% sd()
    sum.blockchain$min[n] <- l1[[n]]$value %>% summary() %>% .[1]
    sum.blockchain$P5[n] <- l1[[n]]$value %>% quantile(.05)
    sum.blockchain$P25[n] <- l1[[n]]$value %>% summary() %>% .[2]
    sum.blockchain$median[n] <- l1[[n]]$value %>% summary() %>% .[3]
    sum.blockchain$P75[n] <- l1[[n]]$value %>% summary() %>% .[5]
    sum.blockchain$P95[n] <- l1[[n]]$value %>% quantile(.95)
    sum.blockchain$max[n] <- l1[[n]]$value %>% summary() %>% .[6]
  }  
  
  # export summary stats
  setwd("D:/Research/Data Scraping/Crypto/Glassnode API/Data Export/Bitcoin/Blockchain")
  write.csv(sum.blockchain,"blockchain_sum_stats.csv")
  
  # export endpoint tier categories
  write.csv(unavail.ends,"blockchain_endpoint_tiers.csv")
  
  # create loop to export all plots
  for(n in 1:length(l1)){
    # create and export graphs
    tmp.graph <- l1[[n]] %>% 
      ggplot(.,aes(x=date,value)) +
      geom_line() +
      ggtitle(paste(l1[[n]]$measure[1]))
    # save plot
    ggsave(paste(l1[[n]]$measure[1],'.pdf',sep=''))
  }  
  
  
  
##### DeFi Category #####
  
  # category
  category <- 'defi/'
  # vector of endpoints to loop through   
  endpoints <- c('total_value_locked')
  
  # blank list
  l1 <- list()
  
  for(n in 1:length(endpoints)){  
    # GET call 
    get <- GET(glue(base.url,category,endpoints[n],crypto,API_key))
    # prepare data
    dat <- fromJSON(rawToChar(get$content)) %>% try()
    
    ### view data ###
    # date and number of addresses 
    ifelse(get$status_code!=200,
           l1[[n]] <- data.frame(date=NA,value=NA),
           l1[[n]] <-  data.frame(date=as.POSIXct(dat$t,origin='1970-01-01'),
                                  value=dat$v) 
    )
    try(l1[[n]]$measure <- endpoints[n]) 
  }
  
  # only 1 endpoint, and it is tier 3... although it is available at Defi Llama
  
  
  
##### Derivatives Category #####
  
  # category
  category <- 'derivatives/'
  # vector of endpoints to loop through   
  endpoints <- c('futures_annualized_basis_3m',
  'futures_estimated_leverage_ratio','futures_funding_rate_perpetual',
  'futures_funding_rate_perpetual_all','futures_liquidated_volume_long_mean',
  'futures_liquidated_volume_long_relative','futures_liquidated_volume_long_sum',
  'futures_liquidated_volume_short_mean','futures_liquidated_volume_short_sum',
  'futures_open_interest_cash_margin_sum','futures_open_interest_crypto_margin_relative',
  'futures_open_interest_crypto_margin_sum','futures_open_interest_latest',
  'futures_open_interest_perpetual_sum','futures_open_interest_perpetual_sum_all',
  'futures_open_interest_sum','futures_open_interest_sum_all',
  'futures_term_structure','futures_term_structure_by_exchange',
  'futures_volume_daily_latest','futures_volume_daily_perpetual_sum',
  'futures_volume_daily_perpetual_sum_all','futures_volume_daily_sum',
  'futures_volume_daily_sum_all','options_25delta_skew_1_month',
  'options_25delta_skew_1_week','options_25delta_skew_3_months',
  'options_25delta_skew_6_months','options_25delta_skew_all',
  'options_atm_implied_volatility_1_month','options_atm_implied_volatility_1_week',
  'options_atm_implied_volatility_3_months','options_atm_implied_volatility_6_months',
  'options_atm_implied_volatility_all','options_implied_volatility_term_structure',
  'options_open_interest_distribution','options_open_interest_put_call_ratio',
  'options_open_interest_sum','options_volatility_smile','options_volume_daily_sum',
  'options_volume_put_call_ratio')
  
  # blank list
  derivs <- list()
  
  for(n in 1:length(endpoints)){  
    # GET call 
    get <- GET(glue(base.url,category,endpoints[n],crypto,API_key))
    # prepare data
    dat <- fromJSON(rawToChar(get$content)) %>% try()
    
    ### view data ###
    # date and number of addresses 
    ifelse(get$status_code!=200,
           derivs[[n]] <- data.frame(date=NA,value=NA),
           derivs[[n]] <-  dat 
    )
    try(derivs[[n]]$measure <- endpoints[n]) 
  }
  
  # *** some have more than just 2 columns. Also, for the endpoints that are available, 
  #     there are not many data points. don't export sum stats at this time

  
##### Distribution Category #####
  
  # category
  category <- 'distribution/'
  # vector of endpoints to loop through   
  endpoints <- c('balance_1pct_holders','balance_exchanges','balance_exchanges_all',
  'balance_exchanges_relative','balance_luna_foundation_guard','balance_miners_all',
  'balance_miners_change','balance_miners_sum','balance_mtgox_trustee','balance_otc_desks',
  'balance_wbtc','exchange_net_position_change','gini','herfindahl','supply_contracts')
  
  # blank list
  disty <- list()
  
  for(n in 1:length(endpoints)){  
    # GET call 
    get <- GET(glue(base.url,category,endpoints[n],crypto,API_key))
    # prepare data
    dat <- fromJSON(rawToChar(get$content)) %>% try()
    
    ### view data ###
    # date and number of addresses 
    ifelse(get$status_code!=200,
           disty[[n]] <- data.frame(date=NA,value=NA),
           disty[[n]] <-  dat
    )
    try(disty[[n]]$measure <- endpoints[n]) 
  }
  
  # not all endpoints have only 2 columns 
  
  # get rid of endpoints that are tier 3 
  x <- NA
  for(n in 1:length(disty)){
    x[n] <- nrow(disty[[n]])
  }
  y <- which(x==1)
  disty[y] <- NULL
  
  # get rid of endpoints that are more than 2 columns. Can summarise these later upon request
  x <- NA
  for(n in 1:length(disty)){
    x[n] <- ncol(disty[[n]])
  }
  y <- which(x==3)
  disty <- disty[y]
  
  # manually clean up as needed
  disty[[3]] <- NULL
  
  # which endpoints are tier 3?
  unavail.ends <- data.frame(endpoint=endpoints,tier=NA)
  unavail.ends[y,]$tier <- "tier3"
  unavail.ends[which(is.na(unavail.ends$tier)),]$tier <- "tier1or2"
  unavail.ends <- unavail.ends %>% mutate(available=ifelse(tier=='tier3','no','yes'))
  
  
  # save data frame of summary stats for addresses category
  # number of metrics to get sum stats for 
  ends <- length(disty)
  sum.distribution <- data.frame(metric=rep(NA,ends),obs=rep(NA,ends),mean=rep(NA,ends),
                               sd=rep(NA,ends),min=rep(NA,ends),P5=rep(NA,ends),P25=rep(NA,ends),
                               median=rep(NA,ends),P75=rep(NA,ends),P95=rep(NA,ends))
  
  
  # loop over all endpoints  
  for(n in 1:length(disty)){
    sum.distribution$metric[n] <- disty[[n]]$measure[1]
    sum.distribution$obs[n] <- disty[[n]] %>% filter(!is.na(v)) %>% select(v) %>% nrow()
    sum.distribution$mean[n] <- disty[[n]] %>% filter(!is.na(v)) %>% .$v %>% mean()
    sum.distribution$sd[n] <- disty[[n]] %>% filter(!is.na(v)) %>% .$v %>% sd()
    sum.distribution$min[n] <- disty[[n]] %>% filter(!is.na(v)) %>% .$v %>% summary() %>% .[1]
    sum.distribution$P5[n] <- disty[[n]] %>% filter(!is.na(v)) %>% .$v %>%quantile(.05)
    sum.distribution$P25[n] <- disty[[n]] %>% filter(!is.na(v)) %>% .$v %>% summary() %>% .[2]
    sum.distribution$median[n] <- disty[[n]] %>% filter(!is.na(v)) %>% .$v %>% summary() %>% .[3]
    sum.distribution$P75[n] <- disty[[n]] %>% filter(!is.na(v)) %>% .$v %>% .[5]
    sum.distribution$P95[n] <- disty[[n]] %>% filter(!is.na(v)) %>% .$v %>% quantile(.95)
    sum.distribution$max[n] <- disty[[n]] %>% filter(!is.na(v)) %>% .$v %>% summary() %>% .[6]
  }  
  
  # export summary stats
  setwd("D:/Research/Data Scraping/Crypto/Glassnode API/Data Export/Bitcoin/Distribution")
  write.csv(sum.distribution,"distribution_sum_stats.csv")
  
  # export endpoint tier categories
  write.csv(unavail.ends,"distribution_endpoint_tiers.csv")
  
  # create loop to export all plots
  for(n in 1:length(disty)){
    # create and export graphs
    tmp.graph <- disty[[n]] %>% mutate(date=as.POSIXct(t,origin='1970-01-01')) %>% 
      ggplot(.,aes(x=date,v)) +
      geom_line() +
      ggtitle(paste(disty[[n]]$measure[1]))
    # save plot
    ggsave(paste(disty[[n]]$measure[1],'.pdf',sep=''))
  }  
  
  

  
##### Entities Category #####
  
  # category
  category <- 'entities/'
  # vector of endpoints to loop through   
  endpoints <- c('active_count','min_1k_count','net_growth_count',
    'new_count','profit_relative','receiving_count','sending_count',
    'supply_balance_0001_001','supply_balance_001_01','supply_balance_01_1',
    'supply_balance_1_10','supply_balance_10_100','supply_balance_100_1k',
    'supply_balance_10k_100k','supply_balance_1k_10k','supply_balance_less_0001',
    'supply_balance_more_100k','supply_distribution_relative')
  
  # blank list
  entities <- list()
  
  for(n in 1:length(endpoints)){  
    # GET call 
    get <- GET(glue(base.url,category,endpoints[n],crypto,API_key))
    # prepare data
    dat <- fromJSON(rawToChar(get$content)) %>% try()
    
    ### view data ###
    # date and number of addresses 
    ifelse(get$status_code!=200,
           entities[[n]] <- data.frame(date=NA,value=NA),
           entities[[n]] <-  data.frame(date=as.POSIXct(dat$t,origin='1970-01-01'),
                                  value=dat$v) 
    )
    try(entities[[n]]$measure <- endpoints[n]) 
  }
  
  # *** All endpoints are tier 3 and off limits

  
##### Fees Category #####
  
  # category
  category <- 'fees/'
  # vector of endpoints to loop through   
  endpoints <- c('exchanges_mean','exchanges_relative','exchanges_sum','fee_ratio_multiple',
   'gas_limit_tx_mean','gas_limit_tx_median','gas_price_mean','gas_price_median',
   'gas_used_mean','gas_used_median','gas_used_sum','gas_used_sum_bridges',
   'gas_used_sum_bridges_relative','gas_used_sum_defi','gas_used_sum_defi_relative',
   'gas_used_sum_erc20','gas_used_sum_erc20_relative','gas_used_sum_nfts',
   'as_used_sum_nfts_relative','gas_used_sum_stablecoins','gas_used_sum_stablecoins_relative',
   'gas_used_sum_vanilla','gas_used_sum_vanilla_relative','tx_types_breakdown_relative',
   'tx_types_breakdown_sum','volume_mean','volume_median','volume_sum')
  
  # blank list
  fees <- list()
  
  for(n in 1:length(endpoints)){  
    # GET call 
    get <- GET(glue(base.url,category,endpoints[n],crypto,API_key))
    # prepare data
    dat <- fromJSON(rawToChar(get$content)) %>% try()
    
    ### view data ###
    # date and number of addresses 
    ifelse(get$status_code!=200,
           fees[[n]] <- data.frame(date=NA,value=NA),
           fees[[n]] <-  data.frame(date=as.POSIXct(dat$t,origin='1970-01-01'),
                                  v=dat$v) 
    )
    try(fees[[n]]$measure <- endpoints[n]) 
  }
  
  # not all endpoints have only 2 columns. also, some data points simply don't apply to Bitcoin 
  # (e.g. Gas fees)
  
  # get rid of endpoints that are tier 3 
  x <- NA
  for(n in 1:length(fees)){
    x[n] <- nrow(fees[[n]])
  }
  y <- which(x==1)
  fees[y] <- NULL
  
  # get rid of endpoints that are more than 2 columns. Can summarise these later upon request
  x <- NA
  for(n in 1:length(fees)){
    x[n] <- ncol(fees[[n]])
  }
  y <- which(x==3)
  fees <- fees[y]
  
  # manually clean up as needed --> ussually data fields that only apply to Ethereum 
  #  fees[[3]] <- NULL
  
  # which endpoints are tier 3?
  unavail.ends <- data.frame(endpoint=endpoints,tier=NA)
  unavail.ends[y,]$tier <- "tier3"
  unavail.ends[which(is.na(unavail.ends$tier)),]$tier <- "tier1or2"
  unavail.ends <- unavail.ends %>% mutate(available=ifelse(tier=='tier3','no','yes'))
  
  
  # save data frame of summary stats for addresses category
  # number of metrics to get sum stats for 
  ends <- length(fees)
  sum.fees <- data.frame(metric=rep(NA,ends),obs=rep(NA,ends),mean=rep(NA,ends),
                                 sd=rep(NA,ends),min=rep(NA,ends),P5=rep(NA,ends),P25=rep(NA,ends),
                                 median=rep(NA,ends),P75=rep(NA,ends),P95=rep(NA,ends))
  
  
  # loop over all endpoints  
  for(n in 1:length(fees)){
    sum.fees$metric[n] <- fees[[n]]$measure[1]
    sum.fees$obs[n] <- fees[[n]] %>% filter(!is.na(v)) %>% select(v) %>% nrow()
    sum.fees$mean[n] <- fees[[n]] %>% filter(!is.na(v)) %>% .$v %>% mean()
    sum.fees$sd[n] <- fees[[n]] %>% filter(!is.na(v)) %>% .$v %>% sd()
    sum.fees$min[n] <- fees[[n]] %>% filter(!is.na(v)) %>% .$v %>% summary() %>% .[1]
    sum.fees$P5[n] <- fees[[n]] %>% filter(!is.na(v)) %>% .$v %>%quantile(.05)
    sum.fees$P25[n] <- fees[[n]] %>% filter(!is.na(v)) %>% .$v %>% summary() %>% .[2]
    sum.fees$median[n] <- fees[[n]] %>% filter(!is.na(v)) %>% .$v %>% summary() %>% .[3]
    sum.fees$P75[n] <- fees[[n]] %>% filter(!is.na(v)) %>% .$v %>% .[5]
    sum.fees$P95[n] <- fees[[n]] %>% filter(!is.na(v)) %>% .$v %>% quantile(.95)
    sum.fees$max[n] <- fees[[n]] %>% filter(!is.na(v)) %>% .$v %>% summary() %>% .[6]
  }  
  
  # export summary stats
  setwd("D:/Research/Data Scraping/Crypto/Glassnode API/Data Export/Bitcoin/Fees")
  write.csv(sum.fees,"fees_sum_stats.csv")
  
  # export endpoint tier categories
  write.csv(unavail.ends,"fees_endpoint_tiers.csv")
  
  # create loop to export all plots
  for(n in 1:length(fees)){
    # create and export graphs
    tmp.graph <- fees[[n]] %>% 
      ggplot(.,aes(x=date,v)) +
      geom_line() +
      ggtitle(paste(fees[[n]]$measure[1]))
    # save plot
    ggsave(paste(fees[[n]]$measure[1],'.pdf',sep=''))
  }  
  
  
##### Indicators Category #####
  
  # category
  category <- 'indicators/'
  # vector of endpoints to loop through   
  endpoints <- c('accumulation_trend_score','asol','asol_account_based',
   'average_dormancy','average_dormancy_supply_adjusted','balanced_price_usd',
   'bvin','cdd','cdd_account_based','cdd_supply_adjusted','cdd_supply_adjusted_binary',
   'cdd90_account_based_age_adjusted','cdd90_age_adjusted','cvdd',
   'cyd','cyd_account_based','cyd_account_based_supply_adjusted','cyd_supply_adjusted',
   'difficulty_ribbon','difficulty_ribbon_compression','dormancy_account_based',
   'dormancy_flow','hash_ribbon','hodled_lost_coins','hodler_net_position_change',
   'investor_capitalization','liveliness','liveliness_account_based','msol',
   'mvrv_account_based','net_realized_profit_loss','net_unrealized_profit_loss',
   'net_unrealized_profit_loss_account_based','nupl_less_155','nupl_less_155_account_based',
   'nupl_more_155','nupl_more_155_account_based','nvt','nvt_entity_adjusted',
   'nvts','pi_cycle_top','puell_multiple','rcap_account_based','realized_loss',
   'realized_profit','realized_profit_loss_ratio','realized_profits_to_value_ratio',
   'reserve_risk','rhodl_ratio','seller_exhaustion_constant','soab',
   'sol_1d_1w','sol_1h','sol_1h_24h','sol_1m_3m','sol_1w_1m','sol_1y_2y',
   'sol_2y_3y','sol_3m_6m','sol_3y_5y','sol_5y_7y','sol_6m_12m','sol_7y_10y',
   'sol_more_10y','sopr','sopr_account_based','sopr_adjusted','sopr_less_155',
   'sopr_more_155','spent_output_price_distribution_ath','spent_output_price_distribution_percent',
   'ssr','ssr_oscillator','stock_to_flow_deflection','stock_to_flow_ratio','svab',
   'svl_1d_1w','svl_1h','svl_1h_24h','svl_1m_3m','svl_1w_1m','svl_1y_2y',
   'svl_2y_3y','svl_3m_6m','svl_3y_5y','svl_5y_7y','svl_6m_12m','svl_7y_10y',
   'svl_more_10y','unrealized_loss','unrealized_loss_account_based','unrealized_profit',
   'unrealized_profit_account_based','urpd_entity_adjusted','utxo_realized_price_distribution_ath',
   'utxo_realized_price_distribution_percent','velocity')
  
  # blank list
  indicators <- list()
  
  for(n in 1:length(endpoints)){  
    # GET call 
    get <- GET(glue(base.url,category,endpoints[n],crypto,API_key))
    # prepare data
    dat <- fromJSON(rawToChar(get$content)) %>% try()
    
    ### view data ###
    # date and number of addresses 
    ifelse(get$status_code!=200,
           indicators[[n]] <- data.frame(date=NA,value=NA),
           indicators[[n]] <-  dat
    )
    try(indicators[[n]]$measure <- endpoints[n]) 
  }
  
  # not all endpoints have only 2 columns. also, some data points simply don't apply to Bitcoin 
  # (e.g. Gas fees)
  
  # get rid of endpoints that are tier 3 
  x <- NA
  for(n in 1:length(indicators)){
    x[n] <- nrow(indicators[[n]])
  }
  y <- which(x==1)
  indicators[y] <- NULL
  
  # # manually clean up as needed --> ussually data fields that only apply to Ethereum 
  # indicators[c(1,14,17,25,34,50,53,54)] <- NULL

  
  # which endpoints are tier 3?
  unavail.ends <- data.frame(endpoint=endpoints,tier=NA)
  unavail.ends[y,]$tier <- "tier3"
  unavail.ends[which(is.na(unavail.ends$tier)),]$tier <- "tier1or2"
  unavail.ends <- unavail.ends %>% mutate(available=ifelse(tier=='tier3','no','yes'))
  
  
  # save data frame of summary stats for addresses category
  # number of metrics to get sum stats for 
  ends <- length(indicators)
  sum.indicators <- data.frame(metric=rep(NA,ends),obs=rep(NA,ends),mean=rep(NA,ends),
                         sd=rep(NA,ends),min=rep(NA,ends),P5=rep(NA,ends),P25=rep(NA,ends),
                         median=rep(NA,ends),P75=rep(NA,ends),P95=rep(NA,ends))
  
  
  # loop over all endpoints  
  for(n in 1:length(indicators)){
    sum.indicators$metric[n] <- indicators[[n]]$measure[1]
    sum.indicators$obs[n] <- indicators[[n]] %>% filter(!is.na(v)) %>% select(v) %>% nrow()
    sum.indicators$mean[n] <- indicators[[n]] %>% filter(!is.na(v)) %>% .$v %>% mean()
    sum.indicators$sd[n] <- indicators[[n]] %>% filter(!is.na(v)) %>% .$v %>% sd()
    sum.indicators$min[n] <- indicators[[n]] %>% filter(!is.na(v)) %>% .$v %>% summary() %>% .[1]
    sum.indicators$P5[n] <- indicators[[n]] %>% filter(!is.na(v)) %>% .$v %>%quantile(.05)
    sum.indicators$P25[n] <- indicators[[n]] %>% filter(!is.na(v)) %>% .$v %>% summary() %>% .[2]
    sum.indicators$median[n] <- indicators[[n]] %>% filter(!is.na(v)) %>% .$v %>% summary() %>% .[3]
    sum.indicators$P75[n] <- indicators[[n]] %>% filter(!is.na(v)) %>% .$v %>% .[5]
    sum.indicators$P95[n] <- indicators[[n]] %>% filter(!is.na(v)) %>% .$v %>% quantile(.95)
    sum.indicators$max[n] <- indicators[[n]] %>% filter(!is.na(v)) %>% .$v %>% summary() %>% .[6]
  }  
  
  # export summary stats
  setwd("D:/Research/Data Scraping/Crypto/Glassnode API/Data Export/Bitcoin/Indicators")
  write.csv(sum.indicators,"indicators_sum_stats.csv")
  
  # export endpoint tier categories
  write.csv(unavail.ends,"indicators_endpoint_tiers.csv")
  
  # create loop to export all plots
  for(n in 1:length(indicators)){
    # create and export graphs
    tmp.graph <- indicators[[n]] %>% mutate(date=as.POSIXct(t,origin='1970-01-01')) %>% 
      ggplot(.,aes(x=date,v)) +
      geom_line() +
      ggtitle(paste(indicators[[n]]$measure[1]))
    # save plot
    ggsave(paste(indicators[[n]]$measure[1],'.pdf',sep=''))
  }  
  
  
##### Insitutions Category #####
  
  # category
  category <- 'institutions/'
  # vector of endpoints to loop through   
  endpoints <- c('3iq_btcq_aum_sum','3iq_btcq_flows_sum','iq_btcq_holdings_sum',
    '3iq_btcq_market_price_usd','3iq_btcq_outstanding_units_sum',
    '3iq_btcq_premium_percent','3iq_ethq_aum_sum','3iq_ethq_flows_sum',
    '3iq_ethq_holdings_sum','3iq_ethq_market_price_usd','3iq_ethq_outstanding_units_sum',
    '3iq_ethq_premium_percent','3iq_qbtc_aum_sum','3iq_qbtc_flows_sum',
    '3iq_qbtc_holdings_sum','3iq_qbtc_market_price_usd','3iq_qbtc_outstanding_units_sum',
    '3iq_qbtc_premium_percent','3iq_qeth_aum_sum','3iq_qeth_flows_sum',
    '3iq_qeth_holdings_sum','3iq_qeth_market_price_usd','3iq_qeth_outstanding_units_sum',
    '3iq_qeth_premium_percent','purpose_etf_flows_sum','purpose_etf_holdings_sum')
  
  # blank list
  institutions <- list()
  
  for(n in 1:length(endpoints)){  
    # GET call 
    get <- GET(glue(base.url,category,endpoints[n],crypto,API_key))
    # prepare data
    dat <- fromJSON(rawToChar(get$content)) %>% try()
    
    ### view data ###
    # date and number of addresses 
    ifelse(get$status_code!=200,
           institutions[[n]] <- data.frame(date=NA,value=NA),
           institutions[[n]] <-  dat 
    )
    try(institutions[[n]]$measure <- endpoints[n]) 
  }
  
  # not interested in sum stats at this point. This is data on BTC and ETH ETFs that are managed by
  # 3iQ out of Canada
  
  

##### Lightning Category #####
  
  # category
  category <- 'lightning/'
  # vector of endpoints to loop through   
  endpoints <- c('base_fee_median','channel_size_mean','channel_size_median',
    'channels_count','fee_rate_median','gini_capacity_distribution',
    'gini_channel_distribution','network_capacity_sum','node_connectivity',
    'nodes_count')
  
  # blank list
  lightning <- list()
  
  for(n in 1:length(endpoints)){  
    # GET call 
    get <- GET(glue(base.url,category,endpoints[n],crypto,API_key))
    # prepare data
    dat <- fromJSON(rawToChar(get$content)) %>% try()
    
    ### view data ###
    # date and number of addresses 
    ifelse(get$status_code!=200,
           lightning[[n]] <- data.frame(date=NA,value=NA),
           lightning[[n]] <-  dat 
    )
    try(lightning[[n]]$measure <- endpoints[n]) 
  }
  
  # not all endpoints have only 2 columns. also, some data points simply don't apply to Bitcoin 
  # (e.g. Gas fees)
  
  # get rid of endpoints that are tier 3 
  x <- NA
  for(n in 1:length(lightning)){
    x[n] <- nrow(lightning[[n]])
  }
  y <- which(x==1)
  lightning[y] <- NULL
  
  # # manually clean up as needed --> ussually data fields that only apply to Ethereum 
  # lightning[c(9)] <- NULL
  
  
  # which endpoints are tier 3?
  unavail.ends <- data.frame(endpoint=endpoints,tier=NA)
  unavail.ends[y,]$tier <- "tier3"
  unavail.ends[which(is.na(unavail.ends$tier)),]$tier <- "tier1or2"
  unavail.ends <- unavail.ends %>% mutate(available=ifelse(tier=='tier3','no','yes'))
  
  
  # save data frame of summary stats for addresses category
  # number of metrics to get sum stats for 
  ends <- length(lightning)
  sum.lightning <- data.frame(metric=rep(NA,ends),obs=rep(NA,ends),mean=rep(NA,ends),
                               sd=rep(NA,ends),min=rep(NA,ends),P5=rep(NA,ends),P25=rep(NA,ends),
                               median=rep(NA,ends),P75=rep(NA,ends),P95=rep(NA,ends))
  
  
  # loop over all endpoints  
  for(n in 1:length(lightning)){
    sum.lightning$metric[n] <- lightning[[n]]$measure[1]
    sum.lightning$obs[n] <- lightning[[n]] %>% filter(!is.na(v)) %>% select(v) %>% nrow()
    sum.lightning$mean[n] <- lightning[[n]] %>% filter(!is.na(v)) %>% .$v %>% mean()
    sum.lightning$sd[n] <- lightning[[n]] %>% filter(!is.na(v)) %>% .$v %>% sd()
    sum.lightning$min[n] <- lightning[[n]] %>% filter(!is.na(v)) %>% .$v %>% summary() %>% .[1]
    sum.lightning$P5[n] <- lightning[[n]] %>% filter(!is.na(v)) %>% .$v %>%quantile(.05)
    sum.lightning$P25[n] <- lightning[[n]] %>% filter(!is.na(v)) %>% .$v %>% summary() %>% .[2]
    sum.lightning$median[n] <- lightning[[n]] %>% filter(!is.na(v)) %>% .$v %>% summary() %>% .[3]
    sum.lightning$P75[n] <- lightning[[n]] %>% filter(!is.na(v)) %>% .$v %>% .[5]
    sum.lightning$P95[n] <- lightning[[n]] %>% filter(!is.na(v)) %>% .$v %>% quantile(.95)
    sum.lightning$max[n] <- lightning[[n]] %>% filter(!is.na(v)) %>% .$v %>% summary() %>% .[6]
  }  
  
  # export summary stats
  setwd("D:/Research/Data Scraping/Crypto/Glassnode API/Data Export/Bitcoin/Lightning")
  write.csv(sum.lightning,"lightning_sum_stats.csv")
  
  # export endpoint tier categories
  write.csv(unavail.ends,"lightning_endpoint_tiers.csv")
  
  # create loop to export all plots
  for(n in 1:length(lightning)){
    # create and export graphs
    tmp.graph <- lightning[[n]] %>% mutate(date=as.POSIXct(t,origin='1970-01-01')) %>% 
      ggplot(.,aes(x=date,v)) +
      geom_line() +
      ggtitle(paste(lightning[[n]]$measure[1]))
    # save plot
    ggsave(paste(lightning[[n]]$measure[1],'.pdf',sep=''))
  }  
  
  
##### Market Category #####
  
  # category
  category <- 'market/'
  # vector of endpoints to loop through   
  endpoints <- c('amer_30d_price_change','apac_30d_price_change','deltacap_usd',
   'emea_30d_price_change','marketcap_realized_usd','marketcap_usd','mvrv',
   'mvrv_less_155','mvrv_more_155','mvrv_z_score','price_drawdown_relative',
   'price_realized_usd','price_usd_close','price_usd_ohlc','')
  
  # blank list
  market <- list()
  
  for(n in 1:length(endpoints)){  
    # GET call 
    get <- GET(glue(base.url,category,endpoints[n],crypto,API_key))
    # prepare data
    dat <- fromJSON(rawToChar(get$content)) %>% try()
    
    ### view data ###
    # date and number of addresses 
    ifelse(get$status_code!=200,
           market[[n]] <- data.frame(date=NA,value=NA),
           market[[n]] <-  dat 
    )
    try(market[[n]]$measure <- endpoints[n]) 
  }
  
  # not all endpoints have only 2 columns. also, some data points simply don't apply to Bitcoin 
  # (e.g. Gas fees)
  
  # get rid of endpoints that are tier 3 
  x <- NA
  for(n in 1:length(market)){
    x[n] <- nrow(market[[n]])
  }
  y <- which(x==1)
  market[y] <- NULL
  
  # # manually clean up as needed --> ussually data fields that only apply to Ethereum 
  # market[c(12)] <- NULL
  
  
  # which endpoints are tier 3?
  unavail.ends <- data.frame(endpoint=endpoints,tier=NA)
  unavail.ends[y,]$tier <- "tier3"
  unavail.ends[which(is.na(unavail.ends$tier)),]$tier <- "tier1or2"
  unavail.ends <- unavail.ends %>% mutate(available=ifelse(tier=='tier3','no','yes'))
  
  
  # save data frame of summary stats for addresses category
  # number of metrics to get sum stats for 
  ends <- length(market)
  sum.market <- data.frame(metric=rep(NA,ends),obs=rep(NA,ends),mean=rep(NA,ends),
                              sd=rep(NA,ends),min=rep(NA,ends),P5=rep(NA,ends),P25=rep(NA,ends),
                              median=rep(NA,ends),P75=rep(NA,ends),P95=rep(NA,ends))
  
  
  # loop over all endpoints  
  for(n in 1:length(market)){
    sum.market$metric[n] <- market[[n]]$measure[1]
    sum.market$obs[n] <- market[[n]] %>% filter(!is.na(v)) %>% select(v) %>% nrow()
    sum.market$mean[n] <- market[[n]] %>% filter(!is.na(v)) %>% .$v %>% mean()
    sum.market$sd[n] <- market[[n]] %>% filter(!is.na(v)) %>% .$v %>% sd()
    sum.market$min[n] <- market[[n]] %>% filter(!is.na(v)) %>% .$v %>% summary() %>% .[1]
    sum.market$P5[n] <- market[[n]] %>% filter(!is.na(v)) %>% .$v %>%quantile(.05)
    sum.market$P25[n] <- market[[n]] %>% filter(!is.na(v)) %>% .$v %>% summary() %>% .[2]
    sum.market$median[n] <- market[[n]] %>% filter(!is.na(v)) %>% .$v %>% summary() %>% .[3]
    sum.market$P75[n] <- market[[n]] %>% filter(!is.na(v)) %>% .$v %>% .[5]
    sum.market$P95[n] <- market[[n]] %>% filter(!is.na(v)) %>% .$v %>% quantile(.95)
    sum.market$max[n] <- market[[n]] %>% filter(!is.na(v)) %>% .$v %>% summary() %>% .[6]
  }  
  
  # export summary stats
  setwd("D:/Research/Data Scraping/Crypto/Glassnode API/Data Export/Bitcoin/Market")
  write.csv(sum.market,"market_sum_stats.csv")
  
  # export endpoint tier categories
  write.csv(unavail.ends,"market_endpoint_tiers.csv")
  
  # create loop to export all plots
  for(n in 1:length(market)){
    # create and export graphs
    tmp.graph <- market[[n]] %>% mutate(date=as.POSIXct(t,origin='1970-01-01')) %>% 
      ggplot(.,aes(x=date,v)) +
      geom_line() +
      ggtitle(paste(market[[n]]$measure[1]))
    # save plot
    ggsave(paste(market[[n]]$measure[1],'.pdf',sep=''))
  }  
  

##### Mempool Category #####
  
  # category
  category <- 'mempool/'
  # vector of endpoints to loop through   
  endpoints <- c('fees_average_relative','fees_distribution','fees_median_relative',
   'fees_sum','txs_count_distribution','txs_count_sum','txs_size_distribution',
   'txs_size_sum','txs_value_distribution','txs_value_sum')
  
  # blank list
  mempool <- list()
  
  for(n in 1:length(endpoints)){  
    # GET call 
    get <- GET(glue(base.url,category,endpoints[n],crypto,API_key))
    # prepare data
    dat <- fromJSON(rawToChar(get$content)) %>% try()
    
    ### view data ###
    # date and number of addresses 
    ifelse(get$status_code!=200,
           mempool[[n]] <- data.frame(date=NA,value=NA),
           mempool[[n]] <-  dat 
    )
    try(mempool[[n]]$measure <- endpoints[n]) 
  }
  
  # not all endpoints have only 2 columns. also, some data points simply don't apply to Bitcoin 
  # (e.g. Gas fees)
  
  # get rid of endpoints that are tier 3 
  x <- NA
  for(n in 1:length(mempool)){
    x[n] <- nrow(mempool[[n]])
  }
  y <- which(x==1)
  mempool[y] <- NULL
  
  # # manually clean up as needed --> ussually data fields that only apply to Ethereum 
  # mempool[c(2,5,7,9)] <- NULL
  
  
  # which endpoints are tier 3?
  unavail.ends <- data.frame(endpoint=endpoints,tier=NA)
  unavail.ends[y,]$tier <- "tier3"
  unavail.ends[which(is.na(unavail.ends$tier)),]$tier <- "tier1or2"
  unavail.ends <- unavail.ends %>% mutate(available=ifelse(tier=='tier3','no','yes'))
  
  
  # save data frame of summary stats for addresses category
  # number of metrics to get sum stats for 
  ends <- length(mempool)
  sum.mempool <- data.frame(metric=rep(NA,ends),obs=rep(NA,ends),mean=rep(NA,ends),
                           sd=rep(NA,ends),min=rep(NA,ends),P5=rep(NA,ends),P25=rep(NA,ends),
                           median=rep(NA,ends),P75=rep(NA,ends),P95=rep(NA,ends))
  
  
  # loop over all endpoints  
  for(n in 1:length(mempool)){
    sum.mempool$metric[n] <- mempool[[n]]$measure[1]
    sum.mempool$obs[n] <- mempool[[n]] %>% filter(!is.na(v)) %>% select(v) %>% nrow()
    sum.mempool$mean[n] <- mempool[[n]] %>% filter(!is.na(v)) %>% .$v %>% mean()
    sum.mempool$sd[n] <- mempool[[n]] %>% filter(!is.na(v)) %>% .$v %>% sd()
    sum.mempool$min[n] <- mempool[[n]] %>% filter(!is.na(v)) %>% .$v %>% summary() %>% .[1]
    sum.mempool$P5[n] <- mempool[[n]] %>% filter(!is.na(v)) %>% .$v %>%quantile(.05)
    sum.mempool$P25[n] <- mempool[[n]] %>% filter(!is.na(v)) %>% .$v %>% summary() %>% .[2]
    sum.mempool$median[n] <- mempool[[n]] %>% filter(!is.na(v)) %>% .$v %>% summary() %>% .[3]
    sum.mempool$P75[n] <- mempool[[n]] %>% filter(!is.na(v)) %>% .$v %>% .[5]
    sum.mempool$P95[n] <- mempool[[n]] %>% filter(!is.na(v)) %>% .$v %>% quantile(.95)
    sum.mempool$max[n] <- mempool[[n]] %>% filter(!is.na(v)) %>% .$v %>% summary() %>% .[6]
  }  
  
  # export summary stats
  setwd("D:/Research/Data Scraping/Crypto/Glassnode API/Data Export/Bitcoin/Mempool")
  write.csv(sum.mempool,"mempool_sum_stats.csv")
  
  # export endpoint tier categories
  write.csv(unavail.ends,"mempool_endpoint_tiers.csv")
  
  # create loop to export all plots
  for(n in 1:length(mempool)){
    # create and export graphs
    tmp.graph <- mempool[[n]] %>% mutate(date=as.POSIXct(t,origin='1970-01-01')) %>% 
      ggplot(.,aes(x=date,v)) +
      geom_line() +
      ggtitle(paste(mempool[[n]]$measure[1]))
    # save plot
    ggsave(paste(mempool[[n]]$measure[1],'.pdf',sep=''))
  }  
  
##### Mining Category #####
  
  # category
  category <- 'mining/'
  # vector of endpoints to loop through   
  endpoints <- c('difficulty_latest','hash_rate_mean','marketcap_thermocap_ratio',
   'miners_outflow_multiple','miners_unspent_supply','revenue_from_fees',
   'revenue_sum','thermocap','volume_mined_sum')
  
  # blank list
  mining <- list()
  
  for(n in 1:length(endpoints)){  
    # GET call 
    get <- GET(glue(base.url,category,endpoints[n],crypto,API_key))
    # prepare data
    dat <- fromJSON(rawToChar(get$content)) %>% try()
    
    ### view data ###
    # date and number of addresses 
    ifelse(get$status_code!=200,
           mining[[n]] <- data.frame(date=NA,value=NA),
           mining[[n]] <-  dat 
    )
    try(mining[[n]]$measure <- endpoints[n]) 
  }
  
  # not all endpoints have only 2 columns. also, some data points simply don't apply to Bitcoin 
  # (e.g. Gas fees)
  
  # get rid of endpoints that are tier 3 
  x <- NA
  for(n in 1:length(mining)){
    x[n] <- nrow(mining[[n]])
  }
  y <- which(x==1)
  mining[y] <- NULL
  
  # # manually clean up as needed --> ussually data fields that only apply to Ethereum 
  # mining[c(2,5,7,9)] <- NULL
  
  
  # which endpoints are tier 3?
  unavail.ends <- data.frame(endpoint=endpoints,tier=NA)
  unavail.ends[y,]$tier <- "tier3"
  unavail.ends[which(is.na(unavail.ends$tier)),]$tier <- "tier1or2"
  unavail.ends <- unavail.ends %>% mutate(available=ifelse(tier=='tier3','no','yes'))
  
  
  # save data frame of summary stats for addresses category
  # number of metrics to get sum stats for 
  ends <- length(mining)
  sum.mining <- data.frame(metric=rep(NA,ends),obs=rep(NA,ends),mean=rep(NA,ends),
                            sd=rep(NA,ends),min=rep(NA,ends),P5=rep(NA,ends),P25=rep(NA,ends),
                            median=rep(NA,ends),P75=rep(NA,ends),P95=rep(NA,ends))
  
  
  # loop over all endpoints  
  for(n in 1:length(mining)){
    sum.mining$metric[n] <- mining[[n]]$measure[1]
    sum.mining$obs[n] <- mining[[n]] %>% filter(!is.na(v)) %>% select(v) %>% nrow()
    sum.mining$mean[n] <- mining[[n]] %>% filter(!is.na(v)) %>% .$v %>% mean()
    sum.mining$sd[n] <- mining[[n]] %>% filter(!is.na(v)) %>% .$v %>% sd()
    sum.mining$min[n] <- mining[[n]] %>% filter(!is.na(v)) %>% .$v %>% summary() %>% .[1]
    sum.mining$P5[n] <- mining[[n]] %>% filter(!is.na(v)) %>% .$v %>%quantile(.05)
    sum.mining$P25[n] <- mining[[n]] %>% filter(!is.na(v)) %>% .$v %>% summary() %>% .[2]
    sum.mining$median[n] <- mining[[n]] %>% filter(!is.na(v)) %>% .$v %>% summary() %>% .[3]
    sum.mining$P75[n] <- mining[[n]] %>% filter(!is.na(v)) %>% .$v %>% .[5]
    sum.mining$P95[n] <- mining[[n]] %>% filter(!is.na(v)) %>% .$v %>% quantile(.95)
    sum.mining$max[n] <- mining[[n]] %>% filter(!is.na(v)) %>% .$v %>% summary() %>% .[6]
  }  
  
  # export summary stats
  setwd("D:/Research/Data Scraping/Crypto/Glassnode API/Data Export/Bitcoin/Mining")
  write.csv(sum.mining,"mining_sum_stats.csv")
  
  # export endpoint tier categories
  write.csv(unavail.ends,"mining_endpoint_tiers.csv")
  
  # create loop to export all plots
  for(n in 1:length(mining)){
    # create and export graphs
    tmp.graph <- mining[[n]] %>% mutate(date=as.POSIXct(t,origin='1970-01-01')) %>% 
      ggplot(.,aes(x=date,v)) +
      geom_line() +
      ggtitle(paste(mining[[n]]$measure[1]))
    # save plot
    ggsave(paste(mining[[n]]$measure[1],'.pdf',sep=''))
  }  
  
##### Protocols Category #####
  
  # category
  category <- 'protocols/'
  # vector of endpoints to loop through   
  endpoints <- c('uniswap_liquidity_latest','uniswap_transaction_count',
   'uniswap_volume_sum')
  
  # blank list
  protocols <- list()
  
  for(n in 1:length(endpoints)){  
    # GET call 
    get <- GET(glue(base.url,category,endpoints[n],crypto,API_key))
    # prepare data
    dat <- fromJSON(rawToChar(get$content)) %>% try()
    
    ### view data ###
    # date and number of addresses 
    ifelse(get$status_code!=200,
           protocols[[n]] <- data.frame(date=NA,value=NA),
           protocols[[n]] <-  dat 
    )
    try(protocols[[n]]$measure <- endpoints[n]) 
  }
  
 # all endpoints are tier 3
  
  
  
  
  
  
  
##### Supply Category #####
  
  # category
  category <- 'supply/'
  # vector of endpoints to loop through   
  endpoints <- c('active_1d_1w','active_1m_3m','active_1w_1m',
   'active_1y_2y','active_24h','active_2y_3y','active_3m_6m',
   'active_3y_5y','active_5y_7y','active_6m_12m','active_7y_10y',
   'active_more_10y','active_more_1y_percent','active_more_2y_percent',
   'active_more_3y_percent','active_more_5y_percent','amer_1y_supply_change',
   'apac_1y_supply_change','burned','current','current_adjusted',
   'emea_1y_supply_change','highly_liquid_sum','hodl_waves',
   'illiquid_change','illiquid_sum','inflation_rate','issued',
   'liquid_change','liquid_illiquid_sum','liquid_sum','loss_sum',
   'lth_loss_sum','lth_net_change','lth_profit_sum',
   'lth_sth_profit_loss_relative','lth_sum','minted',
   'probably_lost','profit_relative','profit_sum',
   'provably_lost','rcap_hodl_waves','revived_more_1y_sum',
   'revived_more_2y_sum','revived_more_3y_sum','revived_more_5y_sum',
   'sth_loss_sum','sth_lth_realized_value_ratio','sth_profit_loss_ratio',
   'sth_profit_sum','sth_sum','supply_by_txout_type')
  
  # blank list
  supply <- list()
  
  for(n in 1:length(endpoints)){  
    # GET call 
    get <- GET(glue(base.url,category,endpoints[n],crypto,API_key))
    # prepare data
    dat <- fromJSON(rawToChar(get$content)) %>% try()
    
    ### view data ###
    # date and number of addresses 
    ifelse(get$status_code!=200,
           supply[[n]] <- data.frame(date=NA,value=NA),
           supply[[n]] <-  dat 
    )
    try(supply[[n]]$measure <- endpoints[n]) 
  }
  
  # not all endpoints have only 2 columns. also, some data points simply don't apply to Bitcoin 
  # (e.g. Gas fees)
  
  # get rid of endpoints that are tier 3 
  x <- NA
  for(n in 1:length(supply)){
    x[n] <- nrow(supply[[n]])
  }
  y <- which(x==1)
  supply[y] <- NULL
  
  # # manually clean up as needed --> ussually data fields that only apply to Ethereum 
  # supply[c(17,20,21,22,23,24,27,28,29,31,32,33,34,35,36,40,41,46,47,48,49,50)] <- NULL
  
  
  # which endpoints are tier 3?
  unavail.ends <- data.frame(endpoint=endpoints,tier=NA)
  unavail.ends[y,]$tier <- "tier3"
  unavail.ends[which(is.na(unavail.ends$tier)),]$tier <- "tier1or2"
  unavail.ends <- unavail.ends %>% mutate(available=ifelse(tier=='tier3','no','yes'))
  
  
  # save data frame of summary stats for addresses category
  # number of metrics to get sum stats for 
  ends <- length(supply)
  sum.supply <- data.frame(metric=rep(NA,ends),obs=rep(NA,ends),mean=rep(NA,ends),
                            sd=rep(NA,ends),min=rep(NA,ends),P5=rep(NA,ends),P25=rep(NA,ends),
                            median=rep(NA,ends),P75=rep(NA,ends),P95=rep(NA,ends))
  
  
  # loop over all endpoints  
  for(n in 1:length(supply)){
    sum.supply$metric[n] <- supply[[n]]$measure[1]
    sum.supply$obs[n] <- supply[[n]] %>% filter(!is.na(v)) %>% select(v) %>% nrow()
    sum.supply$mean[n] <- supply[[n]] %>% filter(!is.na(v)) %>% .$v %>% mean()
    sum.supply$sd[n] <- supply[[n]] %>% filter(!is.na(v)) %>% .$v %>% sd()
    sum.supply$min[n] <- supply[[n]] %>% filter(!is.na(v)) %>% .$v %>% summary() %>% .[1]
    sum.supply$P5[n] <- supply[[n]] %>% filter(!is.na(v)) %>% .$v %>%quantile(.05)
    sum.supply$P25[n] <- supply[[n]] %>% filter(!is.na(v)) %>% .$v %>% summary() %>% .[2]
    sum.supply$median[n] <- supply[[n]] %>% filter(!is.na(v)) %>% .$v %>% summary() %>% .[3]
    sum.supply$P75[n] <- supply[[n]] %>% filter(!is.na(v)) %>% .$v %>% .[5]
    sum.supply$P95[n] <- supply[[n]] %>% filter(!is.na(v)) %>% .$v %>% quantile(.95)
    sum.supply$max[n] <- supply[[n]] %>% filter(!is.na(v)) %>% .$v %>% summary() %>% .[6]
  }  
  
  # export summary stats
  setwd("D:/Research/Data Scraping/Crypto/Glassnode API/Data Export/Bitcoin/supply")
  write.csv(sum.supply,"supply_sum_stats.csv")
  
  # export endpoint tier categories
  write.csv(unavail.ends,"supply_endpoint_tiers.csv")
  
  # create loop to export all plots
  for(n in 1:length(supply)){
    # create and export graphs
    tmp.graph <- supply[[n]] %>% mutate(date=as.POSIXct(t,origin='1970-01-01')) %>% 
      ggplot(.,aes(x=date,v)) +
      geom_line() +
      ggtitle(paste(supply[[n]]$measure[1]))
    # save plot
    ggsave(paste(supply[[n]]$measure[1],'.pdf',sep=''))
  }  
  
##### Transaction Category #####
  
  # category
  category <- 'transactions/'
  # vector of endpoints to loop through   
  endpoints <- c('contract_calls_external_count','contract_calls_internal_count',
   'count','entity_adjusted_count','rate',
   'segwit_adoption','size_mean','size_sum',
   'spent_output_types_share','taproot_adoption','transfers_between_exchanges_count',
   'transfers_count','transfers_count_bridges','transfers_count_bridges_relative',
   'transfers_count_defi','transfers_count_defi_relative','transfers_count_erc20',
   'transfers_count_erc20_relative','transfers_count_nfts','transfers_count_nfts_relative',
   'transfers_count_stablecoins','transfers_count_stablecoins_relative','transfers_count_vanilla',
   'transfers_count_vanilla_relative','transfers_exchanges_to_whales_count','transfers_from_exchanges_count',
   'transfers_from_miners_count','transfers_from_otc_desks_count','transfers_rate',
   'transfers_to_exchanges_count','transfers_to_miners_count','transfers_to_otc_desks_count',
   'transfers_volume_adjusted_mean','transfers_volume_adjusted_median','transfers_volume_adjusted_sum',
   'transfers_volume_between_exchanges_sum','transfers_volume_by_size_entity_adjusted_relative','transfers_volume_by_size_entity_adjusted_sum',
   'transfers_volume_entity_adjusted_mean','transfers_volume_entity_adjusted_median','transfers_volume_entity_adjusted_sum',
   'transfers_volume_exchanges_net','transfers_volume_exchanges_to_whales_sum','transfers_volume_from_exchanges_mean',
   'transfers_volume_from_exchanges_sum','transfers_volume_from_miners_sum','transfers_volume_from_otc_desks_sum',
   'transfers_volume_loss_sum','transfers_volume_mean','transfers_volume_median',
   'transfers_volume_miners_net','transfers_volume_miners_to_exchanges','transfers_volume_miners_to_exchanges_all',
   'transfers_volume_profit_relative','transfers_volume_profit_sum','transfers_volume_sum',
   'transfers_volume_to_exchanges_mean','transfers_volume_to_exchanges_sum','transfers_volume_to_miners_sum',
   'transfers_volume_to_otc_desks_sum','transfers_volume_whales_to_exchanges_sum','transfers_volume_within_exchanges_sum',
   'transfers_whales_to_exchanges_count','tx_types_breakdown_count','tx_types_breakdown_relative')
  
  # blank list
  transactions <- list()
  
  for(n in 1:length(endpoints)){  
    # GET call 
    get <- GET(glue(base.url,category,endpoints[n],crypto,API_key))
    # prepare data
    dat <- fromJSON(rawToChar(get$content)) %>% try()
    
    ### view data ###
    # date and number of addresses 
    ifelse(get$status_code!=200,
           transactions[[n]] <- data.frame(date=NA,value=NA),
           transactions[[n]] <-  dat 
    )
    try(transactions[[n]]$measure <- endpoints[n]) 
  }
  
  # not all endpoints have only 2 columns. also, some data points simply don't apply to Bitcoin 
  # (e.g. Gas fees)
  
  # get rid of endpoints that are tier 3 
  x <- NA
  for(n in 1:length(transactions)){
    x[n] <- ifelse(transactions[[n]] %>% class()=='list',1, 
      nrow(transactions[[n]]))
  }
  y <- which(x==1)
  transactions[y] <- NULL
  
  # # manually clean up as needed --> ussually data fields that only apply to Ethereum 
  # transactions[c(3,6,7)] <- NULL
  
  
  # which endpoints are tier 3?
  unavail.ends <- data.frame(endpoint=endpoints,tier=NA)
  unavail.ends[y,]$tier <- "tier3"
  unavail.ends[which(is.na(unavail.ends$tier)),]$tier <- "tier1or2"
  unavail.ends <- unavail.ends %>% mutate(available=ifelse(tier=='tier3','no','yes'))
  
  
  # save data frame of summary stats for addresses category
  # number of metrics to get sum stats for 
  ends <- length(transactions)
  sum.transactions <- data.frame(metric=rep(NA,ends),obs=rep(NA,ends),mean=rep(NA,ends),
                           sd=rep(NA,ends),min=rep(NA,ends),P5=rep(NA,ends),P25=rep(NA,ends),
                           median=rep(NA,ends),P75=rep(NA,ends),P95=rep(NA,ends))
  
  
  # loop over all endpoints  
  for(n in 1:length(transactions)){
    sum.transactions$metric[n] <- transactions[[n]]$measure[1]
    sum.transactions$obs[n] <- transactions[[n]] %>% filter(!is.na(v)) %>% select(v) %>% nrow()
    sum.transactions$mean[n] <- transactions[[n]] %>% filter(!is.na(v)) %>% .$v %>% mean()
    sum.transactions$sd[n] <- transactions[[n]] %>% filter(!is.na(v)) %>% .$v %>% sd()
    sum.transactions$min[n] <- transactions[[n]] %>% filter(!is.na(v)) %>% .$v %>% summary() %>% .[1]
    sum.transactions$P5[n] <- transactions[[n]] %>% filter(!is.na(v)) %>% .$v %>%quantile(.05)
    sum.transactions$P25[n] <- transactions[[n]] %>% filter(!is.na(v)) %>% .$v %>% summary() %>% .[2]
    sum.transactions$median[n] <- transactions[[n]] %>% filter(!is.na(v)) %>% .$v %>% summary() %>% .[3]
    sum.transactions$P75[n] <- transactions[[n]] %>% filter(!is.na(v)) %>% .$v %>% .[5]
    sum.transactions$P95[n] <- transactions[[n]] %>% filter(!is.na(v)) %>% .$v %>% quantile(.95)
    sum.transactions$max[n] <- transactions[[n]] %>% filter(!is.na(v)) %>% .$v %>% summary() %>% .[6]
  }  
  
  # export summary stats
  setwd("D:/Research/Data Scraping/Crypto/Glassnode API/Data Export/Bitcoin/Transactions")
  write.csv(sum.transactions,"transactions_sum_stats.csv")
  
  # export endpoint tier categories
  write.csv(unavail.ends,"transactions_endpoint_tiers.csv")
  
  # create loop to export all plots
  for(n in 1:length(transactions)){
    # create and export graphs
    tmp.graph <- transactions[[n]] %>% mutate(date=as.POSIXct(t,origin='1970-01-01')) %>% 
      ggplot(.,aes(x=date,v)) +
      geom_line() +
      ggtitle(paste(transactions[[n]]$measure[1]))
    # save plot
    ggsave(paste(transactions[[n]]$measure[1],'.pdf',sep=''))
  }  
```
