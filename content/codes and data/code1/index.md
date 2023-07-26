---
title: "Coin Gecko API"
author: "Chad Dulle"
date: "2023-04-24"
type: "page"
summary: "Access Coingecko.com's API."
output: html_document
---
Here I will give some simple examples of using Coin Gecko's API to access market data for the broad cross-section of cryptocurrencies and tokens from various blockchains.

### Load Packages

```r
# Load Packages
library(tidyverse)
library(httr)
library(jsonlite)
library(glue)
```

### A Basic GET Call
Set the generic URL that can be used in conjunction with any of Coin Gecko's endpoints to make a GET call. In lamen's terms, a GET call is your computer asking the API to grant you access to a particular dataset (i.e. an "endpoint").

```r
# Base URL
base <- 'https://api.coingecko.com/api/v3/'
```

We will use this as a simple example of a single GET call. For this endpoint (coins/list), we will be getting every cryptocurrency/token that is available on CoinGecko's API

```r
##### List all assets #####  

  # set endpoint
  endpoint <- 'coins/list'

  # GET call
  get <- GET(glue(base,endpoint))
  # prepare data
  dat <- fromJSON(rawToChar(get$content))
  # view data
  df1 <- data.frame(name=dat$name,ticker=dat$symbol,id=dat$id)
```

### Optimizing API Limits to get Full Historical Data 

Each API has limits on the data you can retrieve for a single GET call, as well as limits on the number of GET calls you can push to the API in a given timeframe. For example, depending on your account with CoinGecko, you may be able to send somewhere between 10-50 GET calls per minute. For this reason, it is important to optimize the GET calls when getting data in bulk








