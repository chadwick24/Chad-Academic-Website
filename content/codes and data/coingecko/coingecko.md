---
title: "Accessing Coin Gecko's API for Cryptocurrency Market Data"
author: "Chad Dulle"
date: "2023-07-27"
output: html_document
---

Here I will give some simple examples of using Coin Gecko's API to access market data for the broad cross-section of cryptocurrencies and tokens from various blockchains. Coin Gecko offers a basic subscription that is free to use for everyone interested in learning more about cryptocurrency data

### Load Packages

```r
# Install Packages
install.packages('tidyverse')
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
