---
title: "Scraping Reddit Data"
author: "Chad Dulle"
date: "2023-07-20"
---

#### Purpose
Within this file, I share useful code for web scraping Reddit data. This includes keyword searching for subreddits, as well as extracting content from specific subreddit communities. Utilizing this data can enhance your research, allowing for a deeper dive into Social Network/Social Media analysis and market sentiment analysis. A prominent example of Reddit influencing the economic zeitgeist is the r/wallstreetbets community. Harness this code to explore such intriguing intersections of social media and economics.

#### Example: Bitcoin 
Reddit is a very large platform with many users and subreddits. To motivate the usefullness of this file, I will scrape "bitcoin" mentions from the Reddit social media platfrom.


```r
# Load packages
  library(tidyverse)
  # basic webscraping packages
  library(rvest)
  library(httr)
  library(jsonlite)
  # built on 'httr' and 'jsonlite'
  library(RedditExtractoR)
  # newer package 
  #      devtools::install_github("https://github.com/nathancunn/pushshiftR")  
  library(pushshiftR)


##########################
########## UDFs ##########
##########################
  
  # function to check date of unix timestamp
  check_date <- function(unix){
    as.POSIXct(unix, origin="1970-01-01")
  }
  
  # function to get unix timestamp from date object
  get_unix <- function(date){ # date is format yyyy-mm-dd
    as.numeric(as.POSIXct(paste(date,
                                " 00:00:00 EST")))
  }


################################################
############## pushshiftR Package ##############
################################################  
  
  # Let's look at how this is made
    
    #' Gets the pushshift data
    #'
    #' @param postType One of `submission` or `comment`
    #' @param title A string to search for in post titles
    #' @param size Number of results to return, maximum is 1000
    #' @param q A query to search for
    #' @param after Only search for posts made after this data, specified as a UNIX epoch time
    #' @param before As `after`, but before
    #' @param subreddit Only return posts made in this subreddit
    #' @param nest_level How deep to search? `nest_level = 1` returns only top-level comments
    #' @return A tibble of reddit submissions
    #' @export
    #' @importFrom jsonlite fromJSON
    #' @importFrom magrittr %>%
    #' @importFrom dplyr select filter
    #' @import tibble
    getPushshiftData <- function(postType, ...) {
      if(postType == "submission") {
        getPushshiftURL(postType, ...) %>%
          jsonlite::fromJSON() %>%
          .$data %>%
          jsonlite::flatten(recursive = TRUE) %>%
          select(author, title, selftext, created_utc, id, num_comments, score, subreddit) %>%
          as_tibble()
      } else {
        getPushshiftURL(postType, ...) %>%
          jsonlite::fromJSON() %>%
          .$data %>%
          jsonlite::flatten(recursive = TRUE) %>%
          select(author, body, parent_id, score, created_utc, subreddit) %>%
          as_tibble()
      }
    }
    
    #' Gets the pushshift URL
    #'
    #' @param postType One of `submission` or `comment`
    #' @param title A string to search for in post titles
    #' @param size Number of results to return, maximum is 1000
    #' @param q A query to search for
    #' @param after Only search for posts made after this data, specified as a UNIX epoch time
    #' @param before As `after`, but before
    #' @param subreddit Only return posts made in this subreddit
    #' @param nest_level How deep to search? `nest_level = 1` returns only top-level comments
    #' @return A URL
    #' @export
    #' @importFrom jsonlite fromJSON
    #' @importFrom magrittr %>%
    getPushshiftURL <- function(postType = "submission",
                                title = NULL,
                                size = NULL,
                                q = NULL,
                                after = NULL,
                                before = NULL,
                                subreddit = NULL,
                                nest_level = NULL) {
      if(!postType %in% c("submission", "comment")) {
        stop("postType must be one of `submission` or `comment`")
      }
      return(paste("https://api.pushshift.io/reddit/search/",
                   postType,
                   "?",
                   ifelse(is.null(title), "", sprintf("&title=%s", title)),
                   ifelse(is.null(size), "", sprintf("&size=%s", size)),
                   ifelse(is.null(q), "", sprintf("&q=%s", q)),
                   ifelse(is.null(after), "", sprintf("&after=%s", after)),
                   ifelse(is.null(before), "", sprintf("&before=%s", before)),
                   ifelse(is.null(subreddit), "", sprintf("&subreddit=%s", subreddit)),
                   ifelse(is.null(nest_level), "", sprintf("&nest_level=%s", nest_level)),
                   sep = ""))
    }
    
    #' Repeats getPushshiftData until desired period covered
    #' @param postType One of `submission` or `comment`
    #' @param title A string to search for in post titles
    #' @param size Number of results to return, maximum is 1000
    #' @param q A query to search for
    #' @param after Only search for posts made after this data, specified as a UNIX epoch time
    #' @param before As `after`, but before
    #' @param subreddit Only return posts made in this subreddit
    #' @param nest_level How deep to search? `nest_level = 1` returns only top-level comments
    #' @param delay Number of seconds to wait between queries to avoid stressing out the Pushshift server (limit is somewhere around 200 queries per minute)
    #' @return A tibble of the requested data
    #' @export
    #' @importFrom jsonlite fromJSON
    #' @importFrom magrittr %>%
    #' @importFrom dplyr last
    #' @importFrom tibble add_case
  
    getPushshiftDataRecursive <- function(postType = "submission",
                                          title = NULL,
                                          size = NULL,
                                          q = NULL,
                                          after = NULL,
                                          before = NULL,
                                          subreddit = NULL,
                                          nest_level = NULL,
                                          delay = 0) {
      tmp <- getPushshiftData(postType,
                              title,
                              size,
                              q,
                              after,
                              before,
                              subreddit,
                              nest_level)
      out <- tmp %>% filter(FALSE)
      on.exit(return(out), add = TRUE)
      after <- last(tmp$created_utc)
      while(nrow(tmp) > 0) {
        print(
          sprintf("%d %ss fetched, last date fetched: %s",
                  nrow(tmp),
                  postType,
                  as.Date(as.POSIXct(as.numeric(after), origin = "1970-01-01"))))
        out <- rbind(out, tmp)
        after <- last(tmp$created_utc)
        tmp <- getPushshiftData(postType,
                                title,
                                size,
                                q,
                                after,
                                before,
                                subreddit,
                                nest_level)
        Sys.sleep(delay)
      }
    }
    

##################################################
#################### Get Data ####################
##################################################
 

##############################
##### Reddit Submissions #####
##############################
    
    
##### Set Timeframe #####
    
  # set dates in unix
    # from date
    from.date <- as.numeric(as.POSIXct('2015-01-01',tz='GMT'))
    # to date
    to.date <- as.numeric(as.POSIXct('2022-05-01',tz='GMT'))

  # function to check date of unix timestamp
  check_date <- function(unix){
    as.POSIXct(unix, origin="1970-01-01")
  }
  
  # must play it safe with the submission limit (max 100 at a time)
  
  # save time interval --> second in a ...
    # minute
    sec.min <- 60
    # hour
    sec.hr <- 60*60 # this is the number of seconds in an hour
    # day
    sec.day <- 60*60*24
    
  # save number of time periods for the loop 
    # daily 
    periods.daily <- (to.date - from.date)/sec.day 
    periods.daily <- ceiling(periods.daily)
    # hourly
    periods.hourly <- (to.date - from.date)/sec.hr 
    periods.hourly <- ceiling(periods.hourly)
    # minute
    periods.minute <- (to.date - from.date)/sec.min 
    periods.minute <- ceiling(periods.minute)

    
###############################  
##### Search by subreddit #####
###############################
  
  ### find subreddits for bitcoin ###
  subs <- find_subreddits('bitcoin') 
  subs %>% select(subreddit,subscribers) %>% arrange(subscribers)
  
  ### Get submissions (i.e. posts) from r/bitcoin subreddit
  # *** NOTE: search by hour ***
    
    # save time periods (hours)
    periods <- NA
    for(i in 1:periods.hourly){
    periods[i] <- to.date - (sec.hr*i) # use hourly windows 
    }
    # sort dates from earliest to latest
    periods <- sort(periods)
    # check dates 
    periods %>% head(1) %>% as.POSIXct(.,origin='1970-01-01')
    periods %>% tail(1) %>% as.POSIXct(.,origin='1970-01-01')
  
    # save blank list
    l1 <- list()
    # write loop to get many Reddit submissions (limit 100 per request)
    for(i in 1:periods.hourly){
      # get submissions 
      l1[[i]] <- tryCatch(getPushshiftData(postType = "submission",
                                           size = 100, # max of 100 posts per request
                                           after = periods[i], # earlier date
                                           before = periods[i+1], # later date 
                                           subreddit = "bitcoin",
                                           nest_level = 1),
                          error = function(e){
                            message(paste("Zero Reddit submissions found"))
                          })
      Sys.sleep(0)
    }
    
    # make sure you didn't miss anything (if 100 submissions in any request, then you missed some)
    x <- rep(NA,length(l1))
    for(i in 1:length(l1)){
      x[i] <- ifelse(l1[[i]] %>% is.null(),
                     0,
                     l1[[i]] %>% nrow())
      try(ifelse(x[i]>=100,message(paste('YOU MISSED SOME')),""))
    }
    
    
    # need to re-run these with a shorter time
    which(x>=100)
    # run loop over all periods with more than 100 Reddit posts 
    l3 <- list()
    for(n in 1:(which(x>=100) %>% length())){         
      # run for each minute instead of each hour 
      l2 <- list()
      for(i in 1:120){ # 120 minutes in 2 hours (using hour before to hour after to be safe)
        l2[[i]] <- tryCatch(getPushshiftData(postType = "submission",
                                             size = 100, # max of 100 posts per request
                                             after = (periods[which(x>=100)][n])+(60*(i-1)), # earlier date
                                             before = (periods[which(x>=100)][n])+(60*i), # later date
                                             subreddit = "bitcoin",
                                             nest_level = 1),
                            error = function(e){
                              message(paste("Zero Reddit submissions found"))
                            })
        Sys.sleep(0)
      } # end inside loop
      l3[[n]] <- do.call(rbind,l2)
    } # end outside loop 
    
    df2 <- do.call(rbind,l3)

    
    
    
    # put into data frame
    df1 <- do.call(rbind,l1)
    
    # create date
    df.rbitcoin <- df1 %>% mutate(ts=as.POSIXct(created_utc,origin='1970-01-01'))
    df.rbitcoin
    
    # check dates
    df.rbitcoin %>% arrange(ts) %>% select(ts) %>% head(1)
    df.rbitcoin %>% arrange(ts) %>% select(ts) %>% tail(1)
  
  
#############################  
##### Search by keyword #####
#############################
  
  # save blank list
  l1 <- list()
  # write loop to get many Reddit submissions (limit 100 per request)
  for(i in 1:periods){
    # get submissions 
    l1[[i]] <- tryCatch(getPushshiftData(postType = "submission",
                                         size = 100, # max of 100 posts per request
                                         after = start.date + sec.day*i, # *** make sure the time interval matches 
                                         before = start.date + (sec.day*(i+1)),
                                         subreddit = "wallstreetbets",
                                         q='bitcoin',
                                         nest_level = 1),
                        error = function(e){
                          message(paste("Zero Reddit submissions found"))
                        })
    Sys.sleep(0)
  }
  
  # make sure you didn't miss anything (if 100 submissions in any request, then you missed some)
  x <- rep(NA,length(l1))
  for(i in 1:length(l1)){
    x[i] <- ifelse(l1[[i]] %>% is.null(),
                   0,
                   l1[[i]] %>% nrow())
    try(ifelse(x[i]>=100,message(paste('WARNING')),""))
  }
  x
  
  # put into data frame
  df1 <- do.call(rbind,l1)

  # create date
  df <- df1 %>% mutate(date=check_date(created_utc))
```

