#' ---
#' output:
#'   html_document:
#'     toc: TRUE
#'     toc_depth: 4
#'     code_folding: show
#'     
#' --- 
# options(width = 200)

# rm(list=ls())

#'### ------------------------------------------------------------------------
# STEP 1: Master List


#'### ------------------------------------------------------------------------
#'##### STEP2: Creat a data list of stocks which can be succesfully found on yahoo
# ( Many fail)

f2_get_stock_data <- function(symbols = c('AAPL','GOOG','EMAN' ), days = 90){
# returns a data list with containing the data from somes stocks of interest
  dl <- list()

  # uncomment symbols to test the function from within
    # symbols = c('AAPL','GOOG','EMAN')

   for(i in 1:length(symbols)) {
     
     symbols[i] -> symbol
     
     # specify the "from" date to desired start date
     tryit <- try(getSymbols(symbol, 
                             from        = Sys.Date() - days, 
                             src         = 'yahoo', 
                             auto.assign = FALSE))
     
     if(inherits(tryit, "try-error")){
       i <- i + 1
     } 
     
     else {
       # Add stock data to list
       dl[[i]] <- tryit 
       attr(dl[[i]], 'symbol') <- symbol
       attr
       rm(symbol)
     }
   }
  
  return(dl)
}

#'### ------------------------------------------------------------------------
# STEP3 Compute Signal Data for all Stock
# Goes throught the list of symbol data, Compusting the RSI values from the 
# close price and add the values to each xts set of symbol data.

f3_get_stock_thresholds <- function(dlist = dl, periods = 9, 
                      sell_above = 45, buy_below = 40){
  
 # uncomment to test the function from within  
  # dlist <- dl
  # periods = 9
  # sell_above = 70
  # buy_below = 35
 
 for(i in 1:length(dlist)){
   # i = 1
   
  # Generate and merge RSI data to xts objelct
  # This method is using the Cl() function to find the close data
  # CL() is used to extract and transform 'OHLC' time series columns
  # RSI() is the Relative Strength Index
  
  dlist[[i]] <- merge(dlist[[i]], RSI(Cl(dlist[[i]]), n = periods))
  
  # Now take the RSI Data and apply a filter algorithm  to see if the RSI 
  # value ever dipped below 50 
  
  RSI_Threshold <- dlist[[i]][ ,"EMA"]
  
  # Apply the threshold algorithm to all EMA data row by row
  RSI_Threshold <- vapply(RSI_Threshold, 
                          function(x){ ifelse( x < 50, 1, 0) },
                          FUN.VALUE = numeric(nrow(RSI_Threshold)))
  # RSI_Threshold
  
  RSI_sell <- dlist[[i]][ ,"EMA"]
  
  RSI_sell      <- vapply( RSI_sell, 
                           function(x){ ifelse( x > sell_above, 1, 0) },
                           FUN.VALUE = numeric(nrow(RSI_sell)))
  RSI_sell
  
  RSI_buy <- dlist[[i]][ ,"EMA"]
  
  RSI_buy      <- vapply( RSI_buy, 
                           function(x){ ifelse( x < buy_below, 1, 0) },
                           FUN.VALUE = numeric(nrow(RSI_buy)))
  RSI_buy
  
  # Rename the column to RSI Threshold so it can be found again in the xts object
  colnames(RSI_Threshold) <- "RSI_Threshold"
  colnames(RSI_sell)      <- "RSI_sell"
  colnames(RSI_buy)       <- "RSI_buy"
  
  # Merge the threshold data back inthe the xts object
  dlist[[i]] <- merge(dlist[[i]], RSI_Threshold )
  dlist[[i]] <- merge(dlist[[i]], RSI_sell)
  dlist[[i]] <- merge(dlist[[i]], RSI_buy)
  
  # Generate and merge MACD ( macd and signal column) data to xts object
  # This method is using [,"Close"] to find the close data
  # dlist[[i]] <- merge(dlist[[i]], MACD( dlist[[i]][,"Close"], 
  #                                       nFast = 12, 
  #                                       nSlow = 26, 
  #                                       nSig = 9, 
  #                                       maType = "EMA" ))
 }
 
 return(dlist)
}
  

#'### ------------------------------------------------------------------------
# STEP4  If RSI threshold was met in the last 10 days add the stock to the 
# interesting list. append any stocks that meet the RSI threshold withing the
# last ten days
# symbol <- list()
# i=1
f4_get_interesting_symbols <- function(dlist = stock_rsi){
  dlist = stock_rsi
  symbol <- list()
   
  for(i in 1:length(dlist)){ 
    symbol[[i]] <- 
      
       # i = 1
      # convert the xts to a dataframe with the symbol as a column
      data.frame(dlist[[i]], symbol = attr(dlist[[i]], 'symbol')) %>%
      
      # keep the last ten rows in the dataframe
      tail(n = 10) %>%
      
      # group_by the symbol (there is only one symbol, but if I don't use 
      # group_by, the summarise will drop the column)
      mutate(symbol = as.character(symbol)) %>%
      
      group_by(symbol) %>%
      
      # sum up the ten RSI_thresholds
      dplyr::summarise(rsi_sum = sum(RSI_Threshold)) %>%
      
      # keep the stock if at least one day it was above the threshold
      filter(rsi_sum > 0) %>%
      
      select(symbol)
  }
  
  # the symbols is a list of tables dply =  For each element of a list, apply
  # function then combine results into a data frame

  symbol <- unlist(symbol, use.names = TRUE, recursive = TRUE)
 
  # symbol <- plyr::ldply(symbol, data.frame, .progress = 'text')
  return(symbol)
}

#'### ------------------------------------------------------------------------
# function to buy or sell stock shares
transact_shares <- function(shares, balance, price, percent, 
                            transaction = "buy/sell"){
  # shares      = 0
  # balance     = 3123
  # price       = 2.35
  # percent     = .1
  # transaction = "buy"
  
  n_shares <- round(percent*balance/price, digits = 0)
  # n_shares
  
  shares2 <- switch(transaction,
                    buy  = shares + n_shares,
                    sell = if( shares >= n_shares){ shares - n_shares} else {0})
  # shares2
  return(shares2)
}

#'### ------------------------------------------------------------------------
# function to adjust your account balance depending on the type of transaction
transact_account <- function(shares = 0, balance = 0,  price = 0, percent = 0, 
                             transaction = "buy/sell/deposit/withdraw", 
                             amount = 200){
  # shares = 1
  # balance = 1000
  # price = 10
  # percent = .1
  # transaction = "deposit"
  # amount = 200
  
  n_shares <- round(percent*balance/price, digits = 0)
  n_shares
  
  balance2 <- 
    switch(transaction,
           buy      = balance - n_shares*price,
           sell     = if(shares >= n_shares) { balance + n_shares*price} else{
             balance + shares*price},
           deposit  = balance + amount,
           withdraw = balance - amount
    )
  balance2
  return(balance2)
}

#'### ------------------------------------------------------------------------
#' convert the stocks to a data frame, rename the columns, flatten and nest
f5_flatten_and_nest_by_stock <- function(dl2){
  
  dl <- dl2
  
  for( i in 1:length(dl)) {
    # i = 1
    
    # get the stock name
    stock_name <- names(dl[[i]]) %>%
      str_split( pattern = "\\.") %>% 
      unlist() %>%
      first()
    
    stock_name
    
    # convert each xts object in the list to a dataframe
    dl[[i]] <- as.data.frame(dl[[i]])
    
    # rename the columns for each dataframe
    names(dl[[i]])
    
    colnames(dl[[i]]) <- c("open", "high", "low", "close", "volume", 
                           "adjusted","ema", "rsi_threshold", "rsi_sell", "rsi_buy")
    
    # take the dates that are in the row names and make them a column in the dataframe.
    # then add the stock name
    dl[[i]] <- dl[[i]] %>%
      mutate(date  = ymd(row.names(.))) %>%
      mutate(stock = stock_name)
    
  }
  # head(dl)
  
  # flatten the list of data frames into one dataframe
  
  dl_flat <- list.rbind(dl)
  
  dl_flat <- na.omit(dl_flat)
  head(dl_flat)
  
  # nest by each stock
  dl_nested <- dl_flat %>%
    group_by(stock) %>%
    nest()
  
  # return(dl_flat)
  return(dl_nested)
}


