#' ---
#' output:
#'   html_document:
#'     toc: TRUE
#'     toc_depth: 4
#'     code_folding: show
#'     
#' --- 
options(width = 200)

rm(list=ls())

#'### ------------------------------------------------------------------------
# STEP 1: Master List



#'### ------------------------------------------------------------------------
# STEP2: Creat a data list of stocks which can be succesfully found on yahoo
# ( Many fail)

get_stock_data_list <- function(symbols = c('AAPL','GOOG','EMAN')){
# returns a data list with containing the data from somes stocks of interest
  dl <- list()
  # uncomment symbols to test the function from within
  # symbols = c('AAPL','GOOG','EMAN')

   for(i in 1:length(symbols)) {
     
     symbols[i] -> symbol
     
     # specify the "from" date to desired start date
     tryit <- try(getSymbols(symbol, 
                             from        = Sys.Date() - 60, 
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

symbols = c('AAPL','GOOG','EMAN')
dl <- get_stock_data_list(symbols)

#'### ------------------------------------------------------------------------
# STEP3 Compute Signal Data for all Stock
# Goes throught the list of symbol data, Compusting the RSI values from the 
# close price and add the values to each xts set of symbol data.

get_interesting_symbols <- function(dlist = dl){
  
 # uncomment to test the function from within  
 dlist <- dl  
 symbol <- list()

 
 for(i in 1:length(dlist)){
   # i = 1 
   
  # Generate and merge RSI data to xts objelct
  # This method is using the Cl() function to find the close data
  # CL() is used to extract and transform 'OHLC' time series columns
  # RSI() is the Relative Strength Index
  
  dlist[[i]] <- merge(dlist[[i]], RSI(Cl(dlist[[i]]), n = 9))
  
  # Now take the RSI Data and apply a filter algorithm  to see if the RSI 
  # value ever dipped below 50 
  # ?? what is EMA?
  # dlist[[1]][,"EMA"]
  
  RSI_Threshold <- dlist[[i]][ ,"EMA"]
  
  # Apply the threshold algorithm to all EMA data row by row
  RSI_Threshold <- vapply(RSI_Threshold, 
                          function(x){ ifelse( x < 50, 1, 0) },
                          FUN.VALUE = numeric(nrow(RSI_Threshold)))
  
  # Rename the column to RSI Threshold so it can be found again in the xts object
  colnames(RSI_Threshold) <- "RSI_Threshold"
  
  # Merge the threshold data back inthe the xts object
  dlist[[i]] <- merge(dlist[[i]], RSI_Threshold)
  
  # If RSI threshold was met in the last 10 days add the stock to the 
  # interesting list.
  # append any stocks that meet the RSI threshold withing the last ten days
  # symbol <- list()
  # i=1
  
  symbol[[i]] <- 
    # convert the xts to a dataframe with the symbol as a column
    data.frame(dlist[[i]], symbol = attr(dlist[[i]], 'symbol')) %>%
    
    # keep the last ten rows in the dataframe
    tail(n = 10) %>%
    
    # group_by the symbol (there is only one symbol, but if I don't use 
    # group_by, the summarise will drop the column)
    group_by(symbol) %>%
    
    # sum up the ten RSI_thresholds
    summarise(rsi_sum = sum(RSI_Threshold)) %>%
    
    # keep the stock if at least one day it was above the threshold
    filter(rsi_sum > 0) %>%
    
    select(symbol) %>%
    
    # keep just the symbol column
    paste(.$symbol)
 }

  # the symbols is a list of tables
  # dply =  For each element of a list, apply function then combine results into a 
  # data frame
  symbol <- ldply(symbol, data.frame[,-1], .progress = 'text')
  
  return(symbol)

}
 
symbol_list <- get_interesting_symbols(dl)     
symbol_list
<<<<<<< HEAD

=======
>>>>>>> 55ff6717785a95b5c8a9f4ca63d1f052ea44b820
# COMMENETED OUT UNTIL ITS USEFUL
 # Generate and merge MACD ( macd and signal column) data to xts object
 # This method is using [,"Close"] to find the close data
   # dlist[[i]] <- merge(dlist[[i]], MACD( dlist[[i]][,"Close"], 12, 26, 9, maType="EMA" ))
 #'### ------------------------------------------------------------------------
 # END OF PROGRAM #############################################################
 ##############################################################################
 
 
# ways to look at the list (uncomment to try)
# str(dl)
# unlist(dl)
# flatten(dl)
# dl[1]
# dl[[1]]
# dl

# 
# # here is the original datalist
# dl
# 
# for (i in 1:length(dl)){
#   
#   # turn list i into a dataframe
#   df =  data.frame(dl[i]) 
#   
#   # multiply the 6th column of data times 2 and put into a new column 'times2'
#   df$times2 <- df[[6]]*2
#   
#   # turn the dataframe back into a list
#   df <- list(df)
#   
#   # replace the original list 'i' with the modified list
#   dl[i] <- df
# 
#   }
# 
# # the modified datalist
# dl

