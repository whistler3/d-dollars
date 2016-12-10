#' ---
#' output:
#'   html_document:
#'     toc: TRUE
#'     toc_depth: 4
#'     code_folding: show
#'     
#' --- 
options(width = 200)


datalist = list() 
#'### ------------------------------------------------------------------------
#STEP 1: Master List
symbols = c('AAPL','GOOG','EMAN')

#'### ------------------------------------------------------------------------
#STEP2: Creat a data list of stocks which can be succesfully found on yahoo( Many fail)
# As of 0.4-0, ‘getSymbols’ uses env=parent.frame() and
# auto.assign=TRUE by default.
# 
# This  behavior  will be  phased out in 0.5-0  when the call  will
# default to use auto.assign=FALSE. getOption("getSymbols.env") and 
# getOptions("getSymbols.auto.assign") are now checked for alternate defaults

 for(i in 1:length(symbols)) {
   
   symbols[i]-> symbol
   
   # specify the "from" date to desired start date
   tryit <- try(getSymbols(symbol,
                           from="2016-12-01", 
                           src='yahoo', auto.assign=FALSE))
   if(inherits(tryit, "try-error")){
     i <- i+1
     
     remove(list = symbols[i])
   } else {
     datalist[[i]] <-tryit
     rm(symbol)
   }
 }



#'### ------------------------------------------------------------------------
#STEP3 Compute Signal Data for all Stock
#TODO:
# For each stock data in list, create a new column with RSI() data from the close price


yahoo <- data.frame(datalist[1])
yahoo < mutate(yahoo, times2 = AAPL.Adjusted*2 )
yahoo 