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
dl = list() 

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
   tryit <- try(getSymbols(symbol,from="2016-01-01", src='yahoo', auto.assign=FALSE))
   
   if(inherits(tryit, "try-error")){
     i <- i+1
     
     #renive line below should not be need now that auto.assign is set to false above
     #remove(list = symbols[i])
     
   } else {
     dl[[i]] <-tryit #Add stock data to list
     
     rm(symbol)
   }
 }
 rm(tryit) #drop tryit from memory

#'### ------------------------------------------------------------------------
#STEP3 Compute Signal Data for all Stock
# Goes throught the list of symbol data, Compusting the RSI values from the close price
#    and add the values to each xts set of symbol data.
 for(i in 1:length(dl)) {
 #Generate and merge RSI data to xts objelct
 #This method is using the Cl() function to find the close data
   dl[[i]] <- merge (dl[[i]],RSI(Cl(dl[[i]])))
   
 #Generate and merge MACD ( macd and signal column) data to xts object
 #This method is using [,"Close"] to find the close data
   dl[[i]] <- merge(dl[[i]], MACD( dl[[3]][,"Close"], 12, 26, 9, maType="EMA" ))
 }  

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

