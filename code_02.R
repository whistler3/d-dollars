
symbols = c('AAPL','GOOG','EMAN')
d_symbols <- get_stock_data_list(symbols)

datalist <- get_dlist(d_symbols)

datalist

symbols <- get_symbols(datalist)
symbols$symbol

dl2 <- get_stock_data_list(symbols$symbol)
dl2 <- get_dlist(dl2)
dl2

#'### ------------------------------------------------------------------------
#
# # here is the original datalist
get_balance_over_time <- function(){
 dl <- dl2
   
  for (i in 1:length(dl)){
    # i = 1

    # add balance
    df <- data.frame(dl[i])
    df$index <- 1:nrow(df)
    
    df %<>%
      mutate(date = row.names(.)) %>%
      mutate(date = ymd(date)) %>%
      mutate(balance = 200 * index) 
    head(df)
    
    ggplot(df, aes(x = date, y = balance)) + geom_line()

    
    
    # turn the dataframe back into a list
    df <- list(df)
    
    # replace the original list 'i' with the modified list
    dl[i] <- df
    


    
  }
} 
  # 
