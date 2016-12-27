
symbols = c('AAPL','GOOG','EMAN')
d_symbols <- get_stock_data_list(symbols)

d_symbols

datalist <- get_dlist(d_symbols, periods = 9, 
                      sell_above = 55, buy_below = 35)

datalist

symbols <- get_symbols(datalist)
symbols

dl2 <- get_stock_data_list(symbols)
dl2 <- get_dlist(dl2)
head(dl2)

#'### ------------------------------------------------------------------------
#
# # here is the original datalist

add_amount <- function(balance, amount){
  balance2 <- balance + amount
  return(balance2)
}

transact_shares <- function(shares, balance, price, percent, transaction = "buy"){
  # shares = 0
  # check_account = 3123
  # price = 2.35
  # percent = .1
  # transaction = "buy"

  n_shares <- round(percent*check_account/price, digits = 0)
  n_shares

 shares2 <-   switch(transaction,
           buy  = shares + n_shares,
           sell = if(shares >= n_shares){ shares - n_shares} else {0})
          
   shares2
  return(shares2)
}

checking_account <- function(shares = 0, balance = 0,  price = 0, percent = 0, 
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
           sell     = if(shares >= n_shares){ balance + n_shares*price} else{
                      balance + shares*price},
           deposit  = balance + amount,
           withdraw = balance - amount
           )
  balance2
  return(balance2)
}



i = 1
dl <- data.frame(dl2[i])
 dl$check_account[1] <- 200
 dl$shares  <- 0
 dl$hold.cash.balance[1] <- 200
 # dl$hold.stock.balance[1] <- 200
 # dl$hold.stock.shares <- 0

 dl
 
dl <- dl %>%
  mutate(date    = row.names(.)) %>%
  select(date, price = 4, RSI_buy, RSI_sell, check_account, shares, hold.cash.balance) %>%#, 
       #  hold.stock.shares, hold.stock.balance) %>%
  mutate(date    = ymd(date)) %>%
   na.omit()

for (i in 2:nrow(dl)){
  # i = 15
  percent = .1
  
  dl$hold.cash.balance[i] <- checking_account(balance = dl$hold.cash.balance[i-1],
                                      transaction = "deposit", amount = 200)
  # dl$hold.cash.balance[i]
  
  # every day we get $200 added to the check_account
  dl$check_account[i] <- checking_account(balance = dl$check_account[i-1],
                                          transaction = "deposit", amount = 200)
  #dl$check_account[i]

  #every day we get $200 added to the only buy balance
  dl$hold.stock.balance[i] <- checking_account(balance = dl$hold.stock.balance[i-1],
                                             transaction = "deposit", amount = 200)
  dl$hold.stock.balance[i]

  # every day we buy if able
  # dl$hold.stock.shares[i] <- transact_shares(shares = dl$hold.stock.shares[i-1],
  #                                            balance = dl$hold.stock.balance[i],
  #                                            price = dl$price[i],
  #                                            percent = .95,
  #                                            transaction = "buy")
  # 
  # dl$hold.stock.balance[i] <- checking_account(shares  = dl$hold.stock.shares[i-1],
  #                                           balance = dl$hold.cash.balance[i],
  #                                           price   = dl$price[i],
  #                                           percent = .95,
  #                                           transaction = "buy")

                                             
dl$shares[i]
  # do we buy anything today?
  if (dl$RSI_buy[i] > 0 ) #&& dl$check_account[i] > dl$price[i])
      dl$shares[i] <- transact_shares(shares = dl$shares[i-1],
                                balance = dl$check_account[i],
                                price = dl$price[i],
                                percent = percent,
                                transaction = "buy"
                                ) else dl$shares[i] <- dl$shares[i-1]


    if (dl$RSI_buy[i] > 0 )#&& dl$check_account[i] > dl$price[i])
       dl$check_account[i] <- checking_account(shares  = dl$shares[i],
                                               balance = dl$check_account[i],
                                               price   = dl$price[i],
                                               percent = percent,
                                               transaction = "buy")

  # do we sell anything today?
    if (dl$RSI_sell[i] > 0 ) #&& dl$shares[i] > 0)
        dl$shares[i] <- transact_shares(shares = dl$shares[i-1],
                                        balance = dl$check_account[i],
                                        price = dl$price[i],
                                        percent = percent,
                                        transaction = "sell"
                                        ) #else dl$shares[i] <- dl$shares[i-1]

    if (dl$RSI_sell[i] > 0 ) #&& dl$shares[i] > 0)
      dl$check_account[i] <- checking_account(shares  = dl$shares[i-1],
                                              balance = dl$check_account[i],
                                              price   = dl$price[i],
                                              percent = percent,
                                              transaction = "sell")

}


head(dl, n=20)

dl <-      mutate(dl, total.value = check_account + price * shares)

tail(dl, n=20)

ggplot(dl, aes(x = date, y = hold.cash.balance)) + 
  geom_line( color = "blue", show.legend = TRUE) +
  # geom_line(aes( x = date, y = hold.stock.value), color = "red") +
  geom_line(aes( x = date, y = total.value), color = "green") 


write.csv(dl, file =  "./report/ddollars.csv")


# # Convert factors to characters
# iris %>% str()
# 
# iris %>%
#   map_if(is.factor, as.character) %>%
#   str()
# contains 15
# # Specify which columns to map with a numeric vector of positions:
# mtcars %>% map_at(c(1, 4, 5), as.character) %>% str()
# # Or with a vector of names:
# mtcars %>% map_at(c("cyl", "am"), as.character) %>% str()
# list(x = rbernoulli(100), y = 1:100) %>%
#   transpose() %>%
#   map_if("x", ~ update_list(., y = ~ y * 100)) %>%
#   transpose() %>%
#   simplify_all()




> myfile %>% mutate(V5 = ifelse(V1 == 1 & V2 != 4, 1, ifelse(V2 == 4 & V3 != 1, 2, 0)))



head(dl[, 7:14])


df <- expand.grid(x = 1:3, y = 3:1)
df
df %>% rowwise() %>% do(i = seq(.$x, .$y))
.Last.value %>% summarise(n = length(i))
df

#'### ------------------------------------------------------------------------

get_balance_over_time <- function(){
 dl <- dl2
   
  for (i in 1:length(dl)){
     i = 1

    # add balance
    df <- data.frame(dl[i])
    df$index <- 1:nrow(df)
    head(df)
    
    
    df %<>%
      mutate(date    = row.names(.)) %>%
      mutate(date    = ymd(date)) %>%
      mutate(hold_cash = 200 * index) 
     head(df)
    
    head(df)
    
    adjust_balance <- function(balance = 0, adjustment = 0, price = 0){
      # adjustment = 10
      shares_to_buy <- balance/price
      adjustment    <- shares_to_buy*price
      bal <- balance - adjustment
  }
    
    df$balance <- 0
    df$balance[1] <- 200
    df2 <- mutate(df, balance = adjust_balance(balance = balance, 
                                               adjustment = 10))
  
    for(i in 1:nrow(df)){
      i = 1
      if(df$RSI_sell[i]>0){
        
       df$balance[i] = adjust_balance(balance = df$balance[i], price = df[i,4]) 
      }
      
    }
    
    head(df)
    
    
    for (i in 2:nrow(df)){
      df$balance[i] <- df$balance[i-1] + 200
    }
    
    head(df)
    
    mutate(players, G_delta = G - lag(G))
    
    ggplot(df, aes(x = date, y = hold_cash)) + geom_line()

    
    
    # turn the dataframe back into a list
    df <- list(df)
    
    # replace the original list 'i' with the modified list
    dl[i] <- df
    


    
  }
} 

#'### ------------------------------------------------------------------------

# Currently rowwise grouping only works with data frames. Its main
# impact is to allow you to work with list-variables in summarise and mutate
# without having to use [[1]]. This makes summarise() on a rowwise tbl
# effectively equivalent to plyr’s ldply. Examples
df <- expand.grid(x = 1:3, y = 3:1)
df
df %>% rowwise() %>% do(i = seq(.$x, .$y))
.Last.value %>% summarise(n = length(i))
