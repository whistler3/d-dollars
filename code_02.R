
symbols = c('AAPL','GOOG','EMAN')
d_symbols <- get_stock_data_list(symbols)

d_symbols

datalist <- get_dlist(d_symbols, periods = 9, 
                      sell_above = 45, buy_below = 40)

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

buy_shares <- function(shares, balance, price, buy_percent){
  shares2 <- shares + round(buy_percent*(balance/price), digits = 0)
  shares2 <- as.integer(shares2)
  return(shares2)
}

balance_after_sale <- function(shares, balance,  price, sell_percent){
  sell_n_shares <- round(buy_percent*(balance/price), digits = 0)
  # shares_remaining <- shares - sell_n_shares
  balance2 <- balance + price * sell_n_shares
  return(balance2)
}

balance_after_buy <- function(shares, balance,  price, buy_percent){
  buy_n_shares <- round(buy_percent*(balance/price), digits = 0)
  balance2 <- balance - price * buy_n_shares
  return(balance2)
}

sell_shares <- function(shares, balance,  price, sell_percent){
  sell_n_shares <-  shares - round(buy_percent*(balance/price), digits = 0)
  shares_remaining <- shares - sell_n_shares
  return(shares_remaining)
}
# 
# for(i in 2:nrow(dl)){
#   # every day we get $200 added to the balance 
#   dl$balance[i] <- add_amount(dl$balance[i-1] , 200)
# }

i = 1
dl <- data.frame(dl2[i])
 dl$balance <- 2000
 dl$shares  <- 0
 dl$hold.cash <- 2000
 dl$only.buy.balance <- 2000
 dl$only.buy.shares <- 0

 
dl <- dl %>%
  mutate(date    = row.names(.)) %>%
  select(date, price = 4, RSI_buy, RSI_sell, balance, shares, hold.cash, 
         only.buy.shares, only.buy.balance) %>%
  mutate(date    = ymd(date)) %>%
   na.omit()

for (i in 2:nrow(dl)){
  # i = 15
  percent = .1
  
  # every day we get $200 added to our hold cash
  dl$hold.cash[i] <- add_amount(dl$hold.cash[i-1] , 200)
  dl$hold.cash[i]
  
  # every day we get $200 added to the balance
  dl$balance[i] <- add_amount(dl$balance[i-1] , 200)
  dl$balance[i]

  # every day we get $200 added to the only buy balance
  dl$only.buy.balance[i] <- add_amount(dl$only.buy.balance[i-1] , 200)
  dl$only.buy.balance[i]

  # every day we buy if able
  if (dl$only.buy.balance[i] > dl$price[i])
    dl$only.buy.shares[i] <- buy_shares(shares = dl$only.buy.shares[i-1],
                               balance = dl$only.buy.balance[i],
                               price = dl$price[i],
                               buy_percent = .95) else dl$only.buy.shares[i] <- dl$only.buy.shares[i-1]

  if (dl$only.buy.balance[i] > dl$price[i])
    dl$only.buy.balance[i] <- balance_after_buy(shares  = dl$only.buy.shares[i],
                                       balance = dl$only.buy.balance[i],
                                       price   = dl$price[i],
                                       buy_percent = .95)
  
  
  # do we buy anything today?
  if (dl$RSI_buy[i] > 0 && dl$balance[i] > dl$price[i]) 
      dl$shares[i] <- buy_shares(shares = dl$shares[i-1], 
                                balance = dl$balance[i], 
                                price = dl$price[i],
                                buy_percent = .5) else dl$shares[i] <- dl$shares[i-1]

    if (dl$RSI_buy[i] > 0 && dl$balance[i] > dl$price[i])
       dl$balance[i] <- balance_after_buy(shares  = dl$shares[i],
                                          balance = dl$balance[i],
                                          price   = dl$price[i], 
                                          buy_percent = .5)# else(dl$balance <- dl$balance[i])
  
  # do we sell anything today?
  if (dl$RSI_sell[i] > 0)
    dl$shares[i] <- sell_shares(shares       = dl$shares[i],
                                balance      = dl$balance[i],
                                price        = dl$price[i],
                                sell_percent = .5) else dl$shares[i] <- dl$shares[i-1]

  if (dl$RSI_sell[i] > 0)
    dl$balance[i] <- balance_after_sale(shares = dl$shares[i],
                                        balance = dl$balance[i-1],
                                        price = dl$price[i],
                                        sell_percent = .5) else(dl$balance[i-1])
}


dl <- mutate(dl, only.buy.share.value = only.buy.shares * price) %>%
  mutate(buy.sell.value = balance + price * shares)

head(dl, n=20)


ggplot(dl, aes(x = date, y = hold.cash)) + geom_line( color = "blue") +
  geom_line(aes( x = date, y = only.buy.share.value), color = "red") +
  geom_line(aes( x = date, y = buy.sell.value), color = "green")






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
