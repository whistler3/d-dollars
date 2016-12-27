#'### ------------------------------------------------------------------------
# given the following stocks..
symbols = c('AAPL','GOOG','EMAN')

# get the stock data for the given list of stock symbols
stock_data <- get_stock_data(symbols)
head(stock_data[1])

# get the rsi threshold for the given list of stock data
stock_rsi <- get_stock_thresholds(stock_data,   periods = 9, 
                                  sell_above = 61, 
                                  buy_below  = 35)
head(stock_rsi)

# get the list of stock symbols that are 'interesting' (meets rsi threshold)
symbols <- get_stocks_meeting_rsi_threshold(stock_rsi)
symbols

# get the stock data for the stocks that meet the rsi threshold
dl2 <- get_stock_data(symbols)

# add the rsi threshold and buy & sell signals
dl2 <- get_stock_thresholds(dl2)
head(dl2)

#'### ------------------------------------------------------------------------
# function to buy or sell stock shares
transact_shares <- function(shares, balance, price, percent, transaction = "buy/sell"){
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
# will be writing a for loop or equivalent to handle more than one stock
# set up to do the buy sell hold
i = 1

dl <- data.frame(dl2[i]) %>%
  mutate(date    = row.names(.)) %>%
  select(date, price = 4, RSI_buy, RSI_sell) %>%
  mutate(cash.only.total    = 0,
         hold.stock.balance = 0,
         hold.stock.shares  = 0,
         hold.stock.total   = 0,
         investing.balance  = 0,
         investing.shares   = 0,
         investing.total    = 0) %>%
  mutate(date    = ymd(date)) %>%
  na.omit()

head(dl)

#'### -------------------------------
# buy sell hold!!
for (i in 2:nrow(dl)){
  # i = 2
  percent = .1
  #'### ----------------------------------
  dl$cash.only.total[i] <- transact_account(balance = dl$cash.only.total[i-1],
                                      transaction = "deposit", amount = 200)
  # dl$cash.only.total[i]
  
  #'### ----------------------------------
  #every day we get $200 added to the only buy balance
  dl$hold.stock.balance[i] <- transact_account(balance = dl$hold.stock.balance[i-1],
                                             transaction = "deposit", amount = 200)
  dl$hold.stock.balance[i]

  # every day we buy if able
  dl$hold.stock.shares[i] <- transact_shares(shares = dl$hold.stock.shares[i-1],
                                             balance = dl$hold.stock.balance[i],
                                             price = dl$price[i],
                                             percent = .99,
                                             transaction = "buy")
  dl$hold.stock.shares[i]
  
  dl$hold.stock.balance[i] <- transact_account(shares  = dl$hold.stock.shares[i-1],
                                            balance = dl$cash.only.total[i],
                                            price   = dl$price[i],
                                            percent = .99,
                                            transaction = "buy")
  dl$hold.stock.balance[i]
                                             

  #'### ----------------------------------
  # every day we get $200 added to the investing.balance
  dl$investing.balance[i] <- transact_account(balance = dl$investing.balance[i-1],
                                          transaction = "deposit", amount = 200)
  # do we buy anything today?
  if (dl$RSI_buy[i] > 0 ) #&& dl$investing.balance[i] > dl$price[i])
      dl$investing.shares[i] <- transact_shares(shares = dl$investing.shares[i-1],
                                balance = dl$investing.balance[i],
                                price = dl$price[i],
                                percent = percent,
                                transaction = "buy"
                                ) else dl$investing.shares[i] <- dl$investing.shares[i-1]


    if (dl$RSI_buy[i] > 0 )#&& dl$investing.balance[i] > dl$price[i])
       dl$investing.balance[i] <- transact_account(shares  = dl$investing.shares[i],
                                               balance = dl$investing.balance[i],
                                               price   = dl$price[i],
                                               percent = percent,
                                               transaction = "buy")

  # do we sell anything today?
    if (dl$RSI_sell[i] > 0 ) #&& dl$investing.shares[i] > 0)
        dl$investing.shares[i] <- transact_shares(shares = dl$investing.shares[i-1],
                                        balance = dl$investing.balance[i],
                                        price = dl$price[i],
                                        percent = percent,
                                        transaction = "sell"
                                        ) #else dl$investing.shares[i] <- dl$investing.shares[i-1]

    if (dl$RSI_sell[i] > 0 ) #&& dl$investing.shares[i] > 0)
      dl$investing.balance[i] <- transact_account(shares  = dl$investing.shares[i-1],
                                              balance = dl$investing.balance[i],
                                              price   = dl$price[i],
                                              percent = percent,
                                              transaction = "sell")

dl <-      mutate(dl, 
                  investing.total  = price * investing.shares  + investing.balance,
                  hold.stock.total = price * hold.stock.shares + hold.stock.balance)
}

head(dl, n=20)
tail(dl, n=20)


#'### ------------------------------------------------------------------------
#' look at results
ggplot(dl, aes(x = date, y = cash.only.total)) + 
  geom_line( color = "blue", show.legend = TRUE) +
  geom_line(aes( x = date, y = hold.stock.total), color = "red") +
  geom_line(aes( x = date, y = investing.total), color = "green") 

#'### ------------------------------------------------------------------------
#' write to file
write.csv(dl, file =  "./report/ddollars.csv")




