# learning about many models -----

# library(modelr)
# library(tidyverse)
# library(gapminder)

# gapminder
# 
# gapminder %>%
#   ggplot(aes(year, lifeExp, group = country)) +
#   geom_line(alpha = 1/3)
# 
# # fit a model with linear trend
# 
# nz <- filter(gapminder, country == "New Zealand")
# nz %>%
#   ggplot(aes(year, lifeExp)) + geom_line() +
#   ggtitle("Full data = ")
# 
# # create the model
# nz_mod <- lm(lifeExp ~ year, data = nz)
# nz %>%
#   add_predictions(nz_mod) %>%
#   ggplot(aes(year, pred)) + geom_line() +
#   ggtitle("Linear trend + ")
# 
# 
# # add the residuals
# nz %>%
#   add_residuals(nz_mod) %>%
#   ggplot(aes(year, resid)) + 
#   geom_hline(yintercept = 0, color = "white", size = 3) +
#   geom_line() +
#   ggtitle("Remaining pattern")
# 

#-----
#'### ------------------------------------------------------------------------
# given the following stocks.
symbols = c('AAPL','GOOG','EMAN')

# get the stock data for the given list of stock symbols
stock_data <- f2_get_stock_data(symbols)
head(stock_data[1])

# get the rsi threshold for the given list of stock data
stock_rsi <- f3_get_stock_thresholds(stock_data, 
                                  periods    = 9, 
                                  sell_above = 61, 
                                  buy_below  = 35)
head(stock_rsi)

# get the list of stock symbols that are 'interesting' (meets rsi threshold)
symbols <- f4_get_interesting_symbols(stock_rsi)
symbols

# get the stock data for the stocks that meet the rsi threshold
symbols = c('AAPL','GOOG','EMAN')
dl2 <- f2_get_stock_data(symbols, days = 120)

# add the rsi threshold and buy & sell signals
dl2 <- f3_get_stock_thresholds(dl2)
head(dl2)

#' convert the stocks to a data frame, rename the columns, flatten and nest
by_stock <- f5_flatten_and_nest_by_stock(dl2)
head(by_stock)

# run the model
f6_buy_sell_hold_model <- function(dl = df){

  # dl = dl3
  
  dl <- dl %>%
    select( date, close, rsi_buy, rsi_sell) %>%
    mutate(cash.only.total    = 0,
           hold.stock.balance = 0,
           hold.stock.shares  = 0,
           hold.stock.total   = 0,
           investing.balance  = 0,
           investing.shares   = 0,
           investing.total    = 0) 
  
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
                                                 price = dl$close[i],
                                                 percent = .99,
                                                 transaction = "buy")
      dl$hold.stock.shares[i]
      
      dl$hold.stock.balance[i] <- transact_account(shares  = dl$hold.stock.shares[i-1],
                                                   balance = dl$cash.only.total[i],
                                                   price   = dl$close[i],
                                                   percent = .99,
                                                   transaction = "buy")
      dl$hold.stock.balance[i]
      
      #'### ----------------------------------
      # every day we get $200 added to the investing.balance
      dl$investing.balance[i] <- transact_account(balance = dl$investing.balance[i-1],
                                                  transaction = "deposit", amount = 200)
      # do we buy anything today?
      if (dl$rsi_buy[i] > 0 )
        dl$investing.shares[i] <- transact_shares(shares = dl$investing.shares[i-1],
                                                  balance = dl$investing.balance[i],
                                                  price = dl$close[i],
                                                  percent = percent,
                                                  transaction = "buy"
        ) else dl$investing.shares[i] <- dl$investing.shares[i-1]
      
      
      if (dl$rsi_buy[i] > 0 )
        dl$investing.balance[i] <- transact_account(shares  = dl$investing.shares[i],
                                                    balance = dl$investing.balance[i],
                                                    price   = dl$close[i],
                                                    percent = percent,
                                                    transaction = "buy")
      
      # do we sell anything today?
      if (dl$rsi_sell[i] > 0 )
        dl$investing.shares[i] <- transact_shares(shares = dl$investing.shares[i-1],
                                                  balance = dl$investing.balance[i],
                                                  price = dl$close[i],
                                                  percent = percent,
                                                  transaction = "sell"
        ) 
      
      if (dl$rsi_sell[i] > 0 ) 
        dl$investing.balance[i] <- transact_account(shares  = dl$investing.shares[i-1],
                                                    balance = dl$investing.balance[i],
                                                    price   = dl$close[i],
                                                    percent = percent,
                                                    transaction = "sell")
      
      dl <-      mutate(dl, 
                        investing.total  = close * investing.shares  + investing.balance,
                        hold.stock.total = close * hold.stock.shares + hold.stock.balance)
    }
    
  return(dl)
  
  }

# put the model into the by_stock data frame
by_stock <- by_stock %>%
  mutate(model = map(data, f6_buy_sell_hold_model))
by_stock$model[1]



#'### ------------------------------------------------------------------------
#' look at results
by_stock

stocks <- unnest(by_stock, model)


ggplot(stocks, aes(x = date)) + 
  geom_line(aes(y = cash.only.total), color = "blue") +
  geom_line(aes( x = date, y = hold.stock.total), color = "red") +
  geom_line(aes( x = date, y = investing.total), color = "green") +
  facet_wrap(~ stock) + 
  ggtitle(label = "total in dollars",
    subtitle = "red = stock, green = investing, blue = cash") 
  

#'### ------------------------------------------------------------------------
#' write to file
write.csv(stocks, file =  "./report/stocks.csv")

