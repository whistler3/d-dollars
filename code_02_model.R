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
# # given the following stocks.
# symbols = c('AAPL','GOOG','EMAN')
# 
# # get the stock data for the given list of stock symbols
# stock_data <- f2_get_stock_data(symbols)
# head(stock_data[1])
# 
# # get the rsi threshold for the given list of stock data
# stock_rsi <- f3_get_stock_thresholds(stock_data,
#                                   periods    = 9,
#                                   sell_above = 61,
#                                   buy_below  = 35)
# head(stock_rsi)
# 
# # get the list of stock symbols that are 'interesting' (meets rsi threshold)
# symbols <- f4_get_interesting_symbols(stock_rsi)
# symbols
# 
# # get the stock data for the stocks that meet the rsi threshold
# symbols = c('AAPL','GOOG','EMAN')
# dl2 <- f2_get_stock_data(symbols, days = 120)
# 
# # add the rsi threshold and buy & sell signals
# dl2 <- f3_get_stock_thresholds(dl2, sell_above = 50, buy_below = 35)
# head(dl2)
# 
# #' convert the stocks to a data frame, rename the columns, flatten and nest
# by_stock <- f5_flatten_and_nest_by_stock(dl2)
# head(by_stock)
# 
# # put the model into the by_stock data frame
# by_stock <- by_stock %>%
#   mutate(model = map(data, f6_buy_sell_hold_model))
# by_stock$model[1]
# 
# 
# 
# #'### ------------------------------------------------------------------------
# #' look at results
# by_stock
# 
# stocks <- unnest(by_stock, model)
# 
# 
# ggplot(stocks, aes(x = date)) + 
#   geom_line(aes(y = cash.only.total), color = "blue") +
#   geom_line(aes( x = date, y = hold.stock.total), color = "red") +
#   geom_line(aes( x = date, y = investing.total), color = "green") +
#   facet_wrap(~stock) + 
#   ggtitle(label = "total in dollars",
#     subtitle = "red = stock, green = investing, blue = cash") 
#   
# 
# #'### ------------------------------------------------------------------------
# #' write to file
# write.csv(stocks, file =  "./report/stocks.csv")
# 
# 

#-----
#'### ------------------------------------------------------------------------
# attempt to implement a  grid search for the best rsi thresholds - long ways 
# to go. got a ROI!


# get the stock data for the stocks that meet the rsi threshold
symbols = c('AAPL','GOOG','EMAN')
dl2 <- f2_get_stock_data(symbols, days = 120)


#' convert the stocks to a data frame, rename the columns, flatten and nest
by_stock <- f5_flatten_and_nest_by_stock_new(dl2)

head(by_stock)
unnest(by_stock, data)

#' keep only needed data columns and get rsi. filter out na's
by_stock <- by_stock %>%
  mutate(data = map(data, get_rsi_thresholds)) 

#'### ------------------------------------------------------------------------
#' run the buy sell model
f6_buy_sell_hold_model_new <- function(df, buy = .2, 
                                       sell = .8, 
                                       buy_sell_percent = .2){
  # buy = .2
  # sell = .8 
  # buy_sell_percent = .2
  # dl <- by_stock$data[[1]]
  # dl
  
  # buy or sell based on threshold
  dl <- df %>%
    mutate(sell = ifelse(rsi > sell, 1, 0)) %>%
    mutate(buy  = ifelse(rsi < buy,  1, 0))
  
  dl <- dl %>%
    select(date, close, sell, buy) %>%
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
    # buy_sell_percent = .1
    #'### ----------------------------------
    dl$cash.only.total[i] <- update_balance(balance = dl$cash.only.total[i-1],
                                            transaction = "deposit", amount = 200)
    # dl$cash.only.total[i]

    #'### ----------------------------------
    #every day we get $200 added to the only buy balance
    dl$hold.stock.balance[i] <- update_balance(balance = dl$hold.stock.balance[i-1],
                                               transaction = "deposit", amount = 200)
    dl$hold.stock.balance[i]

    # every day we buy if able
    dl$hold.stock.shares[i] <- buy_sell_shares(shares = dl$hold.stock.shares[i-1],
                                               balance = dl$hold.stock.balance[i],
                                               price = dl$close[i],
                                               percent = 1,
                                               transaction = "buy")
    dl$hold.stock.shares[i]

    dl$hold.stock.balance[i] <- update_balance(shares  = dl$hold.stock.shares[i-1],
                                               balance = dl$cash.only.total[i],
                                               price   = dl$close[i],
                                               percent = 1,
                                               transaction = "buy")
    dl$hold.stock.balance[i]

    #'### ----------------------------------
    # every day we get $200 added to the investing.balance
    dl$investing.balance[i] <- update_balance(balance = dl$investing.balance[i-1],
                                              transaction = "deposit", amount = 200)
    # do we buy anything today?
    if (dl$buy[i] > 0 )
      dl$investing.shares[i] <- buy_sell_shares(shares = dl$investing.shares[i-1],
                                                balance = dl$investing.balance[i],
                                                price = dl$close[i],
                                                percent = buy_sell_percent,
                                                transaction = "buy"
      ) else dl$investing.shares[i] <- dl$investing.shares[i-1]


    if (dl$buy[i] > 0 )
      dl$investing.balance[i] <- update_balance(shares  = dl$investing.shares[i],
                                                balance = dl$investing.balance[i],
                                                price   = dl$close[i],
                                                percent = buy_sell_percent,
                                                transaction = "buy")

    # do we sell anything today?
    if (dl$sell[i] > 0 )
      dl$investing.shares[i] <- buy_sell_shares(shares = dl$investing.shares[i-1],
                                                balance = dl$investing.balance[i],
                                                price = dl$close[i],
                                                percent = buy_sell_percent,
                                                transaction = "sell")

    if (dl$sell[i] > 0 )
      dl$investing.balance[i] <- update_balance(shares  = dl$investing.shares[i-1],
                                                balance = dl$investing.balance[i],
                                                price   = dl$close[i],
                                                percent = buy_sell_percent,
                                                transaction = "sell")

  }

  # calculate roi return on investment
  dl <- dl %>%
    mutate(investing.total  = close * investing.shares  + investing.balance) %>%
    mutate(hold.stock.total = close * hold.stock.shares + hold.stock.balance) %>%
    mutate(investing.roi  = 100 * (investing.total  - cash.only.total)/cash.only.total) %>%
    mutate(hold.stock.roi = 100 * (hold.stock.total - cash.only.total)/cash.only.total)

  return(dl)
}

#'### ------------------------------------------------------------------------
# #

by_stock <- by_stock %>%
  mutate(model = map_dbl(data, .f = f6_buy_sell_hold_model_new, ... = args))

by_stock

by_stock %>%
  map_dbl(data, f6_buy_sell_hold_model_new)#, #, buy = .3, sell = .8, buy_sell_percent = .3)


args <- list(buy = .3, sell = .8, buy_sell_percent = .3)
args
by_stock %>%
  f6_buy_sell_hold_model_new(data[[]])


d2<-  map(by_stock$data, f6_buy_sell_hold_model_new())
  

# put the model into the by_stock data frame
by_stock <- by_stock %>%
  mutate(model = map(data, args, f6_buy_sell_hold_model_new))
by_stock

model <- unnest(by_stock, model)
model
write_csv(model, path = "./report/model.csv")



#   sell_above = runif(n = 2, min = 10, max = 100)
#   buy_below  = runif(n = 2, min = 0,  max = 90)
# thresholds <- data.frame(sell_above, buy_below)



sim1_dist <- function(a1, a2) {
  measure_distance(c(a1, a2), sim1)
}

models <- models %>% 
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))
models





# run the model and put the model into the by_stock nest
by_stock <- by_stock %>%
  mutate(model = map(data, f6_buy_sell_hold_model))
by_stock$model[1]

# get return on investment 'roi' and put the roi into the by_stock nest
by_stock <- by_stock %>%
  mutate(roi = map(model, f7_get_roi))

unnest(by_stock, roi)

  #  get the buy sell thresholds based on the rsi

  #   mutate(sell_above = ifelse(rsi > sell_above, 1, 0)) %>%
  #   mutate(buy_below  = ifelse(rsi < buy_below,  1, 0))

# get the stock data for the stocks that meet the rsi threshold
symbols = c('AAPL','GOOG','EMAN')
dl2 <- f2_get_stock_data(symbols, days = 120)

#' convert the stocks to a data frame, rename the columns, flatten and nest
by_stock <- f5_flatten_and_nest_by_stock_new(dl2)

# add the rsi threshold to each stock
by_stock <- by_stock %>%
  mutate(data = map(data, get_rsi_thresholds)) 

by_stock
by_stock$data

by_country <- by_country %>% 
  mutate(model = map(data, country_model))
by_country




#'### ------------------------------------------------------------------------
#

#'### ------------------------------------------------------------------------
# want to generate a random set of rsi thresholds to use in the search ------
# (from the book: http://r4ds.had.co.nz/model-basics.html )

# 
# thresholds <- tibble(
#   sell_above = runif(n = 50, min = 10, max = 100),
#   buy_below  = runif(n = 50, min = 0,  max = 90)
# )
# thresholds
# # 
# thresholds %>%
#   mutate(roi = purrr)
# 
# grid <- expand.grid(
#   a1 = seq(-5, 20, length = 25),
#   a2 = seq(1, 3, length = 25)
# ) %>% 
#   mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))
# 
# grid %>% 
#   ggplot(aes(a1, a2)) +
#   geom_point(data = filter(grid, rank(dist) <= 10), size = 4, colour = "red") +
#   geom_point(aes(colour = -dist)) 
# 
# 
# 
# 
# df <- tibble::data_frame(
#   x = sort(runif(100)),
#   y = 5 * x + 0.5 * x ^ 2 + 3 + rnorm(length(x))
# )
# plot(df)
# 
# m1 <- lm(y ~ x, data = df)
# str(ml)
# df %>% add_residuals(m1)