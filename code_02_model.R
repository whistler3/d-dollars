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
dl2 <- f3_get_stock_thresholds(dl2, sell_above = 50, buy_below = 35)
head(dl2)

#' convert the stocks to a data frame, rename the columns, flatten and nest
by_stock <- f5_flatten_and_nest_by_stock(dl2)
head(by_stock)

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
  facet_wrap(~stock) + 
  ggtitle(label = "total in dollars",
    subtitle = "red = stock, green = investing, blue = cash") 
  

#'### ------------------------------------------------------------------------
#' write to file
write.csv(stocks, file =  "./report/stocks.csv")




#'### ------------------------------------------------------------------------
# attempt to implement a  grid search for the best rsi thresholds - long ways 
# to go. got a ROI!


# get the stock data for the stocks that meet the rsi threshold
symbols = c('AAPL','GOOG','EMAN')
dl2 <- f2_get_stock_data(symbols, days = 120)

# add the rsi threshold and buy & sell signals
dl2 <- f3_get_stock_thresholds(dl2, sell_above = 50, buy_below = 35)
head(dl2)

#' convert the stocks to a data frame, rename the columns, flatten and nest
by_stock <- f5_flatten_and_nest_by_stock(dl2)
head(by_stock)

# run the model and put the model into the by_stock data frame
by_stock <- by_stock %>%
  mutate(model = map(data, f6_buy_sell_hold_model))
by_stock$model[1]



# get roi
roi <- by_stock %>%
  unnest( model) %>%
  group_by(stock) %>%
  # arrange(date) %>%
  filter(row_number() == n()) %>%
  mutate(investing.roi  = 100*(investing.total  - cash.only.total)/cash.only.total) %>%
  mutate(hold.stock.roi = 100*(hold.stock.total - cash.only.total)/cash.only.total) %>%
  select(stock, investing.roi, hold.stock.roi)

roi


roi
