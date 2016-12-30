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

# run the model and put the model into the by_stock nest
by_stock <- by_stock %>%
  mutate(model = map(data, f6_buy_sell_hold_model))
by_stock$model[1]

# get return on investment 'roi' and put the roi into the by_stock nest
by_stock <- by_stock %>%
  mutate(roi = map(model, f7_get_roi))

unnest(by_stock, roi)


# want to generate a random set of rsi thresholds to use in the search
# (from the book: http://r4ds.had.co.nz/model-basics.html )

# 
# thresholds <- tibble(
#   sell_above = runif(n = 50, min = 10, max = 100),
#   buy_below  = runif(n = 50, min = 0,  max = 90)
# )
# thresholds
# 
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
