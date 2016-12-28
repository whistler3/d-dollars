# learning about many models

library(modelr)
library(tidyverse)

library(gapminder)

gapminder

gapminder %>%
  ggplot(aes(year, lifeExp, group = country)) +
  geom_line(alpha = 1/3)

# fit a model with linear trend

nz <- filter(gapminder, country == "New Zealand")
nz %>%
  ggplot(aes(year, lifeExp)) + geom_line() +
  ggtitle("Full data = ")

# create the model
nz_mod <- lm(lifeExp ~ year, data = nz)
nz %>%
  add_predictions(nz_mod) %>%
  ggplot(aes(year, pred)) + geom_line() +
  ggtitle("Linear trend + ")


# add the residuals
nz %>%
  add_residuals(nz_mod) %>%
  ggplot(aes(year, resid)) + 
  geom_hline(yintercept = 0, color = "white", size = 3) +
  geom_line() +
  ggtitle("Remaining pattern")



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
symbols = c('AAPL','GOOG','EMAN')
dl2 <- get_stock_data(symbols, days = 120)

# add the rsi threshold and buy & sell signals
dl2 <- get_stock_thresholds(dl2)
head(dl2)

#'### ------------------------------------------------------------------------
#' convert the stocks to a data frame, rename the columns, flatten and nest
nest_stocks <- function(dl2){

   dl <- dl2

  for( i in 1:length(dl)) {
    # i = 3
    
  # get the stock name
  stock_name <- names(dl[[i]]) %>%
    str_split( pattern = "\\.") %>% 
    unlist() %>%
    first()
  
  stock_name

  # convert each xts object in the list to a dataframe
  dl[[i]] <- as.data.frame(dl[[i]])
  
  # rename the columns for each dataframe
  colnames(dl[[i]]) <- c("open", "high", "low", "close", "volume", 
                         "adjusted","ema", "rsi_threshold", "rsi_sell", "rsi_buy")
  
  # take the dates that are in the row names and make them a column in the dataframe.
  # then add the stock name
  dl[[i]] <- dl[[i]] %>%
    mutate(date  = ymd(row.names(.))) %>%
    mutate(stock = stock_name)
  
  }
  # head(dl)
  
  # flatten the list of data frames into one dataframe

  dl_flat <- list.rbind(dl)
  
  dl_flat <- na.omit(dl_flat)
  head(dl_flat)

  # nest by each stock
  dl_n <- dl_flat %>%
    group_by(stock) %>%
    nest()

  return(dl_n)
}


dl3 <- nest_stocks(dl2)

dl3

  # Now that we have our nested data frame, we’re in a good position to fit some
  # models. We have a model-fitting function:
    
    country_model <- function(df) {
      lm(lifeExp ~ year, data = df)
    }
  # And we want to apply it to every data frame. The data frames are in a list,
  # so we can use purrr::map() to apply country_model to each element:
    
    models <- map(by_country$data, country_model)
  # However, rather than leaving the list of models as a free-floating object, I
  # think it’s better to store it as a column in the by_country data frame.
  # Storing related objects in columns is a key part of the value of data
  # frames, and why I think list-columns are such a good idea. In the course of
  # working with these countries, we are going to have lots of lists where we
  # have one element per country. So why not store them all together in one data
  # frame?
  # 
  # In other words, instead of creating a new object in the global environment,
  # we’re going to create a new variable in the by_country data frame. That’s a
  # job for dplyr::mutate():
    
    by_country <- by_country %>% 
    mutate(model = map(data, country_model))
  by_country
  
}

head(datalist)