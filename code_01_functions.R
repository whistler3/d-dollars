
#### ------------------------------------------------------------------------
# get all of the energy stock symbols
f1_get_company_stock_names <- function(keep_sector = "Energy"){

  # read in the stock list table
  stocks <- read_csv(file = "./data/companylist.csv",
                 col_types = cols(
                                    Symbol    = col_character(),
                                    Name      = col_character(),
                                    LastSale  = col_number(),
                                    MarketCap = col_number(),
                                    `ADR TSO` = col_skip(),
                                    IPOyear   = col_skip(),
                                    Sector    = col_character(),
                                    Industry  = col_character(),
                                    `Summary Quote` = col_skip()),
                 trim_ws = TRUE, col_names = TRUE)

  # normalize the variable names
  colnames(stocks) <- normalize_variable_names(stocks)

  # group and arrange the stocks
  stocks %>%
    group_by(sector) %>%
    summarise(stocks_in_sector = n_distinct(symbol)) %>%
    arrange(desc(stocks_in_sector))

  # how many sectors in the industry?
  # sectors %>% summarize(sectors_in_industry = n_distinct(industry))

  keep_stock_subset <- stocks %>%
    filter(sector == keep_sector) %>%
    arrange(industry, symbol) %>%
    select(industry, symbol, name)

  # keep only the symbols
  symbols <- keep_stock_subset$symbol
}

# symbols <- f1_get_company_stock_names(keep_sector = "Energy")
# symbols




#### ------------------------------------------------------------------------
# returns a data list with containing the data from somes stocks of interest
# Get the stock data for the stocks that meet the rsi threshold
f2_get_stock_data <- function(symbols = c('AAPL','GOOG','EMAN' ),
                              from , to =  today()){
  # symbols = c('AAPL','GOOG','EMAN' )
  # from = today() - days(9)
  # to   = today()
  
  dl <- list()

   for (i in 1:length(symbols)) {

     symbols[i] -> symbol

     # specify the "from" date to desired start date
     tryit <- try(getSymbols(symbol,
                             from        = from,
                             to          = to,
                             src         = 'yahoo',
                             auto.assign = FALSE))

     if (inherits(tryit, "try-error")) {
       i <- i + 1}

     else {
       # Add stock data to list
       dl[[i]] <- tryit
       attr(dl[[i]], 'symbol') <- symbol
       attr
       rm(symbol)}
   }

  return(dl)
}

#### ------------------------------------------------------------------------
# convert the stocks to a data frame, rename the columns, flatten and nest
  f3_flatten_and_nest_by_stock <- function(dl){
       # dl <- stocks_data
      # filter out the stocks that have no data in them.
      dl <- Filter(length, dl)
  
     for (i in seq_along(dl)) {
            # i = 10
    
         # get the stock name
         stock_name <- names(dl[[i]]) %>%
             str_split( pattern = "\\.") %>%
             unlist() %>%
             first()
    
         # convert each xts object in the list to a dataframe
         dl[[i]] <- as.data.frame(dl[[i]])
    
    
         # rename the columns for each dataframe
          names(dl[[i]])
         colnames(dl[[i]]) <- c("open", "high", "low", "close", "volume",
                                                                "adjusted")
    
         # take the dates that are in the row names and make them a column in the
         # dataframe. then add the stock name
         dl[[i]] <- dl[[i]] %>%
             mutate(date  = ymd(row.names(.))) %>%
             mutate(stock = stock_name)
       }
     # flatten the list of data frames into one dataframe
     dl_flat <- list.rbind(dl)
     dl_flat <- na.omit(dl_flat)
     head(dl_flat)
  
     # add columns that we will need later to record our transactions
     dl_flat <- dl_flat %>%
         mutate(transaction = "hold",
                                shares = 0,
                                amount = 0,
                                balance = 0,
                                roi = 1)
  
     dl_flat <- dl_flat %>%
         select(
                  # open,
                  # high,
                  # low,
                  date,
                  close,
                  # volume,
                  # adjusted,
                  stock,
                  transaction,
                  shares,
                  amount,
                  balance,
                  roi
                  )
     # nest by each stock
     # each stock needs to have the same number of rows of data. Filtering by > 500
     # is a hack to filter out the stocks with too few data rows FIX
     dl_nested <- dl_flat %>%
         group_by(stock) %>%
          filter(n() > 500) %>%
         nest()
     # return(dl_flat)
     return(dl_nested)
   }


#### ------------------------------------------------------------------------
# keep only needed data columns and get rsi. filter out na's
 # if this function fails, check to make sure the data's are all the same length
 f4_get_rsi_thresholds <- function(df){
      periods = 14
  
     # keep only needed columns
     # df <- select(df, stock, close, date)
     df <- select(df, date, close, transaction, shares,  amount, balance, roi)
  
     # calculate the rsi
     rsi <- RSI(price = df$close, n = periods)
  
     # round to fewer signifigant digits
     rsi <- round(rsi, digits = 1)
  
     # turn the ve into a dataframe
     data <- data.frame(df, rsi)
  
     # omit the first days that have no rsi threshold
     data <- na.omit(data)
  
     return(data)
   }


 

## slice and nest the time series
#### ------------------------------------------------------------------------
# we have 2 years of data. I am going to cut it up into 12 time slices
 f5_get_rsi_and_slice_into_windows <- function( slices = 24, by = by_stock){
  
     # by = by_stock
  
     # Take the nested data and get all the rsi thresholds
     by_window_stock <- by %>%
         mutate(data = map(data, f4_get_rsi_thresholds))
  
     # cut up the data set into slices by date
     by_window_stock <- by_window_stock %>%
    
       # get the data out of it's nest
       unnest( data) %>%
    
       # group
       group_by(stock) %>%
    
       # cut the data into 24 'windows'
       mutate(window = ntile(date, n = 24)) %>%
    
       group_by(window, stock) %>%
    
       # renest by stock and window
       nest()
  
     return(by_window_stock)
   }

 # by_window_stock <- f5_get_rsi_and_slice_into_windows(slices = 24, by = by_stock)

#### ------------------------------------------------------------------------
# Create a grid of scenario arguments so we can find the max roi

 f6_create_grid_of_arguments <- function(percent = .1, threshold = 60){
  
     # create the buy argument sequence
     buy_seq  <- seq(20, threshold, length.out = 6) %>%  round(digits = 2)
  
     # create the sell argument sequence
     sell_seq <- seq(threshold + 1, 90, length.out = 6) %>%  round(digits = 2)
  
     buy_sell_percent <- percent # seq(.1, .5, length.out = 4) %>% round(digits = 2)
  
     # create the grid
     args <- expand.grid(buy  = buy_seq,
                                                    sell = sell_seq,
                                                    buy_sell_percent = buy_sell_percent)
     return(args)
   }
 # args <- f6_create_grid_of_arguments()
 # args[1,]
 # args
 

# by_window_stock
# 
# # unnest a window of data
# data <- by_window_stock %>% filter(window == 1) %>% unnest(data)
# data
# 
# # group by date, then order by date and rsi
# data <- data %>% group_by(date) %>% arrange(date, desc(rsi)) 

 
#### ------------------------------------------------------------------------
# not used yet
#' run the stock simulations using a grid of arguments and the data nested inside
#' by_stock
 get_last <- function(data, args){
   
     dat <- data
     mod <- tibble()
     mod2 <- tibble()
     
       for( j in 1:nrow(args)){
           mod <- f6_buy_sell_hold_model_last(data = dat, 
                                              buy  = args[j,]$buy,
                                              sell = args[j,]$sell,
                                              buy_sell_percent = args[j,]$buy_sell_percent)
           mod2 <- bind_rows(mod, mod2)
         }
     
       return(mod2)
     } 
 
   # get the grid of arguments
   # args <- get_grid()
   
   
#### ------------------------------------------------------------------------
   # not used yet
# take the stock 'data' that was nested inside 'by_stock' and send it to the 'get_last'
# function. get_last steps thru one stock at a time, using each row of argements and models 
# what would have happened if you had used that investing strategy.
     # by_stock_window1a <- by_stock_window1 %>%
     #   mutate(model = map(data,  get_last, args))
     # 
     #   by_stock_window1a
     # by_stock_window1a %>% unnest(model)
