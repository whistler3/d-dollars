# Functions

# normalize_variable_names ------------------------------------------------
#' Normalize the names of a dataframe. Returns a normalized list of strings to be
#' used as new column names for your dataframe
#'
#'
#' @param A dataframe
#' @return A vector string list with normalized columnn names
#' @example
#'
# rm(df)
# names <- c("funky Na&me", "funk?NAME", "silly (be)", "GLOBAL ", "LOCAL.",
#            "8+4", "/whatis  is this", "model \nYear")
# df    <-  data.frame(matrix(rnorm(21), ncol=8))
# colnames(df) <- names
# names(df)
# colnames(df) <- normalize_variable_names(df)
# names(df)

library(stringr)
normalize_variable_names  <- function(df){
  
  new_names <- names(df)
  new_names <- str_trim(new_names)
  new_names <- gsub(pattern = " ",            replacement = ".", new_names)
  new_names <- gsub(pattern = "_",            replacement = ".", new_names)
  new_names <- gsub(pattern = "\\?",          replacement = "",  new_names)
  new_names <- gsub(pattern = "\\/",          replacement = ".", new_names)
  new_names <- gsub(pattern = "\\&",          replacement = "",  new_names)
  new_names <- gsub(pattern = "\\#",          replacement = "",  new_names)
  new_names <- gsub(pattern = "\\(",          replacement = ".", new_names)
  new_names <- gsub(pattern = "\\)",          replacement = ".", new_names)
  new_names <- gsub(pattern = "\\+",          replacement = ".", new_names)
  new_names <- gsub(pattern = "[.][.]",       replacement = ".", new_names)
  new_names <- gsub(pattern = "^/.",          replacement = "",  new_names)
  new_names <- gsub(pattern = "/.+$",         replacement = "",  new_names)
  new_names <- gsub(pattern = "\\.(?=\\.*$)", replacement = "",  new_names, perl = TRUE)
  new_names <- gsub(pattern = "\\\n",         replacement = "",  new_names)
  new_names <- gsub(pattern = "^\\.",         replacement = "",  new_names)
  new_names <- tolower(x = new_names)
  return(new_names)
}

#'### ------------------------------------------------------------------------
#'#### clean up a column that is supposed to be numeric, but imported with some
#'wonky characters. Cleans and returns as numeric.

 df <- data.frame(c1 = c("$4.00", "5", "6,00.01", "party4", "300000.00", "$-", "$-2"),
                  c2 = c("a", "b", "c",  "d", "e", "f", "g"))

clean_up_numeric <- function(col) {

col = str_trim(col)
col = gsub(pattern = "\\$", replacement = "", col)
col = gsub(pattern = "\\,",   replacement = "", col)
col = gsub(pattern = "[a-z]", replacement = "", col)
col = str_trim(col)
col = suppressWarnings( as.numeric(col) )

  return(col)
}

df$c1 <- clean_up_numeric(df$c1)
df$c1

#'### ------------------------------------------------------------------------
#' trim white space off of all character columns
trim_character_columns <- function(df){

  library(stringr)
  is_string       <- sapply(df, FUN = is.character)
  # is_string
  # glimpse(df)
  df[,is_string] <- sapply(df[,is_string], str_trim)
  return(df)
}
