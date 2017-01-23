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

#'### ------------------------------------------------------------------------
#'#### read in all xlsb files in a directory and save them as csv to the data
#'directory
#'
#' library(excel.link)
#' #'
#' convert_xlsb_to_csv <- function(dir_in = "./data-in", dir_out = "./data"){
#'
#'   # dir_in = "./data-in"
#'   # dir_out = "./data"
#'
#'   # get all the file names of all the files in a directory
#'   filenames <- list.files(dir_in)
#'
#'   # keep only the files with a .xlsb extension
#'   filenames <- filenames[grep("[.]xlsb", filenames)]
#'
#'   # strip off the file extension
#'   data_names <- gsub("[.]xlsb", "", filenames)
#'
#'   # create the file path from the filenames and the data input directory
#'   file_path = file.path(dir_in, filenames)
#'
#'   # read in the files and assign each file one of the data_names
#'   for(i in 1:length(filenames))
#'     assign(data_names[i],
#'             xl.read.file(filename = file.path(dir_in, filenames[i]),
#'                          header = TRUE,
#'                          top.left.cell = "A1"))
#'
#'   # append .csv to each data names
#'   data_names_csv <- paste0(data_names, ".csv")
#'
#'   # create the file path from the filenames and the output data dir
#'   file_path = file.path(dir_out, data_names_csv)
#'
#'   # get the data file associated with each data_name and write it as a csv
#'   for(i in 1:length(data_names))
#'      write_csv(x = get(data_names[i]), path = file_path[i])
#'
#' }
  #


# convert_xlsb_to_csv()
