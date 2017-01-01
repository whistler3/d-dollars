### Load Libraries and functions
# options(width = 150)
# Load libraries -----
library(knitr)
library(rmarkdown)
library(tidyverse)
library(lubridate)
library(stringr)
# library(forcats)
library(modelr)
# library(broom)
library(pander)
library(readxl)
library(NLP)
library(tm)
library(rlist)
library(quantmod) # Financial library

# remove all from memory -----
rm(list=ls())

source("code_01_functions.R")



# source("code_02_model.R")

# takes the output created by the budget chart tool in code_02_forecast and
# manipulates it to create the data for the Tableau forecast variance tool
# rmarkdown::render("code_02_model.R",  output_dir = "./report")

#