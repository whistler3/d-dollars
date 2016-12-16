# ---
# title:"d-dollars"
# author:"Cindy Semrau"
# date:"`r Sys.Date()`"
# output:rmarkdown::html_vignette
# ---
  # remove all from memory -----
 rm(list=ls())


### Load Libraries and functions
# options(width = 150)
# Load libraries -----
# library(knitr)
# library(xtable)
# library(kable)
# library(tables)
# library(excel.link)
# library(AppliedPredictiveModeling)
# library(caret)
# library(rpart)
# library(partykit)
# library(e1071)
# library(corrplot)
# library(slam)
# library(rmarkdown)
library(plyr)
 # library(dtplyr)
library(data.table)
  
library(magrittr)
library(tidyverse)
library(lubridate)
library(stringr)
library(forcats)
library(modelr)
library(broom)
library(purrr)
library(pander)
library(readxl)
library(NLP)
library(tm)
library(rlist)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)

library(quantmod) # Financial library
  rm(list=ls())
  
source('~/RStudioWorkspace/d-dollars/code_01.R', echo=TRUE)

