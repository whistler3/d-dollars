---
title:"d-dollars"
author:"Cindy Semrau"
date:"`r Sys.Date()`"
output:rmarkdown::html_vignette
---

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

library(tidyverse)
library(lubridate)
library(stringr)
library(forcats)
library(modelr)
library(broom)
library(pander)
library(readxl)
library(NLP)
library(tm)
library(rlist)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(quantmod) # Financial library

# remove all from memory -----
rm(list=ls())



source("code_01.R")

