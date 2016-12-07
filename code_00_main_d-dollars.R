---
  title:"d-dollars"
author:"Cindy Semrau"
date:"`r Sys.Date()`"
output:rmarkdown::html_vignette
---
  # remove all from memory -----
rm(list=ls())

# fake comment to test commit
# change made using RStudio

### Load Libraries and functions
# options(width = 150)
# Load libraries -----
library(magrittr)
library(tidyverse)
# library(tibble)
library(lubridate)
library(stringr)
library(readr)
library(forcats)
# library(tidyr)
# library(knitr)
library(ggplot2)
# library(xtable)
# library(kable)
library(pander)
# library(dplyr)

# library(tables)
library(readxl)
# library(excel.link)

library(modelr)
library(broom)
library(purrr)
# library(AppliedPredictiveModeling)
# library(caret)
# library(rpart)
# library(partykit)
# library(e1071)
# library(corrplot)
library(NLP)
# library(slam)
library(readr)
library(tm)
library(data.table)
library(rlist)

library(tidyverse)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)

# library(dtplyr)
# source("code_00_functions.R")

#
rmarkdown::render("code_01.R",  output_dir = "./report")

# rmarkdown::render("code_02.R",  output_dir = "./report")
