setwd('/Users/yadonistroman/Documents/GitHub/trustus-r/data')
library(tidyverse)
library(readxl)

df <- read_excel('total.xlsx')
unique(df[, 'Регион'])

df[df$Регион == 'алтайский край', 2]
