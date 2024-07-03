# СберИндекс

library(tidyverse)
setwd('C:/Users/yadon/Skillbox/R/TrustUs_R/data/СберИндекс')


cash_of <- read.csv('Доля безналичных платежей в торговом обороте.csv', sep = ';')
index <- read.csv('Индекс потребительской активности.csv', sep = ';')
turist <- read.csv('Количество внутренних туристов.csv', sep = ';')

# функция для добавления столбца год/месяц

date_in_data <- function(data, year_month) {
  num <- c()
  for (i in data) {
    num <- c(num, as.integer(i[year_month]))
  }
  return(num)
}

# отфильтруем датафраймы по 2020 году

cash_of$Год <- date_in_data(strsplit(cash_of$Дата, '-'), 1)
turist$Год <- date_in_data(strsplit(turist$Дата, '-'), 1)

cash_of$Месяц <- date_in_data(strsplit(cash_of$Дата, '-'), 2)
turist$Месяц <- date_in_data(strsplit(turist$Дата, '-'), 2)

# функция для добовления месяца строкой

month_data <- function(data) {
  month_v <- c('январь', 'февраль', 'март', 'апрель', 'май', 'июнь', 
               'июль', 'август', 'сентябрь', 'октябрь', 'ноябрь', 'декабрь')
  month_str <- c()
  for (i in data) {
    month_str <- c(month_str, month_v[as.integer(i)])
  }
  return(month_str)
}

cash_of <- cash_of[cash_of$Год == 2020, ]


cash_of$Месяц <- month_data(cash_of$Месяц)
warnings()

sum(complete.cases(cash_of$Дата))



