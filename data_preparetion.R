# СберИндекс

library(tidyverse)

setwd('C:/Users/yadon/Skillbox/R/TrustUs_R/data/СберИндекс')


cash_of <- read.csv('Доля безналичных платежей в торговом обороте.csv', sep = ';')
index <- read_excel('Индекс потребительской активности.xlsx')
index$Дата <- as.character(index$Дата)
turist <- read.csv('Количество внутренних туристов.csv', sep = ';')

# функция для добавления столбца год/месяц

date_in_data <- function(data, year_month) {
  num <- c()
  for (i in data) {
    num <- c(num, as.integer(i[year_month]))
  }
  return(num)
}

cash_of$Год <- date_in_data(strsplit(cash_of$Дата, '-'), 1)
index$Год <- date_in_data(strsplit(index$Дата, '-'), 1)
turist$Год <- date_in_data(strsplit(turist$Дата, '-'), 1)
str(index)
str(cash_of)
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


cash_of$Месяц <- date_in_data(strsplit(cash_of$Дата, '-'), 2)
index$Месяц <- date_in_data(strsplit(index$Дата, '-'), 2)
turist$Месяц <- date_in_data(strsplit(turist$Дата, '-'), 2)

cash_of$Месяц <- month_data(cash_of$Месяц)
index$Месяц <- month_data(index$Месяц)
turist$Месяц <- month_data(turist$Месяц)

# отфильтруем датафраймы по 2020 году

cash_of <- cash_of[cash_of$Год == 2020, ]
index <- index[index$Год == 2020, ]
turist <- turist[turist$Год == 2020, ]

# групировка данных по региону, году и месяцу


test <- index
test$Значение <- as.numeric(test$Значение)


test <- test %>% group_by(Регион, Месяц, Год) %>% summarise(`Индекс.БП` = median(Значение))

test <- test %>% group_by(`Месяц`) %>% summarise(`Регион` = Регион,
                                                       `Год` = Год,
                                                       `Месяц` = Месяц,
                                                       `Индекс.БП` = median(Значение))

cash_of <- cash_of %>% group_by(Регион) %>% summarise(`Регион` = Регион,
                                                                  `Год` = Год,
                                                                  `Месяц` = Месяц, 
                                                                  `Индекс.БП` = Значение)
                                                                    

index <- index %>% group_by(Регион) %>% summarise(`Регион` = Регион,
                                                  `Год` = median(Год),
                                                  `Индекс.ПА` = median(Значение))

turist <- turist %>% group_by(Регион) %>% summarise(`Регион` = Регион,
                                                    `Год` = Год,
                                                    `Месяц` = Месяц,
                                                    `Количество.ВТ` = Значение)


sum(!complete.cases(cash_of))
sum(!complete.cases(index))
sum(!complete.cases(turist))

# ДомКлик

path_ <- 'C:/Users/yadon/Skillbox/R/TrustUs_R/data/ДомКлик/Рейтинг регионов по количеству заявок на кредит/'

list_file <- list.files('C:/Users/yadon/Skillbox/R/TrustUs_R/data/ДомКлик/Рейтинг регионов по количеству заявок на кредит')
my_month <- c()
month_list <- strsplit(list_file, ' ')
for (i in month_list) {
  my_month <- c(my_month, i[1])
}

library(readxl)


file_name <- c()
for (i in list_file) {
  file_name <- c(file_name, paste(path_, i, sep = ''))
}

install.packages('readr')
library(readr)

df <- list.files('C:/Users/yadon/Skillbox/R/TrustUs_R/data/ДомКлик/Рейтинг регионов по количеству заявок на кредит') %>% 
  lapply(read_excel) %>% 
  bind_rows

list_df <- lapply(file_name, read_excel)
