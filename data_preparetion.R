# СберИндекс

library(tidyverse)
library(readxl)

setwd('C:/Users/yadon/Skillbox/R/TrustUs_R/data/СберИндекс')


cash_of <- read.csv('Доля безналичных платежей в торговом обороте.csv', sep = ';')
index <- read_excel('Индекс потребительской активности.xlsx')
index$Дата <- as.character(index$Дата)
turist <- read.csv('Количество внутренних туристов.csv', sep = ';')

# функция для добавления столбца год/месяц

cash_of <- separate(cash_of, col = 'Дата', into = c('Год','Месяц'), sep = '-')
cash_of$Год <- as.integer(cash_of$Год)
cash_of$Месяц <- as.integer(cash_of$Месяц)

index <- separate(index, col = 'Дата', into = c('Год','Месяц'), sep = '-')
index$Год <- as.integer(index$Год)
index$Месяц <- as.integer(index$Месяц)

turist <- separate(turist, col = 'Дата', into = c('Год','Месяц'), sep = '-')
turist$Год <- as.integer(turist$Год)
turist$Месяц <- as.integer(turist$Месяц)

# функция для добовления месяца строкой

month_data <- function(data) {
  month_v <- c('январь', 'февраль', 'март', 'апрель', 'май', 'июнь', 
               'июль', 'август', 'сентябрь', 'октябрь', 'ноябрь', 'декабрь')
  month_str <- c()
  for (i in data) {
    month_str <- c(month_str, month_v[as.integer(i)])
  }
  month_f <- factor(month_str, levels = month_v)
  return(month_f)
}

cash_of$Месяц <- month_data(cash_of$Месяц)
index$Месяц <- month_data(index$Месяц)
turist$Месяц <- month_data(turist$Месяц)

# отфильтруем датафраймы по 2020 году

cash_of <- cash_of[cash_of$Год == 2020, ]
index <- index[index$Год == 2020, ]
turist <- turist[turist$Год == 2020, ]

# групировка данных по региону, году и месяцу

cash_of <- cash_of %>% group_by(Регион, Год, Месяц) %>% summarise(`Индекс.БП` = median(Значение))

index$Значение <- as.numeric(index$Значение)
index <- index %>% group_by(Регион, Год, Месяц) %>% summarise(`Индекс.ПА` = median(Значение))

turist <- turist %>% group_by(Регион, Год, Месяц) %>% summarise(`Количество.ВТ` = median(Значение))


sum(!complete.cases(cash_of))
sum(!complete.cases(index))
sum(!complete.cases(turist))

# объединение в один датафрейм

df_cash_and_index <- full_join(cash_of, index, by = c('Регион', 'Год', 'Месяц'))
df_sberIndex <- full_join(df_cash_and_index, turist, by = c('Регион', 'Год', 'Месяц'))

View(df_sberIndex[!complete.cases(df_sberIndex), ])

# заполним пропуски значениями сверху
# из-за того, что пропущенные значения присутствуют группами по региону целиком
# заполнение пропущенных значений будем производить по среднему по всей России

df_rus <- df_sberIndex[df_sberIndex$Регион == 'Россия', ]

df_sberIndex <- df_sberIndex %>% mutate(Индекс.БП = ifelse(is.na(Индекс.БП), df_rus$Индекс.БП, Индекс.БП))
df_sberIndex <- df_sberIndex %>% mutate(Индекс.ПА = ifelse(is.na(Индекс.ПА), df_rus$Индекс.ПА, Индекс.ПА))
df_sberIndex <- df_sberIndex %>% mutate(Количество.ВТ = ifelse(is.na(Количество.ВТ), df_rus$Количество.ВТ, Количество.ВТ))

# удалим данные по всей России

df_sberIndex <- df_sberIndex[df_sberIndex$Регион != 'Россия', ]

# заменим значение Алтай на республику Алтай и Адыгея на республика Адыгея

df_sberIndex$Регион[df_sberIndex$Регион == 'Алтай'] <- 'Республика Алтай'
df_sberIndex$Регион[df_sberIndex$Регион == 'Адыгея'] <- 'Республика Адыгея'

# ДомКлик
setwd('C:/Users/yadon/Skillbox/R/TrustUs_R/data/ДомКлик/Рейтинг регионов по количеству заявок на кредит/')

avgyst <- read_excel('август 2020.xlsx')
avgyst$Месяц <- 'август'

path_ <- 'C:/Users/yadon/Skillbox/R/TrustUs_R/data/ДомКлик/Рейтинг регионов по количеству заявок на кредит/'

list_file <- list.files(path_)
my_month <- c()
month_list <- strsplit(list_file, ' ')
for (i in month_list) {
  my_month <- c(my_month, i[1])
}


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
df_full <- bind_rows(list_df)

# Росстат

setwd('C:/Users/yadon/Skillbox/R/Для финальной работы/Данные для финальной работы/Данные/Росстат')

salary <- read_excel('Заработная плата.xlsx', sheet = 2)
salary <- salary %>% select(1, 14:25)

