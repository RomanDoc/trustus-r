setwd('/Users/yadonistroman/Documents/GitHub/trustus-r/data')
library(tidyverse)
library(readxl)

df <- read_excel('total.xlsx')
unique(df[, 'Регион'])

str(df)
df$Месяц <- factor(df$Месяц, levels = c('январь', 'февраль', 'март', 'апрель', 'май', 'июнь', 
                                           'июль', 'август', 'сентябрь', 'октябрь', 'ноябрь', 'декабрь'))

df[df$Регион == 'алтайский край', 2]


df_region <- df[df$Регион == 'алтайский край', ]

df_rus <- df %>% group_by(Месяц) %>% reframe(Регион = Регион)

df_test <- cbind.data.frame('Месяц' = factor(unique(df$Месяц), levels = c('январь', 'февраль', 'март', 'апрель', 'май', 'июнь',
                                                                          'июль', 'август', 'сентябрь', 'октябрь', 'ноябрь', 'декабрь')))


df_rus <- aggregate(cbind(`Число абонентов`, `Среднемесячная з.п.`, `Уровень безработицы`, 
                          Индекс.БП, Индекс.ПА, Количество.ВТ, `Онлайн-заявки`, `Офлайн-заявки`, 
                          `Доля сделок, первичка`, `Доля сделок, вторичка`)
                    ~ Месяц, data = df, FUN = mean)

str(df_rus)
