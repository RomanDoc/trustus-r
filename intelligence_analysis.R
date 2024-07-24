setwd('C:/Users/yadon/Skillbox/R/TrustUs_R/data')
library(tidyverse)
library(readxl)
df <- read_excel('total.xlsx')


df_for_hist <- df %>% group_by(Месяц) %>% summarise(Индекс.БП = round(mean(Индекс.БП), 1),
                                                    Индекс.ПА = round(mean(Индекс.ПА), 1),
                                                    Количество.ВТ = round(mean(Количество.ВТ), 1))

df_for_hist$Месяц <- factor(df_for_hist$Месяц, levels = c('январь', 'февраль', 'март', 'апрель', 'май', 'июнь', 
                                                          'июль', 'август', 'сентябрь', 'октябрь', 'ноябрь', 'декабрь'))

ggplot(data = df, aes(x = Месяц, y = Индекс.БП)) + geom_bar()

?group_by?group_bygeom_bar()
?geom_line
str(df_for_hist)

df_errors <- df[!complete.cases(df), ]

