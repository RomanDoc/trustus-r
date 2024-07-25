setwd('C:/Users/yadon/Skillbox/R/TrustUs_R/data')
library(tidyverse)
library(readxl)
df <- read_excel('total.xlsx')


df_for_hist <- df %>% group_by(Месяц) %>% summarise(Индекс.БП = round(mean(Индекс.БП), 1),
                                                    Индекс.ПА = round(mean(Индекс.ПА), 1),
                                                    Количество.ВТ = round(mean(Количество.ВТ), 1))

df_for_hist$Месяц <- factor(df_for_hist$Месяц, levels = c('январь', 'февраль', 'март', 'апрель', 'май', 'июнь', 
                                                          'июль', 'август', 'сентябрь', 'октябрь', 'ноябрь', 'декабрь'))

ggplot(data = df_for_hist, aes(x = Месяц, y = Индекс.БП)) + geom_col() + ylim(0, 100) 

ggplot(data = df_for_hist, aes(x = Месяц, y = Индекс.ПА)) + geom_col()

ggplot(data = df_for_hist, aes(x = Месяц, y = Количество.ВТ)) + geom_col()


three <- df %>% select(Индекс.БП, Индекс.ПА, Количество.ВТ)
BP <- ggplot(data = df, aes(y = Индекс.БП)) + geom_boxplot() + facet_wrap(~Месяц)
BP <- boxplot(df$Индекс.БП ~ df$Месяц)

df_err <- df[df$Индекс.БП %in% BP$out, ]
