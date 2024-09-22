setwd('/Users/yadonistroman/Documents/GitHub/trustus-r/data')
library(tidyverse)
library(readxl)

df <- read_excel('total.xlsx')
unique(df[, 'Регион'])

str(df)
df$Месяц <- factor(df$Месяц, levels = c('январь', 'февраль', 'март', 'апрель', 'май', 'июнь', 
                                           'июль', 'август', 'сентябрь', 'октябрь', 'ноябрь', 'декабрь'))


df_rus <- aggregate(cbind(`Число абонентов`, `Среднемесячная з.п.`, `Уровень безработицы`, 
                          Индекс.БП, Индекс.ПА, Количество.ВТ, `Онлайн-заявки`, `Офлайн-заявки`, 
                          `Доля сделок, первичка`, `Доля сделок, вторичка`)
                    ~ Месяц, data = df, FUN = mean)

str(df_rus)

df_region <- df %>% select(Месяц, `Число абонентов`, `Среднемесячная з.п.`, `Уровень безработицы`, 
                           Индекс.БП, Индекс.ПА, Количество.ВТ, `Онлайн-заявки`, `Офлайн-заявки`, 
                           `Доля сделок, первичка`, `Доля сделок, вторичка`, Регион)
df_region <- as.data.frame(df_region)
df_region <- df_region[df_region$Регион == 'алтайский край', ]

x <- df_region$Месяц
y <- df_region[, 8]

plot(y, x, xlab = 'Месяц', ylab = 'Показатель')
lines(y, x)

ggplot(data = df_region, aes(x = x, y = y, group = 1, colour = 'pink')) + 
  geom_line(size = 2) +
  theme_bw()

str(df_region)
str((df_rus))

