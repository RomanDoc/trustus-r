setwd('/Users/yadonistroman/Documents/GitHub/trustus-r/data')
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


BP <- ggplot(data = df, aes(y = Индекс.БП)) + geom_boxplot() + facet_wrap(~Месяц)
str(BP)
BP$layers$geom$extra_params
?geom_boxplot

extra <- function(data) {
  iqr <- IQR(data)
  q1 <- quantile(data, 0.25)
  q3 <- quantile(data, 0.75)
  lower_b <- q1 - 1.5 * iqr
  upper_b <- q3 + 1.5 * iqr
  return(c(lower_b, upper_b))
}

test_pa <- extra(df$Индекс.ПА)
df_out_index_pa <- df[df$Индекс.ПА < test_pa[1] | df$Индекс.ПА > test_pa[2], ]

test_bp <- extra(df$Индекс.БП)
df_out_index_bp <- df[df$Индекс.БП < test_bp[1] | df$Индекс.БП > test_bp[2], ]

test_vt <- extra(df$Количество.ВТ)
df_out_count_vt <- df[df$Количество.ВТ < test_vt[1] | df$Количество.ВТ > test_vt[2], ]

df_top_pa <- df %>% 
  group_by(Регион) %>% 
  summarise(`Индекс.ПА` = round(mean(Индекс.ПА), 1))

df_top_pa <- arrange(df_top_pa, desc(Индекс.ПА)) %>% top_n(30)

df_top_bp <- df %>% 
  group_by(Регион) %>% 
  summarise(`Индекс.БП` = round(mean(Индекс.БП), 1))

df_top_bp <- arrange(df_top_bp, desc(Индекс.БП)) %>% top_n(30)

df_top_bp_pa <- inner_join(df_top_bp, df_top_pa, by = 'Регион')

df_top <- df[df$Регион %in% df_top_bp_pa$Регион, ]

df_top <- df_top[df_top$Количество.ВТ > 0, ]

target_v <- c('калининградская область', 'камчатский край', 'кировская область', 'нижегородская область')
df_final <- df[df$Регион %in% target_v, ]

ggplot(data = df_final, aes(x = Количество.ВТ, y = Индекс.ПА)) + 
  geom_point() +
  theme_bw() +
  stat_ellipse(color = 'red') +
  labs(title = 'График зависмости пакутельской активности от\n объема безналичных платежей',
       x = 'Количество внутрених туристов',
       y = 'Индекс безналичных платежей (50%)')

cor.test(df_final$Количество.ВТ, df_final$Индекс.ПА)
