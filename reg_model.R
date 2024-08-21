setwd('/Users/yadonistroman/Documents/GitHub/trustus-r/data')
library(tidyverse)
library(readxl)
library(stargazer)
df <- read_excel('total.xlsx')

# добавим столбец сезон

season_v <- rep(c('зима', 'весна', 'лето', 'осень'), each=3)
season_v <- season_v[2:12]
season_v <- c(season_v, season_v[1])
season_v <- rep(season_v, 83)

df <- df %>% add_column('Сезон' = season_v, .before = 'Квартал')
df$Сезон <- factor(df$Сезон, levels = c('зима', 'весна', 'лето', 'осень'))

# 1
str(df)

mod_reg_pa <- lm(data = df, Индекс.ПА ~ `Среднемесячная з.п.` + `Число абонентов` + `Уровень безработицы` + Сезон)
summary(mod_reg_pa)

mod_reg_bp <- lm(data = df, Индекс.БП ~ `Среднемесячная з.п.` + `Число абонентов` + `Уровень безработицы` + Сезон)
summary(mod_reg_bp)

# 2

df <- df %>% mutate(`Онлайн-заявки dummy` = ifelse(`Онлайн-заявки` > `Офлайн-заявки`, 1, 0))

# 3

df_ipot <- df %>% select(Регион, `Всего ипотечных сделок`)
df_ipot$`Всего ипотечных сделок` <- gsub(' ', '', df_ipot$`Всего ипотечных сделок`)
df_ipot$`Всего ипотечных сделок` <- gsub('>', '', df_ipot$`Всего ипотечных сделок`)
ipot_list <- strsplit(df_ipot$`Всего ипотечных сделок`, '-')

ipot_v <- c()
for (i in ipot_list) {
  ipot_v <- c(ipot_v, i[1])
}
df_ipot$`Всего ипотечных сделок` <- as.numeric(ipot_v)

df_ipot <- df_ipot %>% mutate(`Ипотечые сделки dummy` = ifelse(`Всего ипотечных сделок` >= 500, 1, 0))

df$`Ипотечые сделки dummy` <- df_ipot$`Ипотечые сделки dummy`

# 4

mod_lreg_online <- glm(data = df, `Онлайн-заявки dummy` ~ `Среднемесячная з.п.` + 
                         `Число абонентов` + 
                         `Уровень безработицы` +
                         Сезон, 
                       family = 'binomial')
summary(mod_lreg_online)
exp(coef(mod_lreg_online))

# 5

mod_lreg_ipot <- glm(data = df, `Ипотечые сделки dummy` ~ `Среднемесячная з.п.` + 
                         `Число абонентов` + 
                         `Уровень безработицы` +
                         Сезон, 
                       family = 'binomial')
summary(mod_lreg_ipot)
exp(coef(mod_lreg_ipot))

stargazer(mod_lreg_ipot)


small <- df[, c(6, 7, 8)]
cor(small)
fitted_v <- mod_lreg_online$fitted.values
print(fitted_v)
summary(fitted_v)
library(pROC)
roc_onlin <- roc(df$`Ипотечые сделки dummy` ~ mod_lreg_ipot$fitted.values)
roc_onlin$auc





library(lmtest)
library(sandwich)

testcoef_pa <- coeftest(mod_reg_pa, vcov = vcovHC(mod_reg_pa, type = 'HC0'))
confint(testcoef_pa)
coefci(mod_reg_pa)


# добавим в датафрейм данные с остатками и предсказанными значениями
df$fitted_pa <- mod_reg_pa$fitted.values
df$residuals_pa <- mod_reg_pa$residuals

df$fitted_bp <- mod_reg_bp$fitted.values
df$residuals_bp <- mod_reg_bp$residuals

ggplot(data = df, aes(x = fitted_pa, y = residuals_pa)) + 
  geom_point() + 
  geom_hline(yintercept = 0, color = 'red') +
  theme_bw() +
  ggtitle('Индекс покупательской активности') +
  ylab('Остатки') +
  xlab('Предсказанные значения')

ggplot(data = df, aes(x = fitted_bp, y = residuals_bp)) + 
  geom_point() + 
  geom_hline(yintercept = 0, color = 'red') +
  theme_bw() +
  ggtitle('Индекс безналичных платежей') +
  ylab('Остатки') +
  xlab('Предсказанные значения')

summary(mod_reg_pa)
coeftest(mod_reg_pa, vcov = vcovHC(mod_reg_pa, type = 'HC0'))

testcoef_pa

cbind(coeftest(mod_reg_pa), coefci(mod_reg_pa))
cbind(coeftest(mod_reg_pa), confint(mod_reg_pa))

install.packages("broom")
library(broom)

tidy(mod_reg_pa, conf.int = TRUE)




