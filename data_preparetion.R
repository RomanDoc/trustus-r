# СберИндекс

library(tidyverse)
library(readxl)

setwd('C:/Users/yadon/Skillbox/R/TrustUs_R/data/СберИндекс')


cash_of <- read.csv('Доля безналичных платежей в торговом обороте.csv', sep = ';')
index <- read_excel('Индекс потребительской активности.xlsx')
index$Дата <- as.character(index$Дата)
turist <- read.csv('Количество внутренних туристов.csv', sep = ';')

# добовления столбца год/месяц

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

# добавления месяца строкой

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

# Рейтинг регионов по количеству заявок на кредит
# устанвливаем путь до папки с файлами

path_ <- 'C:/Users/yadon/Skillbox/R/TrustUs_R/data/ДомКлик/Рейтинг регионов по количеству заявок на кредит/'

load_xlsx <- function(name_file, name_month) {
  res <- read_excel(paste(path_, name_file, sep = ''))
  res <- res %>% mutate(`Год` = 2020, `Месяц` = name_month)
  return(res)
}

# закгружаем файлы по месяцам

august <- load_xlsx('август 2020.xlsx', 'август')
april <- load_xlsx('апрель 2020.xlsx', 'апрель')
december <- load_xlsx('декабрь 2020.xlsx', 'декабрь')
july <- load_xlsx('июль 2020.xlsx', 'июль')
june <- load_xlsx('июнь 2020.xlsx', 'июнь')
may <- load_xlsx('май 2020.xlsx', 'май')
march <- load_xlsx('март 2020.xlsx', 'март')
november <- load_xlsx('ноябрь 2020.xlsx', 'ноябрь')
october <- load_xlsx('октябрь 2020.xlsx', 'октябрь')
september <- load_xlsx('сентябрь 2020.xlsx', 'сентябрь')
february <- load_xlsx('февраль 2020.xlsx', 'февраль')
january <- load_xlsx('январь 2020.xlsx', 'январь')

# объединим в один датафрейм 

df_credit <- bind_rows(august, april, december, july, june, may, march, november, october, september, february, january)
df_credit <- df_credit %>% select('Регион', 'Месяц', 'Год', 'Всего одобренных заявок', 'Доля онлайн-заявок', 'Доля заявок в офисе банка')

# удалим пропущенные строки

df_credit <- df_credit[complete.cases(df_credit), ]

# переведм столбуц Месяц в факторный тип

month_v <- c('январь', 'февраль', 'март', 'апрель', 'май', 'июнь', 
             'июль', 'август', 'сентябрь', 'октябрь', 'ноябрь', 'декабрь')
df_credit$Месяц <- factor(df_credit$Месяц, levels = month_v)

# заполним пропуски значениями которые выше.
# в первую очередь заменяем все '-' на NA

df_credit$`Всего одобренных заявок` <- ifelse(df_credit$`Всего одобренных заявок` == '—', NA, df_credit$`Всего одобренных заявок`)
df_credit$`Доля онлайн-заявок` <- ifelse(df_credit$`Доля онлайн-заявок` == '—', NA, df_credit$`Доля онлайн-заявок`)
df_credit$`Доля заявок в офисе банка` <- ifelse(df_credit$`Доля заявок в офисе банка` == '—', NA, df_credit$`Доля заявок в офисе банка`)

# заменяем значения с на те которые сверху

df_credit <- df_credit %>% fill(`Всего одобренных заявок`, .direction = 'down')
df_credit <- df_credit %>% fill(`Доля онлайн-заявок`, .direction = 'down')
df_credit <- df_credit %>% fill(`Доля заявок в офисе банка`, .direction = 'down')

# убираем знак процента и переводим в числовой тип данных

df_credit$`Доля онлайн-заявок` <- as.numeric(gsub('%', '', df_credit$`Доля онлайн-заявок`)) / 100
df_credit$`Доля заявок в офисе банка` <- as.numeric(gsub('%', '', df_credit$`Доля заявок в офисе банка`)) / 100

# группируем данные

df_credit <- df_credit %>% group_by(Регион, Год, Месяц) %>% summarise(`Всего одобренных заявок` = `Всего одобренных заявок`,
                                                                          `Онлайн-заявки` = `Доля онлайн-заявок`,
                                                                          `Офлайн-заявки` = `Доля заявок в офисе банка`)

# Рейтинг регионов по количеству ипотечных сделок
# устанвливаем путь до папки с файлами

path_ <- 'C:/Users/yadon/Skillbox/R/TrustUs_R/data/ДомКлик/Рейтинг регионов по количеству ипотечных сделок/'

# закгружаем файлы по месяцам

august <- load_xlsx('август 2020.xlsx', 'август')
april <- load_xlsx('апрель 2020.xlsx', 'апрель')
december <- load_xlsx('декабрь 2020.xlsx', 'декабрь')
july <- load_xlsx('июль 2020.xlsx', 'июль')
june <- load_xlsx('июнь 2020.xlsx', 'июнь')
may <- load_xlsx('май 2020.xlsx', 'май')
march <- load_xlsx('март 2020.xlsx', 'март')
november <- load_xlsx('ноябрь 2020.xlsx', 'ноябрь')
october <- load_xlsx('октябрь 2020.xlsx', 'октябрь')
september <- load_xlsx('сентябрь 2020.xlsx', 'сентябрь')
february <- load_xlsx('февраль 2020.xlsx', 'февраль')
january <- load_xlsx('январь 2020.xlsx', 'январь')

# объединим в один датафрейм 

df_ipoteka <- bind_rows(august, april, december, july, june, may, march, november, october, september, february, january)
df_ipoteka <- df_ipoteka %>% select('Регион', 'Месяц', 'Год', 'Всего ипотечных сделок', 'Доля сделок, первичка', 'Доля сделок, вторичка')

# удалим пропущенные строки

df_ipoteka <- df_ipoteka[complete.cases(df_ipoteka), ]

# переведм столбуц Месяц в факторный тип

month_v <- c('январь', 'февраль', 'март', 'апрель', 'май', 'июнь', 
             'июль', 'август', 'сентябрь', 'октябрь', 'ноябрь', 'декабрь')
df_ipoteka$Месяц <- factor(df_ipoteka$Месяц, levels = month_v)

# заполним пропуски значениями которые выше.
# в первую очередь заменяем все '-' на NA

df_ipoteka$`Всего ипотечных сделок` <- ifelse(df_ipoteka$`Всего ипотечных сделок` == '—', NA, df_ipoteka$`Всего ипотечных сделок`)
df_ipoteka$`Доля сделок, первичка` <- ifelse(df_ipoteka$`Доля сделок, первичка` == '—', NA, df_ipoteka$`Доля сделок, первичка`)
df_ipoteka$`Доля сделок, вторичка` <- ifelse(df_ipoteka$`Доля сделок, вторичка` == '—', NA, df_ipoteka$`Доля сделок, вторичка`)

# заменяем значения с на те которые сверху

df_ipoteka <- df_ipoteka %>% fill(`Всего ипотечных сделок`, .direction = 'down')
df_ipoteka <- df_ipoteka %>% fill(`Доля сделок, первичка`, .direction = 'down')
df_ipoteka <- df_ipoteka %>% fill(`Доля сделок, вторичка`, .direction = 'down')

# убираем знак процента и переводим в числовой тип данных

df_ipoteka$`Доля сделок, первичка` <- as.numeric(gsub('%', '', df_ipoteka$`Доля сделок, первичка`)) / 100
df_ipoteka$`Доля сделок, вторичка` <- as.numeric(gsub('%', '', df_ipoteka$`Доля сделок, вторичка`)) / 100

# группируем данные

df_ipoteka <- df_ipoteka %>% group_by(Регион, Год, Месяц) %>% summarise(`Всего ипотечных сделок` = `Всего ипотечных сделок`,
                                                                        `Доля сделок, первичка` = `Доля сделок, первичка`,
                                                                        `Доля сделок, вторичка` = `Доля сделок, вторичка`)

# объединим два датафрейма

df_domclick <- full_join(df_credit, df_ipoteka, by = c('Регион', 'Год', 'Месяц'))

sum(!complete.cases(df_domclick))

# Росстат

setwd('C:/Users/yadon/Skillbox/R/Для финальной работы/Данные для финальной работы/Данные/Росстат')

salary <- read_excel('Заработная плата.xlsx', sheet = 2)
salary <- salary %>% select(1, 14:25)

# удалим ненужные строки
salary <- salary[-c(1, 2, 4, 23, 26, 36, 45, 53, 68, 71, 72, 77, 88), ]

# изменим некоторые названия субъектов РФ

salary[salary == 'Архангельская область без авт. округа.'] <- 'Архангельская область'
salary[salary == 'в том числе Ненецкий авт.округ'] <- 'Ненецкий Автономный округ'
salary[salary == 'Тюменская область без авт. округов'] <- 'Тюменская область'

# переименуем столбцы в датафрейме

names(salary) <- c('Регион', 'январь', 'февраль', 'март', 'апрель', 'май', 'июнь', 'июль', 'август', 'сентябрь', 'октябрь', 'ноябрь', 'декабрь')

# переведем структуру датафрейма в длинный формат

salary <- salary %>% gather('Месяц', 'Среднемесячная з.п.', 2:13)

# переведем заначения зарплаты в числовой формат и округлим

salary$`Среднемесячная з.п.` <- round(as.numeric(salary$`Среднемесячная з.п.`), 1)

# переведем столбец месяц в факторный тип данных и упорядочим его

salary$Месяц <- factor(salary$Месяц, levels = month_v)

# доьавим столбец с годом

salary$Год <- 2020

# сгрупируем датафрейм по региону, году и месяцу

salary <- salary %>% group_by(Регион, Год, Месяц) %>% summarise(`Среднемесячная з.п.` = `Среднемесячная з.п.`)
