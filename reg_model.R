setwd('/Users/yadonistroman/Documents/GitHub/trustus-r/data')
library(tidyverse)
library(readxl)
df <- read_excel('final.xlsx')

df <- df %>% mutate(`Онлайн-заявки dummy` = ifelse(`Онлайн-заявки` > `Офлайн-заявки`, 1, 0))


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
