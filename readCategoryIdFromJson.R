library(dplyr)
library(jsonlite)
library(data.table)

path = "~/cat.json"
catdf = fromJSON(path)

list = catdf %>% filter(bannerDedupeCategory=="Offline") %>% select("id") %>% unlist()

listConcat = paste0('$',paste(list,collapse = '$,$'),'$')
