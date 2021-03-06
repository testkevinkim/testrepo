library(dplyr)
library(jsonlite)
library(data.table)

path = "~/cat.json"
catdf = fromJSON(path)

list = catdf %>% filter(bannerDedupeCategory=="Offline") %>% select("id") %>% unlist()

listConcat = paste0('$',paste(list,collapse = '$,$'),'$')

listConcat %>% head() # head of dataframe

length(listConcat) # this line for making conflict