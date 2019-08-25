

# alpha vantage - 5 per minute 500 per day
# build Canadian Universe top 500 amt
# run AV


library(rvest)
library(stringr)

canUniv = function(L){
    canUnivPath=paste0("http://eoddata.com/stocklist/TSX/",L,".htm")
    
    a = read_html(canUnivPath) %>% html_nodes("table")
    atbl = a[6] %>% html_table()
    
    table = atbl[[1]] %>% as.data.table()
    tableDT= table[,c("Code","Close","Volume")]
    colnames(tableDT) = c("TICKER","CLOSE","VOLUME")
    
    tableDT[,VOLUME:=gsub(",","",VOLUME) %>% as.numeric(),]
    tableDT # return
}

canUnivList = LETTERS %>% lapply(function(x) canUniv(x))
canUnivDT = rbindlist(canUnivList)

saveRDS(canUnivDT, "./Desktop/Delete/canadianUniverse.rds")

canUnivDT$DOT = str_count(canUnivDT$TICKER, "\\.")

canSelect =canUnivDT[DOT==0][CLOSE >1][,AMT:=as.double(CLOSE)*as.double(VOLUME),][AMT >100000]


library(quantmod)



qtDownload =function(ticker){
  print(ticker)
  qdata = as.data.table(getSymbols(Symbols=ticker,env=NULL,from="2010-01-01", auto.assign = F, src="yahoo"))
  qdata[,TICKER:=ticker,]
  colnames(qdata)=c("DATE","OPEN","HIGH","LOW","CLOSE","VOLUME","ADJC","TICKER")
  qdata # return
}

qtDownList = paste0(canSelect$TICKER,".TO") %>% lapply(function(x) tryCatch({qtDownload(x)}, error=function(e){cat("error ",conditionMessage(e),"\n")}) )

qtDownCanada = rbindlist(qtDownList)

saveRDS(qtDownCanada, "./Desktop/Delete/canadianhistory.rds")
