

# main

basePath = "~/Desktop/Delete/PROD_CLASS"

source(paste0(basePath,"/","Util.R"))
source(paste0(basePath,"/","Select.R"))
source(paste0(basePath,"/","Report.R"))

# init
util=util$new()
selection = selection$new()
reporting = reporting$new()

# flow logic
# 1. called  at 07:00 -> history download  ->  plo screening -> saved -> report building if(book exists) -> email, marketCheck
# 2. waitTill  09:01
# 3. screening start by every 5 minutes till 10:00, entrycnt=1 -> save and read
# 4. screening start by every  10 minutes till 11:00, entrycnt=1 -> save and read
# 5. waitTill 11:01 -> end

bookPath = paste0(basePath,"/","entrybook.rds")
historyPath  = paste0(basePath,"/","history.rds")
ploPath = paste0(basePath,"/","plo.rds")

util$logPathDefine(basePath, "rankbanklog") # define log file path

universeList = util$loadUniverse() # universe list  return

  util$logWrite(paste("total  universe size:", length(universeList)))

  
  
  
historyDT = util$wholeDayDownload(universeList,1) # past 30 days history

  util$logWrite(paste("total history size:", nrow(historyDT)))
  
saveRDS(historyDT, historyPath)

  util$logWrite("historyDT saved")
  
  
  
ploDT = selection$ploScreen(1, historyDT)
saveRDS(ploDT, ploPath)

  util$logWrite("plom calculated and saved")
  

if(file.exists(bookPath)){
  # book exists
  # report building and email
  bookDT =  readRDS(bookPath)
  gainResult = reporting$resultCal(bookDT, historyDT)
  
  if( nrow(gainResult) > 0){
    # report
    gainResultAgg = gainResult[,.(NOGM=as.double(mean(NOGAIN,na.rm = T)),
                                  CGM=as.double(mean(CGAIN.na.rm =T)),
                                  ENTRYCNT=.N),
                               by=DATE] %>% arrange(DATE)  %>% as.data.frame()
    
    gainResultAgg10 = gainResult[TIME <= "10:00:00"][,.(NOGM=as.double(mean(NOGAIN,na.rm = T)),
                                  CGM=as.double(mean(CGAIN.na.rm =T)),
                                  ENTRYCNT=.N),
                               by=DATE] %>% arrange(DATE)  %>% as.data.frame()
    
    gainResultAgg11 = gainResult[TIME <= "11:00:00"][,.(NOGM=as.double(mean(NOGAIN,na.rm = T)),
                                                           CGM=as.double(mean(CGAIN.na.rm =T)),
                                                           ENTRYCNT=.N),
                                                        by=DATE] %>% arrange(DATE)  %>% as.data.frame()
    
    util$emailReport("rank-driven entry based on PLOM and DOWNO",
                     list(AGGRESULT=gainResultAgg,  
                          ENTRYBEFORE10=gainResultAgg10,
                          ENTRYBEFORE11=gainResultAgg11,
                          SAMPLERESULT=as.data.frame(gainResult) %>% head(20)))
  }
  #  skip
  
}else{
  util$emailReport("no  existing book", list(NOBOOK=data.frame(NO="NOBOOK",stringsAsFactors = F)))
}
  
  

  
util$waitTill("08:30:00")

if(util$marketCheck()){
  util$logWrite("market open today")
  util$waitTill("09:03:00") # monitoring started from this time
  curtime = util$curTime()
  
  entryDT = data.table(NULL)
  
  ploUniverse = ploDT$TICKER %>% unique()
  
  
  existingEntry =c("000000")
  
  while(curtime  < "12:00:01"){
    
    realEntry = util$dayDownload(ploUniverse)
    
    combEntry  = merge(realEntry,  ploDT, by="TICKER")
    
    selectEntry = selection$entryScreen(combEntry,existingList = existingEntry)
    
    existingEntry =  c(existingEntry,  selectEntry$TICKER  %>% unique())
    
    if(file.exists(bookPath)){
      
      entryDT =  readRDS(bookPath)
      
    }
    
    entryDT = rbindlist(list(entryDT, selectEntry ))
    
    saveRDS(entryDT, bookPath)
    
    if(curtime <"10:00:00"){
      sleepSecond = 60*5
    }else{
      sleepSecond = 60*10
    }
    
    util$sleepBy(sleepSecond)
    
    curtime = util$curTime()
  }
  
  
  
}else{
  util$emailReport("today market closed", list(MARKET=data.frame(STATUS="CLOSED",stringsAsFactors = F)))
}
















