

basePath ="~/Desktop/Delete/Kodex/"
source(paste0(basePath,"kodexPipeline.R"))
kodex = kodexBreak$new()

# parameters
# as class variable:
# 1. dayMultipleVal
# 2. daybookDF
# 3. universeList : 3 ETF tickers



# call this at 7:45 -> check today's market open
# wait till 09:00:30 -> load dayHisotry -> optimal multiple calculation -> save to file, save to dayMultipleVal
# exeCheck(multiple) -> while
#   realResult = realPipeline(proximity, dayMultiple)
#   if realResult is null -> pass
#   else
#     if dayBook is empty
#       dayBookAdd(realResult)
#     else if dayBook$TICKER[1] != realResult$TICKER[1]
#       dayBookCancel()
#       dayBookAdd(realResult)
#     else -> pass
#
#   exeResult = exeCheck(multiple)
#   Sys.sleep(15)


if(kodex$marketCheck()){
  # market open
  kodex$emailReport(subject_string = "today market open", 
                  namedListOfDataframe = list(UNIVERSE=data.frame(UNIVERSECNT= c("TODAYMARKET OPEN"),stringsAsFactors = F))
  )
  
  
  # today market open
  kodex$waitTill("09:00:30")
  
  cTime = kodex$curTime() 
  
  kodex$historyResult() # dowload 3 tickers history and save it
  
  kodex$dayMultipleVal = kodex$dayMultiple()[1] # today's optimal multiple
  kodex$PHLVAL = kodex$dayMultiple()[2]
  
  exeResult = kodex$exeCheck()  # init
  proximityVal = 0.002 # if proximity  is less  than 0.002 -> place  order
  sleepInterval = 15 #seconds
  
  while(is.null(exeResult) & cTime < "15:30:00"){
    # while check exeResult and currentTime
    
    realResult =  kodex$realPipeline(proximityVal) #  return  ticker or NULL
    
    if(!is.null(realResult)){
      
      kodex$dayBookAdd(realResult)
      
    }else if(realResult[1] !=  kodex$dayBookDF[1]){
      
      kodex$dayBookCancel() # remove record in kodex$dayBookDF
      kodex$dayBookAdd(realResult)
      
    }else{
      # pass
    }
    
    
    
    Sys.sleep(sleepInterval)
    cTime = kodex$curTime() 
    exeResult =  kodex$exeCheck()
  }
  
  kodex$waitTill("15:45:00")
  
  kodex$historyResult()
  exitDT = kodex$exitData()
  
  todayDate = kodex$newFeed()$DATE %>% max()
  
  if(  nrow(kodex$dayBookDF) ==0){
    # empty
    
  }else{
    # entry
    if(file.exists(kodex$orderBookPath)){
      orderBookDT =  readRDS(kodex$orderBookPath)
      orderBookDT = rbindlist(list(orderBookDT, kodex$dayBookDF %>% mutate(DATE=todayDate) %>% as.data.table()  ))
      saveRDS(orderBookDT, kodex$orderBookPath)
    }else{
      orderBookDT = kodex$dayBookDF %>% mutate(DATE=todayDate) %>% as.data.table()
      saveRDS(orderBookDT, kodex$orderBookPath)
    }
    
    # report
    resultDT  =  merge(exitDT,  orderBookDT,  by= c("TICKER","DATE") ) %>%  as.data.table()
    resultDT[,GAIN:=N1OPEN/PRICE-1,][,DIRECTION:=ifelse(TICKER==.self$universeList[2],1,0),]
    
    report = resultDT[,.(GM=mean(GAIN,na.rm = T),DIRECTIONM=mean(DIRECTION,na.rm=T), CNT=.N),by=DATE]
    
    if( nrow(report) >0 ){
      kodex$emailReport(subject_string  = "kosdaq break gain report",
                      namedListOfDataframe = list(DAILY=as.data.frame(resultDT), AGGRESULT=as.data.frame(report)))
    }else{
      kodex$emailReport(subject_string  = "kosdaq break gain report - no result",
                        namedListOfDataframe = list(NORESULT=data.frame(RESULT=c("NO RESULT"), stringsAsFactors = F)) )    }
    
    
  }
  
  
  
} else{
  # market open
  kodex$emailReport(subject_string = "today market closed", 
                    namedListOfDataframe = list(UNIVERSE=data.frame(UNIVERSECNT= c("TODAYMARKET CLOSE"),stringsAsFactors = F))
  )
}



