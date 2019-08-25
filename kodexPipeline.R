

gc()

# kosdaq index break

# basepath= "~/0.DEV/H.DataCapture/kodexBreak/"

basepath= "~/Desktop/Delete/Kodex/"

source(paste0(basepath,"basic.R"))

basic = basic$new()

library(methods)

'%nin%' =Negate('%in%')

kodexBreak= setRefClass("kodexBreak",
                         
                         contains = "basic",
                         
                         fields=list(
                           
                           libraryList="vector", # load library
                           universeList ="vector", # 3 tickers
                           dayBookDF ="data.frame",
                           dayMultipleVal = "numeric",
                           PHLVAL =  "numeric",
                           
                           capital = "numeric",
                           basePath = "character",
                           dayMultiplePath="character", # daily entry multiple result saved
                           orderBookPath="character", # orderbook result saved
                           dayHistoryPath="character", # history saved for exit report built
                           dayBookPath="character" #day book result  saved
                           
                         ),
                         
                         methods = list(
                           
                           # __init__
                           initialize = function(){
                             
                             .self$libraryList=c("dplyr","data.table","rvest","splitstackshape","anytime",
                                                 "stringr","lubridate","jsonlite","readr","xml2","httr",
                                                 "tidyverse","tableHTML","gmailr","zoo")
                             
                             .self$loadUniverse()
                             
                             .self$dayBookDF = data.frame(NULL)
                             
                             .self$loadLibrary()
                             
                             .self$dayMultipleVal = 0
                             .self$PHLVAL=0
                             .self$capital =  1000000
                             
                             print(paste("current captial = ", .self$capital))
                             
                             .self$universeList =c("229200","233740","251340")
                              # reg, lev, inverse
                             
                             .self$basePath="~/0.DEV/H.DataCapture/kodexbreak/"
                             .self$logPath =paste0(.self$basePath,"kodexbreak.log")
                             
                             .self$dayMultiplePath=paste0(.self$basePath,"kodexdaymultiple.rds")
                             .self$dayBookPath=paste0(.self$basePath,"kodexdaybook.rds")
                             .self$orderBookPath = paste0(.self$basePath,"kodexorderbook.rds")
                             .self$dayHistoryPath=paste0(.self$basePath,"kodexhistory.rds")
                           },
                           
                           # output : data.table
                           wholeDayDownload =function(universeList, maxPage =1){
                             
                             #daum_day_f start
                             daum_day_f <- function(PAGEN=1,TICKER=NULL){
                               
                               if(Sys.info()[1]=="Linux"){
                                 
                               }else{
                                 original_locale <-Sys.getlocale()
                                 Sys.setlocale("LC_ALL", "English")
                               }
                               
                               daum_string <-paste('http://finance-service.daum.net/item/quote_yyyymmdd_sub.daum?page=',PAGEN,'&code=',TICKER,'&modify=1',sep="")
                               file<-read_html(daum_string)
                               tables<-html_nodes(file, "table")
                               table1 <- html_table(tables[1], fill = TRUE) %>% as.data.frame()
                               if(table1 %>% nrow() !=0){
                                 colnames(table1) <- c("DATE","OPEN","HIGH","LOW","CLOSE","TO_PCLOSE","TO_CLOSE_PERC","VOLUME")
                                 table1 <- table1 %>% select(DATE,OPEN,HIGH,LOW,CLOSE,VOLUME) %>% filter(DATE!="") %>%
                                   mutate(DATE=as.Date(gsub("\\.","\\-",paste("20",DATE,sep=""))),
                                          OPEN=as.numeric(gsub("\\,","", OPEN)),
                                          HIGH=as.numeric(gsub("\\,","", HIGH)),
                                          LOW=as.numeric(gsub("\\,","", LOW)),
                                          CLOSE=as.numeric(gsub("\\,","", CLOSE)),
                                          VOLUME=as.numeric(gsub("\\,","", VOLUME)),TICKER=TICKER)
                               } else {
                                 table1 =NULL
                               }
                               
                               return(table1)
                             } ## unit test pass
                             
                             #daum_day_f end
                             
                             start_time=Sys.time()
                             ALLDAUM_DAY_ADJ <- NULL # data.table
                             
                             TICKER_LIST = universeList
                             MAXPAGEN = maxPage
                             allcounter=0
                             
                             for(TICKER_CNT  in seq(1,length(TICKER_LIST)) ){
                               
                               TICKER_SELECT <- TICKER_LIST[TICKER_CNT]
                               
                               counter=1
                               runflag=1
                               
                               while(runflag ==1 & counter <= MAXPAGEN ){
                                 
                                 temp <- daum_day_f(PAGEN=counter,TICKER=TICKER_SELECT) 
                                 
                                 if(is.null(temp)){
                                   runflag=0
                                   Sys.sleep(1)
                                   
                                 } else{
                                   
                                   
                                   ALLDAUM_DAY_ADJ <- rbindlist(list(ALLDAUM_DAY_ADJ, temp ))
                                   
                                   counter=counter+1
                                   
                                 }
                               }
                               
                               allcounter=allcounter+1
                               
                               if(allcounter %% 100 ==1){
                                 cat(TICKER_CNT ," / TICKER= ",TICKER_SELECT," ",allcounter,"\n")
                               }
                               
                               
                             }
                             
                             ALLDAUM_DAY_ADJ <- ALLDAUM_DAY_ADJ %>% mutate(DATE=as.character(DATE)) %>% as.data.table
                             
                             cat("run time:", as.character(Sys.time() - start_time),"\n")
                             ALLDAUM_DAY_ADJ # return
                             
                           }, # for init data downlaoding - screening based on historical move
                           
                           
                           exitData = function(){
                             
                             dayhistory = readRDS(.self$dayHistoryPath) %>% as.data.table()                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
                             
                             cols=c("OPEN")
                             days = 1
                             anscols =paste0("N",days,cols)
                             dayhistory[,(anscols):=shift(.SD, days,NA,"lead"),.SDcols=cols]
                             
                             dayhistory = dayhistory %>% na.omit() %>% as.data.table()
                             
                             dayhistory # return
                           },
                           
                           # run this after open ASAP
                           dayMultiple = function(mulSeq=seq(0.2,0.8,0.1)){
                             
                             dayhistory = readRDS(.self$dayHistoryPath) %>% as.data.table()
                             
                             dayhistory = dayhistory[TICKER==.self$universeList[1]]
                             
                             dayhistory[,HL:=HIGH/LOW-1,] #HL
                             
                             setorderv(dayhistory, "DATE") # sort
                             
                             
                             cols=c("OPEN")
                             days = 1
                             anscols =paste0("N",days,cols)
                             dayhistory[,(anscols):=shift(.SD, days,NA,"lead"),.SDcols=cols]
                             
                             dayhistory = dayhistory %>% na.omit()
                             
                             
                             
                             cols=c("HL")
                             days = 1
                             anscols =paste0("P",days,cols)
                             dayhistory[,(anscols) := shift(.SD, days,NA,"lag"), .SDcols=cols]
                             
                             
                             
                             # function
                             localOpt = function(DT, atmul){
                               
                               temp =DT[,ENTRYUP:=OPEN*(1+P1HL*atmul),
                                        ][,ENTRYDOWN:=OPEN*(1-P1HL*atmul),
                                          ][,EXIT:=N1OPEN,
                                            ][,UPGAIN:=ifelse(HIGH>=ENTRYUP,EXIT/ENTRYUP-1,NA),
                                              ][,DOWNGAIN:=ifelse(LOW<=ENTRYDOWN,ENTRYDOWN/EXIT-1,NA),
                                                ][,GAIN:=ifelse(is.na(UPGAIN),DOWNGAIN,UPGAIN),]
                               gainresult = temp$GAIN %>% median(na.rm=T)
                               return(gainresult)
                             }
                             
                             
                             # optimization
                             
                             mulList = rep(NA, nrow(dayhistory))
                             
                             obsday=20
                             for(  j in seq(1, nrow(dayhistory)) ){
                               
                               if( j < obsday){
                                 #pass
                               }else{
                                 obs = kqsearch[(j-obsday+1):(j-1)]
                                 returnseq= mulseq %>% lapply(function(x) localOpt(obs,x)) %>% unlist()
                                 
                                 #print(mulseq[which.max(returnseq)])
                                 
                                 mulList[j] = mulseq[which.max(returnseq)]
                               }
                            
                             }
                             
                             optimalMultiple = mulList %>% tail(1) #make sure whether last one is the most recent  one
                              phl  =  dayhistory$P1HL  %>% tail(1)
                              
                             list(optimalMultiple,  phl)                            # return

                           },
                           
                           # exitResult as data.table saved
                           # >> callable
                           historyResult =function(){
                             allUniv = .self$universeList
                             allData = .self$wholeDayDownload(universeList = allUniv, maxPage = 1)
                             historyPath = .self$dayHistoryPath
                             saveRDS(allData, historyPath) # save as data.table
                             
                           },
                           
                           newFeed = function(){
                             selectUniv = .self$universeList
                             feedDT = .self$dayDownload(selectUniv)[VOLUME >0] %>% as.data.table()
                             feedDT # return DataTable
                           },
                           
                           realPipeline =function(proximity=0.02){
                             result = NULL
                             
                             if(.self$dayMultipleVal >0){
                               multiple  =  .self$dayMultipleVal
                               phl =.self$PHLVAL
                               input =  .self$newFeed()[TICKER==.self$universeList[1]][,OH:=HIGH/OPEN-1,
                                                                                     ][,OL:=abs(LOW/OPEN-1),
                                                                                       ][,UPDIFF:= multiple*phl -  OH,
                                                                                         ][,DOWNDIFF:= multiple*phl -  OL,
                                                                                           ][,DIRECTION:=ifelse(UPDIFF <= proximity & UPDIFF >=0,
                                                                                                                1, 
                                                                                                                ifelse(DOWNDIFF <= proximity & DOWNDIFF >=0, -1,0)),]
                               
                               if(input$DIRECTION[1] ==1){
                                 # ticker  = lev
                                 result = .self$universeList[2]
                                 
                               }else if(input$DIRECTION[1] == -1){
                                 # ticker = inverse
                                 result = .self$universeList[3]
                                 
                               }else{
                                 result = NULL
                               }
                               
                             }
                             
                             result # return
                           },
                           
                           
                           exeCheck =function(){
                             result  =  NULL
                             
                             
                             if(.self$dayMultipleVal >0){
                               multiple  =  .self$dayMultipleVal
                               phl =.self$PHLVAL
                               input =  .self$newFeed()[TICKER==.self$universeList[1]][,OH:=HIGH/OPEN-1,
                                                                                       ][,OL:=abs(LOW/OPEN-1),
                                                                                         ][,UPDIFF:=  multiple*phl -OH,
                                                                                           ][,DOWNDIFF:=  multiple*phl - OL,
                                                                                             ][,DIRECTION:=ifelse(UPDIFF <0,
                                                                                                                  1, 
                                                                                                                  ifelse(DOWNDIFF <= 0, -1,0)),]
                               
                               if(input$DIRECTION[1] ==1){
                                 # ticker  = lev
                                 result = .self$universeList[2]
                                 
                               }else if(input$DIRECTION[1] == -1){
                                 # ticker = inverse
                                 result = .self$universeList[3]
                                 
                               }else{
                                 result = NULL
                               }
                             }
                              result # return
                           },
                           
                           
                           dayBookAdd=function(ticker){
                             
                             capital = .self$capital
                             
                             current = .self$newFeed()[TICKER== ticker]$OPEN *
                               .self$dayMultipleVal *
                               .self$PHLVAL *
                               ifelse(ticker ==  .self$universeList[2],2,1)
                             
                             entryprice = floor(current/5)*5
                             
                             entryqty = floor( capital / entryprice )
                             
                             entryDF  =  data.frame(TICKER=ticker, PRICE=entryprice, QTY=entryqty,  stringsAsFactors = F)
                             
                             .self$dayBookDF = rbind(.self$dayBookDF, entryDF)
                             
                             print(.self$dayBookDF)
                             
                           },
                           
                           dayBookCancel=function(){
                             .self$dayBookDF = data.frame(NULL)
                             print(paste("current row  count :", nrow(.self$dayBookDF)))
                           }
                           
                           
                         ) # method end 
)# class end




