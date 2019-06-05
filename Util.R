

# Util class


library(methods)
'%nin%' =Negate('%in%')

util = setRefClass("util",
                    
                    fields=list(
                      libraryList="vector",
                      
                      logPath="character"
                      
                    ),
                    methods = list(
                      # __init__
                      initialize = function(){
                        
                        
                        .self$libraryList=c("dplyr","data.table","rvest","splitstackshape","anytime",
                                            "stringr","lubridate","jsonlite","readr","xml2","httr",
                                            "tidyverse","tableHTML","gmailr","zoo")
                        
                        .self$loadLibrary()
                        
                        print("define log file through methods -> logPathDefine(basePath,  logname)  ")
                        
                        
                   },
                      
                      loadLibrary = function(){
                        eval(parse(text = lapply(.self$libraryList, function(x) paste0("library(",x,")") )))
                      },
                   
                      logPathDefine = function(basePath, logname){
                        .self$logPath = paste0(basePath,"/",logname,".log")
                      },
                      
                      logWrite = function(msg){
                        cat(paste(now("Asia/Seoul") %>% format(),":",msg), file = .self$logPath, append=T, sep="\n")
                      },
                      
                      sleepBy = function(sleepSeconds){
                        Sys.sleep(sleepSeconds)
                      },
                      
                      curTime = function(){
                        
                        now("Asia/Seoul") %>% format() %>% substring(12,19)
                        
                      },
                      
                      waitTill =function(wait_till_time){
                        
                        waittime_it = as.ITime(wait_till_time)
                        curtime_it = as.ITime(.self$curTime())
                        timediff_it = waittime_it - curtime_it
                        
                        time_diff_sec = hour(timediff_it)*3600 +
                          minute(timediff_it)*60 +
                          second(timediff_it)*1
                        
                        while(waittime_it  > curtime_it){
                          
                          if(time_diff_sec > 60){
                            Sys.sleep(60)
                          } else{
                            Sys.sleep(1)
                          }
                          waittime_it = as.ITime(wait_till_time)
                          curtime_it = as.ITime(.self$curTime())
                          timediff_it = waittime_it - curtime_it
                          
                          time_diff_sec = hour(timediff_it)*3600 +
                            minute(timediff_it)*60 +
                            second(timediff_it)*1
                          
                        }#while
                        print(paste("wait ends at", curtime_it))
                        
                        
                      },
                      
                      loadUniverse =function(returnTypeVal = "list", returnData=FALSE){
                        returnType = returnTypeVal
                        
                        all_q_string=paste0('http://finance-service.daum.net/quote/all.daum?type=U&stype=Q')
                        all_p_string=paste0('http://finance-service.daum.net/quote/all.daum?type=U&stype=P')
                        
                        ticker_extract_f <- function(url_string){
                          cont <- read_html(url_string)
                          
                          ticker_extract_result <- cont %>% html_nodes("table") %>% html_nodes("a") %>% html_attr("href") %>% 
                            lapply(function(x) ifelse(nchar(gsub('\\/item\\/main\\.daum\\?code\\=','',x)) > 7 | nchar(gsub('\\/item\\/main\\.daum\\?code\\=','',x)) < 6,
                                                      NA,
                                                      gsub('\\/item\\/main\\.daum\\?code\\=','',x) ) ) %>% unlist() %>% unique() %>% na.omit()
                          return(ticker_extract_result)
                        }
                        q=ticker_extract_f(all_q_string)
                        p=ticker_extract_f(all_p_string)
                        ticker_all = NULL
                        ticker_all$list <- c(q,p ) %>% unique()
                        ticker_all$df <- rbind(data.frame(TICKER=q, MTYPE="Q",stringsAsFactors = F),
                                               data.frame(TICKER=p, MTYPE="P",stringsAsFactors = F))
                        
                   
                          if(returnType =="list"){
                            ticker_all$list
                          }else{
                            ticker_all$df
                          }
                        
                        
                      },
                      
                      emailReport = function(subject_string, 
                                             namedListOfDataframe ){
                        # to_address, from_address : constant
                        # body : list of dataframe => while loop makes sequence of html
                        # no return, just email sent
                        library(tableHTML)
                        library(gmailr)
                        
                        summary_text <-subject_string
                        
                        html_body = ""
                        for( i in names(namedListOfDataframe)){
                          html_body = paste0(html_body,"<p>",i,"</p>", 
                                             eval(
                                               parse(
                                                 text=
                                                   paste0("tableHTML(namedListOfDataframe$",i,")")
                                               )
                                             )
                          )
                        }
                        
                        
                        mime() %>%
                          to("kevin.ky.kim@hotmail.com") %>%
                          from("kevin.keunyoung.kim@gmail.com") %>%
                          subject(summary_text) %>% 
                          html_body(html_body) %>% 
                          send_message()
                        
                        cat('gmail sent','\n')
                        
                      }, 
                      
                      # today market open? return Boolean
                      marketCheck = function(tickers = c("005930","000660","005380","005490","015760")){
                        data = .self$dayDownload(selectTickerList = tickers)
                        #print(data)
                        openStatusRef = ifelse(.self$curTime() < "09:00:00", "PREOPEN","OPEN")
                        
                        #print(paste("openStatusRef",openStatusRef))
                        #print(data[,.(STATUS=mean(ifelse(MKS==openStatusRef,1,0))),])
                        if(.self$curTime() < "09:00:00"){
                          statusBool = ifelse( nrow(data) == 0,TRUE,FALSE) # between 8:00 and 9:00 no data feed => 
                        }else{
                          statusBool = ifelse( data[,OPENV:=ifelse(MKS == openStatusRef,1,0),]$OPENV %>% mean(na.rm = T) > 0.5, TRUE,FALSE) # between 8:00 and 9:00 no data feed => 
                        }
                        
                        statusBool
                      },
                      
                      # naver real download
                      dayDownload =function(selectTickerList){
                        
                        #timefeed_f
                        timefeed_f <- function(tickerlist){
                          #tickerlist : not having 'A' and comma seperated long string
                          
                          minute_call_json <- read_lines(paste("http://polling.finance.naver.com/api/realtime.nhn?query=SERVICE_ITEM:",tickerlist,sep=""))
                          minute_call_json <- iconv(minute_call_json, 'UTF-8', 'ascii', sub='') 
                          
                          minute_call_df <- fromJSON(minute_call_json)$result$areas$datas[[1]] %>%
                            select(TICKER=cd, OPEN=ov,HIGH=hv,LOW=lv,CLOSE=nv,VOLUME=aq, MKS=ms,PCLOSE=pcv)
                          minute_call_date <- as.POSIXct(fromJSON(minute_call_json)$result$time/1000, origin="1970-01-01", tz="Asia/Seoul") %>% format("%Y-%m-%d") %>% as.character()
                          minute_call_time <- as.POSIXct(fromJSON(minute_call_json)$result$time/1000, origin="1970-01-01", tz="Asia/Seoul") %>% format("%H:%M:%S") %>% as.character()
                          
                          minute_call_df <-  minute_call_df %>% mutate(DATE=minute_call_date , TIME=minute_call_time) %>%
                            dplyr::select(TICKER,
                                          DATE,
                                          TIME,
                                          DLOW=LOW,
                                          DHIGH=HIGH,
                                          DOPEN=OPEN,
                                          CURP=CLOSE,
                                          PCLOSE,
                                          VOLUME,
                                          MKS)  
                          
                          # return data frame
                          return(minute_call_df)
                        } ## unit test pass
                        
                  
                        KR_MASTER_RUN <- selectTickerList
                        
                        # naver_real
                        
                        
                        naver_real <- NULL
                        
                        starttime=Sys.time()
                        
                        
                        while(length(KR_MASTER_RUN) >0 ){
                          tck <- KR_MASTER_RUN[1:500] %>% na.omit() # batch size = 500 tickers
                          tickerlist <- gsub(",NA","",paste(tck,collapse = ","))
                          
                          possibleError <- tryCatch(
                            temp_naver <- timefeed_f(tickerlist = tickerlist),
                            
                            
                            error=function(e) e
                          )
                          
                          if(!inherits(possibleError,"error")){
                            #print("no error")
                            naver_real <- rbindlist(list(naver_real, temp_naver))
                            KR_MASTER_RUN  <- setdiff(KR_MASTER_RUN  , tck)
                          } else{
                            print("error")
                            KR_MASTER_RUN  <- setdiff(KR_MASTER_RUN  , tck)
                          }
                          
                        }
                        
                        naver_real <- naver_real %>% 
                          dplyr::select(TICKER,DATE,TIME, OPEN=DOPEN,HIGH=DHIGH,LOW=DLOW,CLOSE=CURP,PCLOSE,VOLUME,MKS)
                        
                        cat("naver_real run time:",Sys.time() -starttime," run at \n") # run time
                        
                        naver_real[VOLUME >0] %>% as.data.table()
                        
                        
                      },
                      # bad stock screening
                      badStock = function(){
                        
                        start_time=Sys.time()
                        base_string = 'http://finance-service.daum.net/quote/attention.daum?stype=MARKET&page=PAGEN&type=FLAGTYPE&order=desc&col=pchgrate'
                        
                        flag=c("C","D","S",
                               "W","B","M","R")
                        market=c("P","Q")
                        page=c(1,2)
                        
                        
                        grid=expand.grid(FLAG=flag, MKT=market, PAGE=page)
                        
                        result = NULL
                        
                        for(i in seq(1,nrow(grid))){
                          run_string = gsub("MARKET",grid[i,"MKT"], base_string)
                          run_string = gsub("PAGEN",grid[i,"PAGE"], run_string)
                          run_string = gsub("FLAGTYPE",grid[i,"FLAG"], run_string)
                          
                          file<-read_html(run_string)
                          tables<-html_nodes(file, "td") %>% html_nodes( "a") %>% html_attr("href") %>% lapply(function(x) ifelse(substring(x,1,1) =="/",  
                                                                                                                                  gsub("\\/item\\/main\\.daum\\?code\\=","",x),NA)) %>% unlist() %>% na.omit() %>% as.vector()
                          if(length(tables)==0){
                            result=result
                          }else{
                            result = c(result, tables)
                          }
                          
                        }
                        
                        cat("run time:", as.character(Sys.time() - start_time),"\n")
                        
                        .self$badStockUniverseList = result
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
                     
                   } # for init data downlaoding - screening based on historical move
                   
                      
                    ) # method end 
)# class end