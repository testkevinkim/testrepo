

# basic class


library(methods)
'%nin%' =Negate('%in%')

basic = setRefClass("basic",
                    
                    fields=list(
                      libraryList="vector",
                      
                      logPath="character",
                      
                      sleepTimeSecond ="numeric",
                      
                      universeList = "vector",
                      
                      badStockUniverseList = "vector"
                    ),
                    methods = list(
                      # __init__
                      initialize = function(){
                        
                        # 1. at 7:00, update history, pamt screening
                        # 2. at 8:10, check market open
                        # 3. at 9:01, monitor and append then screening
                        # 4. at 10:00, add h10data, result analysis, email report
                        
                        .self$libraryList=c("dplyr","data.table","rvest","splitstackshape","anytime",
                                            "stringr","lubridate","jsonlite","readr","xml2","httr",
                                            "tidyverse","tableHTML","gmailr","zoo")
                        
                        .self$sleepTimeSecond = 10
                        .self$loadLibrary()
                        .self$logPath ="~/0.DEV/H.DataCapture/Upshoot/upshoot.log"
                        
                        cat("loadLibrary","\n","logWrite","\n","sleepBy","\n","curTime",
                            "\n","waitTill","\n","loadUniverse","\n","emailReport","\n","marketChekc","\n","dayDownload","\n","badStock","\n")
                      },
                      
                      loadLibrary = function(){
                        eval(parse(text = lapply(.self$libraryList, function(x) paste0("library(",x,")") )))
                      },
                      
                      logWrite = function(msg){
                        cat(paste(now("Asia/Seoul") %>% format(),":",msg), file = .self$logPath, append=T, sep="\n")
                      },
                      
                      sleepBy = function(){
                        Sys.sleep(.self$sleepTimeSecond)
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
                        
                        if(!returnData){
                          
                          .self$universeList = ticker_all$list
                          
                        } else{
                          
                          if(returnType =="list"){
                            ticker_all$list
                          }else{
                            ticker_all$df
                          }
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
                      dayDownload =function(selectTickerList=NULL){
                        
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
                        
                        if(is.null(selectTickerList)){
                          KR_MASTER_RUN <- .self$universeList
                        }else{
                          KR_MASTER_RUN <- selectTickerList
                        }
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
                      }
                      
                    ) # method end 
)# class end