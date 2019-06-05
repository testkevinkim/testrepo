

# report class


library(methods)
'%nin%' =Negate('%in%')

reporting = setRefClass("reporting",
                        
                        methods = list(
                          # __init__
                          initialize = function(){
                            
                            print("  reporting  class  init")
                            print(" called methods:  ploScreen, entryScreen ")
                            
                            
                          },
                          
                          
                          resultCal=function(entryBookDT, historyDT){
                            
                            ddata  = (historyDT)
       
                            setorderv(ddata,c("TICKER","DATE"),c(1,1))
                            
                            lagleadFunc=function(dataset,prefix="N",days=1,move="lead"){
                              
                              DT = (dataset)
                              shiftStr ="
                              cols=c('TICKER','LOW','OPEN')
                              days = _N_
                              anscols =paste0('_PREFIX_',days,cols)
                              _dataset_[,(anscols) := shift(.SD, days,NA,'_MOVE_'), .SDcols=cols]"
                              shiftStrExe = gsub("_N_",days, shiftStr)
                              shiftStrExe = gsub("_PREFIX_",prefix, shiftStrExe)
                              shiftStrExe = gsub("_dataset_","DT", shiftStrExe)
                              shiftStrExe = gsub("_MOVE_",move, shiftStrExe)
                              
                              eval(parse(text=shiftStrExe))
                              
                              return(DT)
                            }
                            
                            ddata = lagleadFunc(ddata, "N",1,"lead") %>% 
                              select(TICKER,DATE, N1OPEN, DCLOSE=CLOSE,  DLOW=LOW,  DVOLUME=VOLUME, DCLOSE=CLOSE, N1TICKER) %>%
                              as.data.table()
                            
                            joindata = merge(entryBookDT,  ddata, by=c("TICKER","DATE"))
                            
                            joindata = joindata[TICKER==N1TICKER][,NOGAIN:= N1OPEN/CLOSE-1,][,CGAIN:=DCLOSE/CLOSE-1,]
                            
                            joindata  #  return
                            
                          }
                          
                          
                        ) # method end 
)# class end