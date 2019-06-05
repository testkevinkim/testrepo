

# Selection class


library(methods)
'%nin%' =Negate('%in%')

selection = setRefClass("selection",
             
                   methods = list(
                     # __init__
                     initialize = function(){
                       
                       print("  selection  class  init")
                       print(" called methods:  ploScreen, entryScreen ")

                       
                     },
                     
                     # output: data.table
                     ploScreen =  function(minPAMT=1,  inputDT){
                       
                       ddata  = as.data.table(inputDT)
                     
                       ddata = ddata[,DATERANKREVERSE:=frank(desc(DATE), ties.method = 'first'),by=TICKER]  %>% as.data.table()
                       
                       #  p1volume == 0 : excluded
                       pvolScreen  = ddata[DATERANKREVERSE==1
                                                 ][VOLUME  >0
                                                   ][,PAMT:=as.double(CLOSE)*as.double(VOLUME)/10^8,
                                                     ][PAMT >= minPAMT]
                       
                       pvolScreenList  =  pvolScreen$TICKER  %>%  unique()
                       
                       ddata = ddata[TICKER %in% pvolScreenList ][DATERANKREVERSE <= 20] %>%  as.data.table()
                       
                       setorderv(ddata,c("TICKER","DATE"),c(1,1))
                       
                       lagleadFunc=function(dataset,prefix="P",days=1,move="lag"){
                         
                         DT = dataset
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
                       
                       ddata = lagleadFunc(ddata, "P",1,"lag")
                       ddata = ddata[TICKER==P1TICKER][,PLO:=OPEN/P1LOW-1,]
                       
                       ddataagg = ddata[,.(PLOM=as.double(median(PLO,na.rm = T)), PLOCNT=.N),by=TICKER][PLOCNT >=10]
                       
                       ddataagg # return
                       
                     },
                     
                     entryScreen=function(inputDT, OCdown=-0.01, LCmax=0.1, entryCnt=1, preScreenMultiple=2,  existingList){
                       #  inputDT has  ticker, time, date, OHLCV, PLOM
                       realDT=  inputDT[TICKER %nin% existingList]
                       
                       initDT = realDT[CLOSE/OPEN-1 <=   OCdown][CLOSE/LOW-1 <= LCmax][,DOWNO:=LOW/OPEN-1,]
                       
                       preDT = initDT[,PLOMRANK:=frank(desc(PLOM),ties.method = 'first'),][PLOMRANK  <= entryCnt* preScreenMultiple ]
                       
                       resultDT = preDT[,DOWNORANK:=frank(DOWNO, ties.method = 'first'),][DOWNORANK <= entryCnt]
                       
                       resultDT # return
                       
                     }
                     
                     
                     
                   ) # method end 
)# class end