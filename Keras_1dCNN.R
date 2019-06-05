

library(dplyr)
library(data.table)
library(keras)
library(abind)

basePath = "~/Desktop/Delete/"
dataPrep = function(){
  
  library(dplyr)
  library(data.table)
  library(lubridate)
  kpath = "~/Desktop/Delete/rstudio-export/KR_DAUM_DAY.RDS"
  dt =readRDS(kpath)
  setorderv(dt, c("TICKER","DATE"),c(1,1))
  
  # market
  basePath ="~/Desktop/Delete/"
  mkDT = fread(paste0(basePath, "^KS11.csv"))
  colnames(mkDT) =c("DATE","OPEN","HIGH","LOW","CLOSE","ADJCLOSE","VOLUME")
  mkDT[,DATE:=as.Date(DATE),]
  mkDT = mkDT %>% mutate_if(is.character, as.numeric) %>% as.data.table()
  
  dt = merge(dt, mkDT %>% select(DATE, ADJCLOSE) %>% as.data.table(),   by="DATE")
  # disparity  and facevalue 
  
  pcmean = function(DT, date){
    
    mDT = DT[VOLUME  >0][,.(PCM=mean(CLOSE),PCSD=sd(CLOSE)),by=TICKER][,DATE:=date,]
    mDT # return
    
  }
  
  dateSeq = dt$DATE %>% unique() %>% sort()
  dateSeqLen = dateSeq %>% length()
  
  allDT = data.table(NULL)
  
  for(i in seq(21, dateSeqLen)){
    minDate  = dateSeq[i-20]
    maxDate = dateSeq[i-1]
    
    temp = dt[DATE >= minDate][DATE <= maxDate]
    tempR = pcmean(temp, dateSeq[i])
    allDT = rbindlist(list(allDT, tempR))
    
    if(i %% 100 ==1){
      print(paste(i, dateSeq[i]))
    }
  }
  
  
  
  
  setorderv(dt, c("TICKER","DATE"),c(1,1))
  
  for(i in c(1,21)){
    shiftStr ="
    cols=c('TICKER','CLOSE','ADJCLOSE')
    days = _N_
    anscols =paste0('_PREFIX_',days,cols)
    _dataset_[,(anscols) := shift(.SD, days,NA,'_MOVE_'), .SDcols=cols]"
    shiftStrExe = gsub("_N_",i, shiftStr)
    shiftStrExe = gsub("_PREFIX_","P", shiftStrExe)
    shiftStrExe = gsub("_dataset_","dt", shiftStrExe)
    shiftStrExe = gsub("_MOVE_","lag", shiftStrExe)
    
    eval(parse(text=shiftStrExe))
  }
  
  
  for(i in c(10)){
    shiftStr ="
    cols=c('TICKER','CLOSE','OPEN')
    days = _N_
    anscols =paste0('_PREFIX_',days,cols)
    _dataset_[,(anscols) := shift(.SD, days,NA,'_MOVE_'), .SDcols=cols]"
    shiftStrExe = gsub("_N_",i, shiftStr)
    shiftStrExe = gsub("_PREFIX_","N", shiftStrExe)
    shiftStrExe = gsub("_dataset_","dt", shiftStrExe)
    shiftStrExe = gsub("_MOVE_","lead", shiftStrExe)
    
    
    eval(parse(text=shiftStrExe))
    
  }
  
  dt=dt[TICKER==N10TICKER]
  dt = dt[TICKER == P21TICKER]
  
  
  
  comDT = merge(allDT, dt, by=c("TICKER","DATE"))[,AMT:=as.double(CLOSE)*as.double(VOLUME)/10^8,][VOLUME >0]
  
  comDT[,DISPARITY:=CLOSE/PCM-1,][,ENTRY:=CLOSE,][,EXIT:=N10OPEN][,GAIN:=EXIT/ENTRY-1,][,PCVOL:=PCSD/CLOSE,]
  
  comDTs = comDT[,DISPARITYRANK:=frank((DISPARITY),ties.method = 'first'),by=DATE]
  comDTsf = comDTs[,FACERANKPERC:=frank(CLOSE, ties.method = 'first')/.N,by=DATE
                   ][,OC:=CLOSE/OPEN-1,][,PCO:=OPEN/P1CLOSE-1,][,MKTPCC:=ADJCLOSE/P1ADJCLOSE-1,]
  
  comDTsf # return

}

dt = dataPrep()

dateSeq = dt$DATE %>% unique() %>% sort()
pastDays = 20
selectRank = 150

dtSub =copy(dt) %>% select(TICKER,DATE, VOLUME, CLOSE, OC, PCO, DISPARITY,MKTPCC) %>% as.data.table()

seriesDTpre = data.table(NULL)

for( i in seq(pastDays+1,length(dateSeq))){
  
  lastDT = dt[DATE == dateSeq[i]][DISPARITYRANK <= selectRank][,GAINRANKPERC:=frank(GAIN,ties.method = 'first')/.N,by=DATE] %>%
    select(TICKER, ENTRY,EXIT,GAIN,LASTDATE=DATE, LASTVOLUME=VOLUME,LASTCLOSE=CLOSE, LASTDISPARITYRANK=DISPARITYRANK,GAINRANKPERC) %>%
    as.data.table()
  
  pastDT = dtSub[DATE <= dateSeq[i] & DATE >= dateSeq[i-pastDays]]
  tempDT = merge(lastDT, pastDT, by="TICKER")[,DISPARITYRANKPERC:=frank(DISPARITY, ties.method = 'first')/.N,by=DATE
                                              ][,KEY:=paste0(TICKER,LASTDATE),]
  
  seriesDTpre = rbindlist(list(seriesDTpre, tempDT))
  
  if(i %% 100 == pastDays+1){
    print(paste(i, dateSeq[i]))
  }
}

# check full date series
seriesDTpreAgg = seriesDTpre[,.(CNT=.N),by=KEY]
goodKeyList = seriesDTpreAgg[CNT == pastDays+1]$KEY %>% unique()


seriesDTpre = seriesDTpre[KEY %in% goodKeyList]
seriesDTpre %>% dim()
saveRDS(seriesDTpre,paste0(basePath,"seriesDTre.rds"))

# data ops
# features: OC, PCO, VLM, disparityRank, disparityVal, mktPCC, facevalueRankPerd
# target: 0 or 1 (gain above median or not)

# flow:
# 1. disparity rank <= 100
# 2. n10gain rankPerc -> 0.5 threshold
# 3. past 20 days feature series

# data prep for keras
# data.table -> as.matrix()

trRatio= 0.7
trDateMax = dateSeq[floor(length(dateSeq)*trRatio)]
seriesDTpre[,CLOSERATIO:=CLOSE/LASTCLOSE-1,
         ][,VOLUMERATIO:=VOLUME/LASTVOLUME,
           ][,TARGET:=ifelse(GAINRANKPERC > 0.5,1,0),
             ]

seriesDT = copy(seriesDTpre)
trDT = seriesDT[LASTDATE <= trDateMax]
tsDT = seriesDT[LASTDATE > trDateMax]

featureCols = c("CLOSERATIO","VOLUMERATIO","OC","PCO","DISPARITY","DISPARITYRANKPERC","MKTPCC")
targetCols =c("TARGET")

reduceDF = function(DT, selectColList, filterCol){

 smallDT = DT[KEY==filterCol][,selectColList,with=FALSE] %>% data.matrix()
return(smallDT)

}

arrayOpsFeature = function(seriesDT, featureCols){
    seriesKey = seriesDT$KEY %>% unique()
    
    print(paste("seriesKey length:", length(seriesKey)))
    
    mxList = seriesKey %>% lapply(function(x) reduceDF(seriesDT, featureCols, x) )
    mxList21 = mxList[!is.na(mxList)]
    mxArray = abind(mxList21, along=3)
    mxArray = aperm(mxArray,c(3,1,2))
    
    print(paste("array dim:", dim(mxArray)[1]))
    mxArray # return
}

arrayOpsTarget = function(seriesDT){
  seriesKey = seriesDT$KEY %>% unique()
  print(paste("seriesKey length:", length(seriesKey)))
  
  mxArray = seriesDT %>% select(KEY,TARGET) %>% unique() %>% select(TARGET) %>% unlist() %>% array()

  print(paste("array dim:", dim(mxArray)[1]))
  mxArray # return
}

tr_x = arrayOpsFeature(trDT, featureCols)
tr_y = arrayOpsTarget(trDT)

ts_x = arrayOpsFeature(tsDT, featureCols)
ts_y = arrayOpsTarget(tsDT)

saveRDS(tr_x,paste0(basePath, "tr_x.rds"))
saveRDS(tr_y,paste0(basePath, "tr_y.rds"))
saveRDS(ts_x,paste0(basePath, "ts_x.rds"))
saveRDS(ts_y,paste0(basePath, "ts_y.rds"))




# 1D CNN

xtrain = array_reshape(tr_x, c(nrow(tr_x), 21,7))
ytrain = array_reshape(tr_y, c(nrow(tr_y)))

xtest = array_reshape(ts_x, c(nrow(ts_x), 21,7))
ytest = array_reshape(ts_y, c(nrow(ts_y)))


# parameters
filterSize = 40
filterSize2 =20
kernelSize = 2
maxpoolSize = 7L
model <- keras_model_sequential()

model %>% 
  layer_conv_1d( filterSize, kernelSize, padding="valid", activation = "relu", strides = 1L) %>%
  layer_max_pooling_1d(pool_size = maxpoolSize, strides = 1L) %>%
  layer_conv_1d( filterSize2, kernelSize, padding ="valid", activation = "relu", strides = 1L) %>%
  layer_global_average_pooling_1d() %>%
  layer_dropout(0.5) %>%
  layer_dense(units = 1L, activation = "sigmoid")

# compile model
model %>% compile(
  loss = "binary_crossentropy",
  optimizer = "adam",
  metrics = "accuracy"
)


model %>%
  fit(
    xtrain, ytrain,
    batch_size = 5000,
    epochs = 50,
    validation_data = list(xtest, ytest),
    view_metrics="auto"
  )



pred = predict(model, xtest, verbose = 1)
predMap = data.table(KEY = tsDT$KEY %>% unique(), SCORE=pred, stringsAsFactors = F)

predDT = merge(tsDT, predMap, by="KEY")[!is.nan(SCORE.V1)]

highscoreDT = predDT[DATE==LASTDATE
                     ][,AMT:=as.double(CLOSE)*as.double(VOLUME)/10^8,
                       ][,SCORERANKPERC:=frank(desc(SCORE.V1),ties.method = 'first'),by=LASTDATE
                         ][SCORERANKPERC <=5][AMT >1][,.(GM=mean(GAIN),AMTM=median(AMT),CNT=.N),by=DATE] %>% arrange(DATE) %>% as.data.table()
highscoreDT %>% summary


barplot(highscoreDT$GM )
barplot(highscoreDT$GM %>% sort())



highscoreDT$CR=1
fee = 0.003+0.015/100*2+0.01

for(i in seq(2, nrow(highscoreDT))){
  highscoreDT[i,"CR"] =( 1+ (highscoreDT[i,"GM"]-fee)/10)*highscoreDT[i-1,"CR"]
}


barplot(highscoreDT$CR, log='y')
highscoreDT %>% tail()
highscoreDT %>% summary()
nrow(highscoreDT)





### save mode to file
model %>% save_model_hdf5(paste0(basePath,"disparity_1D_CNN_model.h5"))
model %>% summary


# CNN result :
# DATE                  GM                AMTM               CNT       
# Min.   :2016-07-11   Min.   :-0.17334   Min.   :  0.0068   Min.   :1.000  
# 1st Qu.:2016-12-08   1st Qu.:-0.01649   1st Qu.:  5.8486   1st Qu.:5.000  
# Median :2017-05-15   Median : 0.01530   Median : 10.8046   Median :5.000  
# Mean   :2017-05-14   Mean   : 0.02983   Mean   : 18.2347   Mean   :4.952  
# 3rd Qu.:2017-10-19   3rd Qu.: 0.06905   3rd Qu.: 20.8000   3rd Qu.:5.000  
# Max.   :2018-04-11   Max.   : 0.35438   Max.   :334.2838   Max.   :5.000  



# manual result : disparityrank *4 -> facevalrank*1 -> pcvolrankperc < 0.3
# DATE                  GM                CNT         AMTM        
# Min.   :2016-07-11   Min.   :-0.23427   Min.   :6   Min.   :  2.272  
# 1st Qu.:2016-12-15   1st Qu.:-0.03357   1st Qu.:6   1st Qu.:  9.209  
# Median :2017-05-26   Median : 0.01029   Median :6   Median : 13.280  
# Mean   :2017-05-26   Mean   : 0.02622   Mean   :6   Mean   : 17.594  
# 3rd Qu.:2017-11-06   3rd Qu.: 0.07364   3rd Qu.:6   3rd Qu.: 20.446  
# Max.   :2018-04-13   Max.   : 0.58297   Max.   :6   Max.   :157.419


# CNN tune result : more filters, kernelsize =2, epoch=50
# DATE                  GM                AMTM                CNT       
# Min.   :2016-07-11   Min.   :-0.16313   Min.   :  0.00685   Min.   :1.000  
# 1st Qu.:2016-11-27   1st Qu.:-0.01070   1st Qu.:  6.72580   1st Qu.:5.000  
# Median :2017-05-15   Median : 0.02685   Median : 11.66888   Median :5.000  
# Mean   :2017-05-14   Mean   : 0.03908   Mean   : 19.97580   Mean   :4.947  
# 3rd Qu.:2017-11-01   3rd Qu.: 0.07642   3rd Qu.: 22.38550   3rd Qu.:5.000  
# Max.   :2018-04-11   Max.   : 0.76431   Max.   :285.66997   Max.   :5.000 
