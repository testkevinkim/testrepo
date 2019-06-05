
# class hourPrep

hourPrep = setRefClass("hourPrep",
                       fields = 
                         list(
                           filePath="character",
                           libraryList ="vector"
                         ),
                       methods=
                         list(
                           initialize = function(){
                             .self$filePath="~/Desktop/Delete/rstudio-export/all_hist_data_60minute.rds"
                             .self$libraryList=c("dplyr","data.table","fts","zoo")
                             
                             .self$loadLibrary()
                             
                           },
                           loadLibrary = function(){
                             lapply(.self$libraryList, function(x) eval(parse(text=paste("library(",x,")") ) ) )
                           },
                           loadData = function(){
                             path=.self$filePath
                             readRDS(path) %>% as.data.table()
                           },
                           lagData = function( DT,sortCols,sortDirection, cols, prefix="P", moveN=1){
                             DT=DT %>% as.data.table()
                             setorderv(DT, sortCols, sortDirection)
                             anscols=paste0(prefix,moveN,cols)
                             DT[,(anscols) := shift(.SD, moveN, NA,"lag"), .SDcols=cols]
                           },
                           leadData = function( DT,sortCols,sortDirection, cols, prefix="N", moveN=1){
                             DT=DT %>% as.data.table()
                             setorderv(DT, sortCols, sortDirection)
                             anscols=paste0(prefix,moveN,cols)
                             DT[,(anscols) := shift(.SD, moveN, NA,"lead"), .SDcols=cols]
                           },
                           dayNumAttached = function(DT, sortCols, sortDirection, dateCol="DATE"){
                             DT =DT %>% as.data.table()
                             setorderv(DT, sortCols, sortDirection)
                             
                             dateselect=paste0("daynumdt = data.table(dateseries=DT$",dateCol," %>% sort(), stringsAsFactors=FALSE)")
                             dataframebuild = paste0("daynumdt$DAYNUM = as.numeric(row.names(daynumdt))")
                             dayoplist = c(dateselect, dataframebuild)
                             lapply(dayoplist, function(x) parse(eval(text=x)))
                             
                             
                           }
                           
                         )#method ends
                       )#class ends
