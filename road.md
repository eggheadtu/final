#每筆資料約700到800個路口
#每五分鐘讀1筆資料
#每一天讀288筆資料
#預計連續讀取一星期

install.packages("jsonlite")
install.packages("RCurl")

library(jsonlite)
library(RCurl)
totalData <- NULL
roadTotalData <- NULL


for(num in 1:288){

    tryCatch({
        road <- fromJSON(getURL("http://data.taipei/opendata/datalist/apiAccess?scope=resourceAquire&rid=5aacba65-afda-4ad5-88f5-6026934140e6"))
        curTime <- Sys.time()
        roadTempData <- road$result$results
        roadTempData$curTime <- curTime
        roadTempData <- subset(roadTempData, select = c(curTime, SectionId,  SectionName, AvgSpd, AvgOcc, TotalVol, MOELevel, StartWgsX, StartWgsY, EndWgsX, EndWgsY))
        write.table(roadTempData, file = "roadTotalData.txt", sep= ",", append= T, row.names = F,col.name = F)
        roadTotalData <- rbind(roadTotalData, roadTempData)
        
        dataCount <- nrow(roadTempData)
        tempData <- data.frame(curTime, dataCount)
        write.table(tempData, file = "totalData.txt",sep=",", append=T, row.names = F,col.name = F)
        totalData <- rbind(totalData, tempData)
        
        Sys.sleep(299)
    }, warning = function(w) {
        print(w)
    }, error = function(e) {
        print(e)
    }, finally = {
        
    })
}
----------------------------
?tryCatch




?delay

??column.name
?write.table

testit <- function(x)
{
    p1 <- proc.time()
    Sys.sleep(x)
    proc.time() - p1 # The cpu usage should be negligible
}
testit(3.7)



parse.dcf(text = NULL, file = "", fields = NULL,
          versionfix = FALSE)

  
write.csv(roadTempData, file = "roadTotalData.csv", append = TRUE)

?
?save

  install.packages("xlsx")
library(xlsx)
write.csv(roadTotalData, file='roadTempData')



col_idx <- grep("curTime", names(roadTempData))
roadTempData <- roadTempData[, c(col_idx, (1:ncol(roadTempData))[-col_idx])]
names(roadTempData)

roadTempData <- subset(roadTempData, select = c(curTime, SectionId, SectionName, AvgSpd, AvgOcc, TotalVol, MOELevel, StartWgsX, StartWgsY, EndWgsX, EndWgsY))

id <- roadData$SectionId
dataCount <- aggregate(roadData$SectionId~SectionId,roadData,length)
library(knitr)
kable(head(PostCount[order(PostCount$Posts,decreasing = T),],10))

for(i in 1:nrow(PostCount)){
  totalPageLikes <- merge(totalPage, likes, by = "dateDay")
}
