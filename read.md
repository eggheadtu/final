


roadTotalData <- NULL
roadTotalData <- read.table("roadTotalData.txt", header = F, sep = ",")
colnames(roadTotalData) <- c("curTime", "SectionId", "SectionName", "AvgSpd", "AvgOcc", "TotalVol", "MOELevel", "StartWgsX", "StartWgsY", "EndWgsX", "EndWgsY")
options(digits=16)

install.packages("ggmap")
library(ggmap)

twmap <- get_map(location = 'Taipei', zoom = 18, language = "zh-TW")




------------------------------------

?read.table

colnames(roadTotalData) <- c()
colnames(roadTotalData) <- c("curTime", "SectionId", "SectionName", "AvgSpd", "AvgOcc", "TotalVol", "MOELevel", "StartWgsX", "StartWgsY", "EndWgsX", "EndWgsY")

X <- data.frame(bad=0, worse=0, S=0)
colnames(X) <- c("good", "better")
