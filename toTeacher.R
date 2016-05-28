install.packages("ggmap")
install.packages("RColorBrewer")
install.packages("ggplot2")
install.packages("ggvis")

roadTotalData <- NULL
roadTotalData <- read.table("roadTotalData.txt", header = F, sep = ",")
colnames(roadTotalData) <- c("curTime", "SectionId", "SectionName", "AvgSpd", "AvgOcc", "TotalVol", "MOELevel", "StartWgsX", "StartWgsY", "EndWgsX", "EndWgsY")
options(digits=16)
densityVol <- NULL

for(i in  1 : nrow(roadTotalData) ){
  for(j in 1 : (roadTotalData[i,]$TotalVol / 10)){
    densityVol <- rbind(densityVol, roadTotalData[i,])
  }
}
head(densityVol, 5)

library(ggmap)
library(RColorBrewer)
library(ggplot2)
library(plotly)
library(scales)
library(ggvis)
library(colorspace)

taipeiMap = get_map(location = c(121.49,24.9,121.58,25.2), zoom=13, maptype = 'roadmap', color = "bw")
taipeiMRT <- read.table("taipeiMRT.txt", header = T, sep = ",")
colorSize <- 1
taipeiMRTMapO = ggmap(taipeiMap, extent = 'device') + 
  geom_path(data = subset(taipeiMRT, line_no == 1), aes(x = lon,y = lat, group = factor(line_no), fill = factor(line_no)), size = colorSize, color="#D4942F") +
  geom_path(data = subset(taipeiMRT, line_no == 2), aes(x = lon,y = lat, group = factor(line_no), fill = factor(line_no)), size = colorSize, color="Red") +
  geom_path(data = subset(taipeiMRT, line_no == 3), aes(x = lon,y = lat, group = factor(line_no), fill = factor(line_no)), size = colorSize, color="#25B325") +
  geom_path(data = subset(taipeiMRT, line_no == 4), aes(x = lon,y = lat, group = factor(line_no), fill = factor(line_no)), size = colorSize, color="#FFC02E") +
  geom_path(data = subset(taipeiMRT, line_no == 5), aes(x = lon,y = lat, group = factor(line_no), fill = factor(line_no)), size = colorSize, color="Blue") +
  guides(size=FALSE, colour = FALSE)
taipeiMRTMapO

for(curtime in unique(roadTotalData$curTime)){
  selectCurTime <- subset(roadTotalData, curTime==curtime)
  AvgOcc <- selectCurTime$AvgOcc
  plotTime <- gsub(":","-",curtime)
  mypath <- file.path("C:","Users","user","Documents","plot_TotalVol",paste("plot_", plotTime, ".png", sep = ""))
  
  taipeiMapO = taipeiMRTMapO +
    geom_segment(data = subset(selectCurTime, AvgOcc >= 0), aes(x = StartWgsX, y = StartWgsY, xend = EndWgsX, yend = EndWgsY, colour = AvgOcc), size=1.1) + 
    scale_colour_gradientn(colours = rev(diverge_hcl(12, h = c(0, -100), c = c(40, 80), l = c(75, 40), power = 1)), limits=c(0,15), oob=squish, na.value = "white") +
    geom_label( aes(x=121.568, y=25.095, label=curtime))
  ggsave(filename = mypath)
}

for(curtime in unique(roadTotalData$curTime)){
  
  selectCurTime <- subset(roadTotalData, curTime==curtime)
  plotTime <- gsub(":","-",curtime)
  mypath <- file.path("C:","Users","user","Documents","plot_TotalVol",paste("plot_", plotTime, ".png", sep = ""))
  densityVol <- NULL
  
  for(i in  1 : nrow(selectCurTime) ){
    for(j in 1 : (selectCurTime[i,]$TotalVol / 10)){
      densityVol <- rbind(densityVol, selectCurTime[i,])
    }
  }
  
  taipeiMapO = ggmap(taipeiMap, extent = 'device') +
    stat_density2d(data = densityVol, aes(x = (StartWgsX + EndWgsX)*0.5, y = (StartWgsY + EndWgsY)*0.5, colour = TotalVol, fill = ..level.., alpha = ..level..), size = 0.001, bins = 15, geom = "polygon") +
    scale_fill_gradient(low = "lightgreen", high = "lightpink1", guide = FALSE, limits=c(0, 800), oob=squish) +
    scale_alpha(range = c(0, 0.4), guide = FALSE) +
    geom_density2d(data = densityVol, aes(x = (StartWgsX + EndWgsX)*0.5, y = (StartWgsY + EndWgsY)*0.5, colour = TotalVol), size = 0.6, color = "navy") +
    geom_label( aes(x=121.568, y=25.095, label=curtime))
  taipeiMapO
  ggsave(filename = mypath)
}