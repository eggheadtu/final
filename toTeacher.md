roadTotalData <- NULL
roadTotalData <- read.table("roadTotalData.txt", header = F, sep = ",")
colnames(roadTotalData) <- c("curTime", "SectionId", "SectionName", "AvgSpd", "AvgOcc", "TotalVol", "MOELevel", "StartWgsX", "StartWgsY", "EndWgsX", "EndWgsY")
options(digits=16)

install.packages("ggmap")
install.packages("RColorBrewer")
install.packages("ggplot2")
library(ggmap)
library(RColorBrewer)
library(ggplot2)
library(plotly)

TaipeiMap = get_map(location = c(121.49,24.9,121.58,25.2), zoom=13, maptype = 'roadmap', color = 'bw')
ggmap(TaipeiMap,extent = 'device')

for(curtime in unique(roadTotalData$curTime)){
  
  selectCurTime <- subset(roadTotalData, curTime==curtime)
  AvgOcc <- selectCurTime$AvgOcc
  plotTime <- gsub(":","-",curtime)
  mypath <- file.path("C:","Users","user","Documents","final",paste("plot_", plotTime, ".png", sep = ""))
  ggsave(filename = mypath)
  
  TaipeiMapO = ggmap(TaipeiMap, extent = 'device')+
    
    geom_segment(data = subset(selectCurTime, AvgOcc >= 0), aes(x = StartWgsX, y = StartWgsY, xend = EndWgsX, yend = EndWgsY, colour = AvgOcc), size=1.1)+ 
    scale_color_continuous(low = "#F5FCF6", high = "green", limits=c(0, 100))+
    guides(size=FALSE, colour = FALSE)+
    
    geom_label(selectCurTime, aes(x=25.02, y=121.33, label=curTime))
  
  TaipeiMapO
  
}
ggplotly(TaipeiMapO)