roadTotalData <- NULL
roadTotalData <- read.table("roadTotalData.txt", header = F, sep = ",")
colnames(roadTotalData) <- c("curTime", "SectionId", "SectionName", "AvgSpd", "AvgOcc", "TotalVol", "MOELevel", "StartWgsX", "StartWgsY", "EndWgsX", "EndWgsY")
options(digits=16)

install.packages("ggmap")
install.packages("RColorBrewer")
install.packages("ggplot2")
library(ggmap)
library("RColorBrewer")
library("ggplot2")

TaipeiMap = get_map(location = c(121.49,24.86,121.58,25.25), zoom=13, maptype = 'roadmap', color = 'bw')
ggmap(TaipeiMap,extent = 'device')



for(curtime in unique(roadTotalData$curTime)){
  selectCurTime <- subset(roadTotalData, curTime==curtime)


  i=1:10000
  png(filename="i.png")
  
  
  TaipeiMapO = ggmap(TaipeiMap, extent = 'device')+
  
               geom_segment(data = subset(selectCurTime, AvgOcc>=0), aes(x = StartWgsX, y = StartWgsY, xend = EndWgsX, yend = EndWgsY, colour = AvgOcc), size=1.1)+ 
               scale_color_continuous(low = "white", high = "red")+
               guides(size=FALSE)+
             
               geom_density2d(data = subset(selectCurTime, TotalVol >= 0), aes(x = (StartWgsX + EndWgsX)*0.5, y = (StartWgsY + EndWgsY)*0.5), size = 0.2) +
               stat_density2d(data = subset(selectCurTime, TotalVol >= 0), aes(x = (StartWgsX + EndWgsX)*0.5, y = (StartWgsY + EndWgsY)*0.5, fill = ..level.., alpha = ..level..), size = 0.01, bins = 16, geom = "polygon") +
               scale_fill_gradient(low = "green", high = "red", guide = FALSE) +
               scale_alpha(range = c(0, 0.3), guide = FALSE)

  TaipeiMapO
  dev.off()
}
-------------------------------
?png
             geom_polygon(data = subset(selectCurTime, AvgOcc>=0), aes(x = (StartWgsX + EndWgsX)*0.5, y = (StartWgsY + EndWgsY)*0.5), color = "grey", size = 0.1, alpha = 0.5)+
             scale_fill_gradientn(colours = brewer.pal(9,"Reds"))
             
myPalette <- colorRampPalette(rev(brewer.pal(7, "Spectral")))
sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(1, 100))

TaipeiMapO = ggmap(TaipeiMap)+ 
             geom_point(data=subset(selectCurTime,TotalVol>=0), aes(x=StartWgsX, y=StartWgsY,color=TotalVol),size=1.5)+ 
             sc+
             guides(size=FALSE)

25¢X05'07.3"N 121¢X37'41.8"E
25.109448, 121.456956

¡A"terrain", "terrain-background", "satellite",
# "roadmap","hybrid" (google maps), "terrain", "watercolor", 
# "toner" (stamen maps)

?ggplot

??geom_segments
