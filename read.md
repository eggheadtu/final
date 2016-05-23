roadTotalData <- NULL
roadTotalData <- read.table("roadTotalData.txt", header = F, sep = ",")
colnames(roadTotalData) <- c("curTime", "SectionId", "SectionName", "AvgSpd", "AvgOcc", "TotalVol", "MOELevel", "StartWgsX", "StartWgsY", "EndWgsX", "EndWgsY")
options(digits=16)

install.packages("ggmap")
install.packages("RColorBrewer")
install.packages("ggplot2")
install.packages("ggsave")
library(ggmap)
library(RColorBrewer)
library(ggplot2)
library(ggsave)

TaipeiMap = get_map(location = c(121.49,24.9,121.58,25.2), zoom=13, maptype = 'roadmap', color = 'bw')
ggmap(TaipeiMap,extent = 'device')

TaipeiMap = get_map(source = c("osm"), location = c(121.49,24.89,121.58,25.22), zoom=13, maptype = 'roadmap', color = 'bw')
ggmap(TaipeiMap,extent = 'device')

for(curtime in unique(roadTotalData$curTime)){

  selectCurTime <- subset(roadTotalData, curTime==curtime)
  plotTime <- gsub(":","-",curtime)
  mypath <- file.path("C:","Users","CGU","Documents",paste("plot_", plotTime, ".png", sep = ""))

  ggsave(filename = mypath)
   
  TaipeiMapO = ggmap(TaipeiMap, extent = 'device')+
  
               geom_segment(data = subset(selectCurTime, AvgOcc>=0), aes(x = StartWgsX, y = StartWgsY, xend = EndWgsX, yend = EndWgsY, colour = AvgOcc), size=1.1)+ 
               scale_color_continuous(low = "white", high = "red", limits=c(1, 100))+
               guides(size=FALSE, colour = FALSE)+
               
               
               geom_density2d(data = subset(selectCurTime, TotalVol >= 0), aes(x = (StartWgsX + EndWgsX)*0.5, y = (StartWgsY + EndWgsY)*0.5), size = 0.2) +
               stat_density2d(data = subset(selectCurTime, TotalVol >= 0), aes(x = (StartWgsX + EndWgsX)*0.5, y = (StartWgsY + EndWgsY)*0.5, fill = ..level.., alpha = ..level..), size = 0.01, bins = 16, geom = "polygon") +
               scale_fill_gradient(low = "green", high = "red", guide = FALSE) +
               scale_alpha(range = c(0, 0.3), guide = FALSE)
            
  TaipeiMapO
  
}
-------------------------------



 
ggsave(plot = last_plot(), device = NULL, path = "C:","Users","CGU","Documents", scale = 1, width = NA, height = NA, units = c("mm"), dpi = 300)

  png(file=mypath)
    dev.off()
?ggsave
?get_map
?ggmap
names = LETTERS[1:26]

beta1 = rnorm(26, 5, 2) 
beta0 = 10 

for(i in 1:26){
 x = rnorm(500, 105, 10)
 y = beta0 + beta1[i]*x + 15*rnorm(500)

 mypath <- file.path("C:","Users","user","Documents","final",paste("myplot_", names[i], ".jpg", sep = ""))

 png(file=mypath)
    mytitle = paste("my title is", names[i])
    plot(x,y, main = mytitle)
 dev.off()
}
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
