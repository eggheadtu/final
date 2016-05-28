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
  mypath <- file.path("C:","Users","user","Documents","plot_AvgOcc",paste("plot_", plotTime, ".png", sep = ""))
  
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



#########################################################
ggmap(TaipeiMap + taipeiMRTMap)
?geom_path
summary(roadTotalData$AvgOcc)
ggplotly(roadTotalData$AvgOcc)
densityVol <- NULL

for(i in  1 : nrow(selectCurTime) ){
  for(j in 1 : (selectCurTime[i,]$TotalVol / 10)){
    densityVol <- rbind(densityVol, selectCurTime[i,])
  }
}
head(density, 5)

table(roadTotalData[roadTotalData$AvgOcc == 100,]$SectionName)
str(roadTotalData)

tail(sort(table(roadTotalData[roadTotalData$AvgOcc == 100,]$SectionName)),8)





TaipeiMap = get_map(location = c(121.49,24.9,121.58,25.2), zoom=13, maptype = 'roadmap', color = "bw")
ggmap(TaipeiMap,extent = 'device')

for(curtime in unique(roadTotalData$curTime)){
  
  selectCurTime <- subset(roadTotalData, curTime==curtime)
  AvgOcc <- selectCurTime$AvgOcc
  plotTime <- gsub(":","-",curtime)
  mypath <- file.path("C:","Users","user","Documents","final",paste("plot_", plotTime, ".png", sep = ""))
  
  
  taipeiMapO = ggmap(TaipeiMap, extent = 'device') +
    
    geom_segment(data = subset(selectCurTime, AvgOcc >= 0), aes(x = StartWgsX, y = StartWgsY, xend = EndWgsX, yend = EndWgsY, colour = AvgOcc), size=1.1)+ 
    scale_color_continuous(low = "red", high = "green", limits=c(0, 50), oob = squish)+
    
    
    geom_label( aes(x=121.568, y=25.095, label=curtime))
  
  taipeiMapO
  
  pMap <- plotly_build(TaipeiMapO)
  for(i in 1:length(pMap$data)){
    pMap$data[[i]]$text <- selectCurTime$SectionName
  }
  
  
  ggsave(filename = mypath)
}
ggplotly(taipeiMRTMapO)+TaipeiMap




for(curtime in unique(roadTotalData$curTime)){
  
  selectCurTime <- subset(roadTotalData, curTime==curtime)
  AvgOcc <- selectCurTime$AvgOcc
  plotTime <- gsub(":","-",curtime)
  mypath <- file.path("C:","Users","user","Documents","final1",paste("plot_", plotTime, ".png", sep = ""))
  ggsave(filename = mypath)
  
  
  TaipeiMapO = ggmap(TaipeiMap, extent = 'device') +
    
    geom_density2d(data = densityVol, aes(x = (StartWgsX + EndWgsX)*0.5, y = (StartWgsY + EndWgsY)*0.5, colour = TotalVol), size = 0.3, color = "#EBAEAE") +
    stat_density2d(data = densityVol, aes(x = (StartWgsX + EndWgsX)*0.5, y = (StartWgsY + EndWgsY)*0.5, colour = TotalVol, fill = ..level.., alpha = ..level..), size = 0.001, bins = 15, geom = "polygon") +
    scale_fill_gradient(low = "#FCDCDC", high = "red", guide = FALSE, limits=c(0, 700)) +
    scale_alpha(range = c(0, 0.08), guide = FALSE)
  
  
  
  
  TaipeiMapO
  ggsave(filename = mypath)
}


-------------------------------
  geom_segment(data = subset(selectCurTime, AvgOcc >= 0), aes(x = StartWgsX, y = StartWgsY, xend = EndWgsX, yend = EndWgsY, colour = AvgOcc), size=1.1)+ 
  #scale_color_continuous(low = "cornsilk1", high = "darkorchid", limits=c(0, 20), oob = squish)+
  #scale_color_continuous(low = "white", high = "black", limits=c(0, 50), oob = squish)+
  #scale_colour_gradientn(colours = rev(heat.colors(100)), limits=c(0,18),oob=squish)+
  scale_colour_gradientn(colours = rev(diverge_hcl(12, h = c(0, -100), c = c(40, 80), l = c(75, 40), power = 1)), limits=c(0,12), oob=squish, na.value = "white")+
  #scale_colour_gradientn(colours = YlOrRd, limits=c(0,10),oob=squish)+
  #scale_colour_brewer(palette = "YlOrRd")
  
  geom_path(data = subset(taipeiMRT, line_no == 1), aes(x = lon,y = lat, group = factor(line_no), fill = factor(line_no)), size = colorSize, color="#D4942F") +
  geom_path(data = subset(taipeiMRT, line_no == 2), aes(x = lon,y = lat, group = factor(line_no), fill = factor(line_no)), size = colorSize, color="Red") +
  geom_path(data = subset(taipeiMRT, line_no == 3), aes(x = lon,y = lat, group = factor(line_no), fill = factor(line_no)), size = colorSize, color="#25B325") +
  geom_path(data = subset(taipeiMRT, line_no == 4), aes(x = lon,y = lat, group = factor(line_no), fill = factor(line_no)), size = colorSize, color="#FFC02E") +
  geom_path(data = subset(taipeiMRT, line_no == 5), aes(x = lon,y = lat, group = factor(line_no), fill = factor(line_no)), size = colorSize, color="Blue") +
  
  geom_path(data = subset(taipeiMRT, line_no == 1), aes(x = lon,y = lat, group = factor(line_no), fill = factor(line_no)), size = colorSize, color="#EDCCAF") +
  geom_path(data = subset(taipeiMRT, line_no == 2), aes(x = lon,y = lat, group = factor(line_no), fill = factor(line_no)), size = colorSize, color="#FFC2C2") +
  geom_path(data = subset(taipeiMRT, line_no == 3), aes(x = lon,y = lat, group = factor(line_no), fill = factor(line_no)), size = colorSize, color="#B1FAAF") +
  geom_path(data = subset(taipeiMRT, line_no == 4), aes(x = lon,y = lat, group = factor(line_no), fill = factor(line_no)), size = colorSize, color="#FAECAF") +
  geom_path(data = subset(taipeiMRT, line_no == 5), aes(x = lon,y = lat, group = factor(line_no), fill = factor(line_no)), size = colorSize, color="#CACEFA") +
  
  geom_density2d(data = subset(selectCurTime, TotalVol >= 0), aes(x = (StartWgsX + EndWgsX)*0.5, y = (StartWgsY + EndWgsY)*0.5, colour = TotalVol), size = 0.3, color = "#EBAEAE") +
  stat_density2d(data = subset(selectCurTime, TotalVol >= 0), aes(x = (StartWgsX + EndWgsX)*0.5, y = (StartWgsY + EndWgsY)*0.5, colour = TotalVol, fill = ..level.., alpha = ..level..), size = 0.001, bins = 15, geom = "polygon") +
  scale_fill_gradient(low = "#FCDCDC", high = "red", guide = FALSE, limits=c(0, 700)) +
  scale_alpha(range = c(0, 0.08), guide = FALSE)

geom_density2d(data = subset(selectCurTime, TotalVol >= 0), aes(x = (StartWgsX + EndWgsX)*0.5, y = (StartWgsY + EndWgsY)*0.5), size = 0.2) +
  scale_color_continuous(low = "#FFFEE8", high = "#FFF700", limits=c(0, 100))+
  scale_color_continuous(low = "#F5FCF6", high = "green", limits=c(0, 100))+
  scale_color_continuous(low = "#FFF9F9", high = "red", limits=c(0, 100))+
  + geom_text( aes(121.33, 25.05),data=curtime, parse = FALSE, check_overlap = FALSE, na.rm = FALSE)
25¢X05'39.6"N 121¢X33'43.4"E
geom_text( aes(121.31, 25.02,label=curtime))
geom_text(label = curtime, aes(121.31, 25.02))
25¢X02'37.9"N 121¢X31'46.7"E
25¢X05'50.9"N 121¢X34'17.0"E
TaipeiMapO + labs(title=curtime)
geom_label
?geom_text
usr <- par( "usr" )
text( usr[ 2 ], usr[ 4 ], curtime,    adj = c( 1, 1 ), col = "blue" )
text( usr[ 1 ], usr[ 3 ], "left bottom",  adj = c( 0, 0 ), col = "blue" )
plot( 1:10 )


TaipeiMap = get_map(source = c("osm"), location = c(121.49,24.89,121.58,25.22), zoom=13, maptype = 'roadmap', color = 'bw')
ggmap(TaipeiMap,extent = 'device')

geom_density2d(data = subset(selectCurTime, TotalVol >= 0), aes(x = (StartWgsX + EndWgsX)*0.5, y = (StartWgsY + EndWgsY)*0.5), size = 0.2) +
  stat_density2d(data = subset(selectCurTime, TotalVol >= 0), aes(x = (StartWgsX + EndWgsX)*0.5, y = (StartWgsY + EndWgsY)*0.5, fill = ..level.., alpha = ..level..), size = 0.01, bins = 16, geom = "polygon") +
  scale_fill_gradient(low = "green", high = "red", guide = FALSE) +
  scale_alpha(range = c(0, 0.3), guide = FALSE)
=======
  geom_segment(data = subset(selectCurTime, AvgOcc>=0), aes(x = StartWgsX, y = StartWgsY, xend = EndWgsX, yend = EndWgsY, colour = AvgOcc), size=1.1)+ 
  scale_color_continuous(low = "pink", high = "red", limits=c(1, 100))+
  guides(size=FALSE, colour = FALSE)


geom_density2d(data = subset(selectCurTime, TotalVol >= 0), aes(x = (StartWgsX + EndWgsX)*0.5, y = (StartWgsY + EndWgsY)*0.5), size = 0.2) +
  stat_density2d(data = subset(selectCurTime, TotalVol >= 0), aes(x = (StartWgsX + EndWgsX)*0.5, y = (StartWgsY + EndWgsY)*0.5, fill = ..level.., alpha = ..level..), size = 0.01, bins = 16, geom = "polygon") +
  scale_fill_gradient(low = "green", high = "red", guide = FALSE) +
  scale_alpha(range = c(0, 0.3), guide = FALSE)

TaipeiMapO

}
-------------------------------
  
  >>>>>>> origin/master



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
"terrain", "terrain-background", "satellite",
# "roadmap","hybrid" (google maps), "terrain", "watercolor", 
# "toner" (stamen maps)

?ggplot

??geom_segments