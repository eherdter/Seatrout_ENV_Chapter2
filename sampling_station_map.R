# making maps of sampling stations 
rm(list=ls())
devtools::install_github("dkahle/ggmap")
install_version("ggplot2", version = "2.1.0", repos = "http://cran.us.r-project.org")
library(haven)
library(maps)
library(mapdata)
library(ggplot2)
library(ggmap)
library(raster)
library(gridExtra)
library(grid)
#library(rgdal)
library(dplyr)
#library(raster)

personal_comp = "~/Desktop/PhD project/Projects/Seatrout/FWRI SCRATCH FOLDER/Elizabeth Herdter/SAS data sets/FIMData/NEWNov7"
work_comp = "U:/PhD_projectfiles/Raw_Data/Seatrout_FIM_Data/FIMData/NEWNov7"

setwd(personal_comp)
setwd(work_comp)

tb = subset(read_sas("tb_yoy_cn_c.sas7bdat"), month %in% c(4,5,6,7,8,9,10)) %>% dplyr::select(number, Longitude, Latitude)
ch = subset(read_sas("ch_yoy_cn_c.sas7bdat"), month %in% c(4,5,6,7,8,9,10) & !Reference %in% c("CHM1989051501","CHM1989051502","CHM1989051503")) %>% dplyr::mutate(bUnk=bunk) %>% dplyr::select(-bunk) %>% dplyr::select(number,Longitude, Latitude) 
ir = subset(read_sas("ir_yoy_cn_c.sas7bdat"), month %in% c(5,6,7,8,9,10,11) & !Reference %in% c("IRM1997091503", "IRM1997101501", "IRM1997071407")) %>% dplyr::select(number,Longitude, Latitude)
jx = subset(read_sas("jx_yoy_cn_c.sas7bdat") , month %in% c(5,6,7,8,9,10,11) & !Reference %in% c("JXM2001080703")) %>% dplyr::select(number,Longitude, Latitude)
ap = subset(read_sas("ap_yoy_cn_c.sas7bdat"), month %in% c(6,7,8,9,10,11)) %>% dplyr::mutate(bUnk=bunk) %>% dplyr::select(-bunk) %>% dplyr::select(number,Longitude, Latitude)
ck = subset(read_sas("ck_yoy_cn_c.sas7bdat"),  month %in% c(5,6,7,8,9,10,11))%>% dplyr::select(number,Longitude, Latitude)

all <- rbind(tb, ch, ir, jx, ap, ck)
all$binary_label <- ifelse(all$number>0, "present", "absent")


positive <- all %>% subset(number > 0)
positive$unique <- paste(positive$Longitude, positive$Latitude, sep=",")
test <- positive %>% distinct(Longitude, Latitude)
aggregated <- aggregate(number ~ unique, data=positive, FUN= "sum")
aggregated = aggregated %>% mutate(long= gsub(",.*$", "", aggregated$unique), lat=gsub("^.*,", "", aggregated$unique))

aggregated$lat <- as.numeric(aggregated$lat)
aggregated$long <- as.numeric(aggregated$long)



#remove outliers ####
#ch outlier
ch[ch$Longitude == min(ch$Longitude),]
test <- base::subset(ch, Longitude == min(ch$Longitude))
#reference numbers: CHM1989051501, CHM1989051502, CHM1989051503

#ir outlier
test <- base::subset(ir, Longitude <= -81)
#IRM1997091503, IRM1997101501, 

ir[ir$Longitude == max(ir$Longitude),]
test <- base::subset(ir, Longitude > -80.25)
#IRM1997071407

#jx outlier 
jx[jx$Longitude == min(jx$Longitude),]
#JXM2001080703 


#Here- Basic plot####
map('state', xlim=c(-86, -80), y=c(26,31), col="gray90", fill=TRUE)
points(all$Longitude, all$Latitude, pch=1, col='black', cex=0.35)


#world map####
library(rworldmap)
newmap <- getMap(resolution="high")
plot(newmap, xlim=c(-82.5, -81), ylim=c(26, 27))


florida <- map_data("state", "florida", xlim=c(-82.5, -81), ylim=c(26, 27))
country <- map_data("usa")
pinellas <- map_data("county", "florida")

plot = ggplot() + geom_polygon(data=florida, aes(x=long, y=lat, group=group),fill="white", color="black") + coord_fixed(1.3) +theme_nothing()
plot + coord_fixed(xlim=c(-82.54, -81), ylim=c(27, 29), ratio=1.3)


# cool maps with ggplot and google maps ####
sbbox <- make_bbox(lon=all$Longitude, lat=all$Latitude, f=.1)
sq_map <- get_map(location = sbbox, maptype = "satellite", source = "google")
#sq_map <- get_map(location = sbbox, maptype = "terrain", source = "stamen")

ggmap(sq_map) +geom_point(data=all, mapping=aes(x=Longitude, y=Latitude), color="red")


#PLOT ALL SAMPLING STATIONS ####
#example to create inset maps with ggplot2 ####
#http://r-nold.blogspot.com/2014/06/creating-inset-map-with-ggplot2.html


long = c(-88, -88, -84, -84, -79,-79)
lat =c(25,31,25,31, 25,31)
test <- data.frame(cbind(lat, long))
test$group <- 1
#main map
plot = ggplot() + 
  #geom_polygon(data=florida, aes(x=long+0.1, y=lat-0.1,group=group), fill="#9ecae1")+
  geom_polygon(data=florida, aes(x=long, y=lat, group=group),color="grey10", fill ="#fff7bc") + 
  geom_point(data=all, aes(x=Longitude, y=Latitude, color=binary_label), size=0.25) + 
  #geom_point(data=all, aes(x=Longitude, y=Latitude), size=0.25, color="grey") + 
  #geom_point(data=aggregated, aes(x=long, y=lat), size=0.25, color="red") + 
  coord_equal()+coord_map(xlim=c(-85.5,-80), ylim=c(26.25,30.85))+
  theme_bw()+xlab("")+ylab("") + 
  scale_x_continuous(breaks=seq(-85.5,-80,1), labels=c(paste(seq(-85.5,-80, 1),"°E", sep="")))+
  scale_y_continuous(breaks=seq(27.25,30.85, 1), labels=c(paste(seq(27.25,30.85, 1),"°N", sep="")))+
  theme(axis.text.y =element_text(angle = 90, hjust=0.5))

#inset map


# Extent rectangle for inset map
pol<-data.frame(xmin=-85.25,xmax=-79 ,ymin=24.75 ,ymax=32)

inset_plot = ggplot() + geom_polygon(data=country, aes(long, lat, group=group), color="grey10", fill="#fff7bc")+
  coord_equal() + theme_bw() +labs(x=NULL, y=NULL)+
  scale_x_continuous(breaks=seq(-130, -65, 10), labels=c(paste(seq(-130,-65, 10), "°E", sep="")))+ 
  scale_y_continuous(breaks=seq(25,50,5), labels=c(paste(seq(25,50,5), "°N", sep="")))+
  geom_rect(data = pol, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha=0, colour="red", size = 0.5, linetype=1)+
  theme(axis.text.x =element_blank(),axis.text.y= element_blank(), axis.ticks=element_blank(),axis.title.x =element_blank(),
        axis.title.y= element_blank())+
  theme(plot.margin=unit(c(0,0,0,0), "mm"))

File <- ("U:/PhD_projectfiles/Figures/sampling_map.tiff")
if (file.exists(File)) stop(File, " already exists")
dir.create(dirname(File), showWarnings = FALSE)

tiff(File, units="in", width=5, height=5, res=300)
#png(file="florida_sampling_stations.png",w=1800,h=1800, res=300)
grid.newpage()
v1<-viewport(width = 1, height = 1, x = 0.5, y = 0.5) #plot area for the main map
v2<-viewport(width = 0.35, height = 0.35, x = 0.25, y = 0.28) #plot area for the inset map
print(plot,vp=v1) 
print(inset_plot,vp=v2)
dev.off()

#http://stackoverflow.com/questions/21027548/cropping-extra-white-space-in-a-plot-made-using-ggplot-package-in-r


#tampa bay ####
plot_tb = ggplot() + #geom_polygon(data=florida, aes(x=long+0.1, y=lat-0.1,group=group), fill="#9ecae1")+
  geom_polygon(data=florida, aes(x=long, y=lat, group=group),color="grey10", fill ="#fff7bc") + 
  geom_point(data=aggregated, aes(x=long, y=lat), size=0.25, color="red") + 
  #geom_point(data=all, aes(x=Longitude, y=Latitude), size=0.25, color="grey") + 
  #geom_point(data=aggregated, aes(x=long, y=lat), size=0.25, color="red") + 
  coord_equal()+coord_map(xlim=c(-83,-82), ylim=c(27.25,28.25))+
  theme_bw()+xlab("")+ylab("") + 
  scale_x_continuous(breaks=seq(-83,-82,0.5), labels=c(paste(seq(-83,-82, 0.5),"°E", sep="")))+
  scale_y_continuous(breaks=seq(27.25,28.25, 1), labels=c(paste(seq(27.25,28.25, 1),"°N", sep="")))+
  theme(axis.text.y =element_text(angle = 90, hjust=0.5))


shape <- shapefile("~/Desktop/DRI_Export_2016/DRI_Export_2016.shp")
shape1 <- readOGR(shape=path.expand("~/Desktop/DRI_Export_2016/DRI_Export_2016"), layer="DRI_Export_2016")
plot_tb = ggplot() + geom_polygon(data=shape, aes(x=long, y=lat, group=group), fill="#9ecae1")

#inset map






#PLOT ALL UNIQUE SAMPLING STATIONS FOR ALL ####
#add scale bars to maps ####
#https://github.com/3wen/legendMap

library(ggsn)
library(legendMap)

plot= ggplot() + #geom_polygon(data=florida, aes(x=long+0.1, y=lat-0.1,group=group), fill="#9ecae1")+
  geom_polygon(data=florida, aes(x=long, y=lat, group=group),color="grey10", fill ="grey") + 
  geom_point(data=aggregated, aes(x=long, y=lat), size=0.5, color="red", shape=2) + 
  #geom_point(data=all, aes(x=Longitude, y=Latitude), size=0.25, color="grey") + 
  #geom_point(data=aggregated, aes(x=long, y=lat), size=0.25, color="red") + 
  coord_equal()+ coord_map(xlim=c(-85.5,-80.0), ylim=c(26.0,31.0))+
  scale_x_continuous(breaks=seq(-85.5, -80.0, 1), labels=c(paste(seq(-85.5, -80.0, 1), "°E", sep="")))+ 
  scale_y_continuous(breaks=seq(26.0,31.0,1), labels=c(paste(seq(26.0,31.0,1), "°N", sep="")))+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(), 									
        panel.background=element_rect(fill='white', colour='black'))+
  xlab("")+ylab("") + 
  theme(axis.text.y =element_text(angle = 0, hjust=1)) +
  scale_bar(lon = -85, lat = 27, 
distance_lon = 50, distance_lat = 5, distance_legend = 20, 
dist_unit = "km", arrow_length=50, arrow_distance=60, arrow_north_size=6)+
annotate("text", x = -84.9, y = 29.5, label = "AP")+
  annotate("text", x = -83.5, y = 29.25, label = "CK")+
  annotate("text", x = -83, y = 27.75, label = "TB")+
  annotate("text", x = -82.5, y = 26.5, label = "CH")+
  annotate("text", x = -81.2, y = 30.5, label = "JX")+
  annotate("text", x = -80.4, y = 28.5, label = "IR")+
  annotate("text", x = -84.5, y = 28.5, label = "Gulf of Mexico", fontface="bold")

# Extent rectangle for inset map
pol<-data.frame(xmin=-85.5,xmax=-79 ,ymin=24.75 ,ymax=32)

inset_plot = ggplot() + geom_polygon(data=country, aes(long, lat, group=group), color="grey10", fill="grey")+
  coord_equal() + theme_bw() +labs(x=NULL, y=NULL)+
  scale_x_continuous(breaks=seq(-130, -65, 10), labels=c(paste(seq(-130,-65, 10), "°E", sep="")))+ 
  scale_y_continuous(breaks=seq(25,50,5), labels=c(paste(seq(25,50,5), "°N", sep="")))+
  geom_rect(data = pol, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha=0, colour="red", size = 0.5, linetype=1)+
  theme(axis.text.x =element_blank(),axis.text.y= element_blank(), axis.ticks=element_blank(),axis.title.x =element_blank(),
        axis.title.y= element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  theme(plot.margin=unit(c(0,0,0,0), "mm"))

File <- ("U:/PhD_projectfiles/Figures/UNIQUE_sampling_map.tiff")
if (file.exists(File)) stop(File, " already exists")
dir.create(dirname(File), showWarnings = FALSE)

tiff(File, units="in", width=5, height=5, res=300)

#png(file="UNIQUE_florida_sampling_stations.png",w=1800,h=1800, res=300)
grid.newpage()
v1<-viewport(width = 1, height = 1, x = 0.5, y = 0.5) #plot area for the main map
v2<-viewport(width = 0.35, height = 0.35, x = 0.3, y = 0.175) #plot area for the inset map
print(plot,vp=v1) 
print(inset_plot,vp=v2)
dev.off()

#add scale bars to maps ####

#https://stackoverflow.com/questions/39067838/parsimonious-way-to-add-north-arrow-and-scale-bar-to-ggmap
scalebar = function(x,y,w,n,d, units="km"){
  # x,y = lower left coordinate of bar
  # w = width of bar
  # n = number of divisions on bar
  # d = distance along each division

  bar = data.frame(
    xmin = seq(0.0, n*d, by=d) + x,
    xmax = seq(0.0, n*d, by=d) + x + d,
    ymin = y,
    ymax = y+w,
    z = rep(c(1,0),n)[1:(n+1)],
    fill.col = rep(c("black","white"),n)[1:(n+1)])

  labs = data.frame(
    xlab = c(seq(0.0, (n+1)*d, by=d) + x, x),
    ylab = c(rep(y-w*1.5, n+2), y-3*w),
    text = c(as.character(seq(0.0, (n+1)*d, by=d)), units)
  )
  list(bar, labs)
}

sb = scalebar(27.5, -3.8, 0.05, 5, 0.3, "degrees" )
# 
# # Plot map
# 
# ggmap(map, extent= "device") +
#   geom_rect(data=sb[[1]], aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=z), inherit.aes=F,
#             show.legend = F,  color = "black", fill = sb[[1]]$fill.col) +
#   geom_text(data=sb[[2]], aes(x=xlab, y=ylab, label=text), inherit.aes=F, show.legend = F) 