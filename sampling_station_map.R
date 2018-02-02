# making maps of sampling stations 

library(maps)
library(mapdata)
library(ggplot2)
library(ggmap)

tb = subset(read_sas("tb_yoy_cn_c.sas7bdat"), month %in% c(4,5,6,7,8,9,10)) %>% select(Longitude, Latitude)



ch = subset(read_sas("ch_yoy_cn_c.sas7bdat"), month %in% c(4,5,6,7,8,9,10)) %>% mutate(bUnk=bunk) %>% select(-bunk) %>% select(Longitude, Latitude)
ir = subset(read_sas("ir_yoy_cn_c.sas7bdat"), month %in% c(5,6,7,8,9,10,11)) %>% select(Longitude, Latitude)
jx = subset(read_sas("jx_yoy_cn_c.sas7bdat") , month %in% c(5,6,7,8,9,10,11)) %>% select(Longitude, Latitude)
ap = subset(read_sas("ap_yoy_cn_c.sas7bdat"), month %in% c(6,7,8,9,10,11)) %>% mutate(bUnk=bunk) %>% select(-bunk) %>% select(Longitude, Latitude)
ck = subset(read_sas("ck_yoy_cn_c.sas7bdat"),  month %in% c(5,6,7,8,9,10,11))%>% select(Longitude, Latitude)




all <- rbind(tb, ch, ir, jx, ap, ck)

#Basic plot
map('state', xlim=c(-86, -80), y=c(26,31), col="gray90", fill=TRUE)
points(all$Longitude, all$Latitude, pch=1, col='black', cex=0.35)





library(rworldmap)
newmap <- getMap(resolution="high")
plot(newmap, xlim=c(-82.5, -81), ylim=c(26, 27))




florida <- map_data("state", "florida")

plot = ggplot() + geom_polygon(data=florida, aes(x=long, y=lat, group=group),fill="white", color="black") + coord_fixed(1.3) +theme_nothing()
plot + coord_fixed(xlim=c(-82.54, -81), ylim=c(27, 29), ratio=1.3)


sbbox <- make_bbox(lon=all$Longitude, lat=all$Latitude, f=.1)
sq_map <- get_map(location = sbbox, maptype = "watercolor", source = "stamen")


#sq_map <- get_map(location = sbbox, maptype = "terrain", source = "stamen")

ggmap(sq_map) +geom_point(data=all, mapping=aes(x=Longitude, y=Latitude), color="red")


#http://r-nold.blogspot.com/2014/06/creating-inset-map-with-ggplot2.html

#https://stackoverflow.com/questions/39067838/parsimonious-way-to-add-north-arrow-and-scale-bar-to-ggmap
# scalebar = function(x,y,w,n,d, units="km"){
#   # x,y = lower left coordinate of bar
#   # w = width of bar
#   # n = number of divisions on bar
#   # d = distance along each division
#   
#   bar = data.frame( 
#     xmin = seq(0.0, n*d, by=d) + x,
#     xmax = seq(0.0, n*d, by=d) + x + d,
#     ymin = y,
#     ymax = y+w,
#     z = rep(c(1,0),n)[1:(n+1)],
#     fill.col = rep(c("black","white"),n)[1:(n+1)])
#   
#   labs = data.frame(
#     xlab = c(seq(0.0, (n+1)*d, by=d) + x, x), 
#     ylab = c(rep(y-w*1.5, n+2), y-3*w),
#     text = c(as.character(seq(0.0, (n+1)*d, by=d)), units)
#   )
#   list(bar, labs)
# }
# 
# sb = scalebar(33.5, -3.8, 0.05, 5, 0.3, "degrees" )
# 
# # Plot map
# 
# ggmap(map, extent= "device") +
#   geom_rect(data=sb[[1]], aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=z), inherit.aes=F,
#             show.legend = F,  color = "black", fill = sb[[1]]$fill.col) +
#   geom_text(data=sb[[2]], aes(x=xlab, y=ylab, label=text), inherit.aes=F, show.legend = F) 