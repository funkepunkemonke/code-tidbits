library(readr)
library(tidyverse)
library(ggplot2)
library(maps)
library(raster)
library(rgbif)
library(data.table)

world.inp <- map_data("world")

myspecies <- export_gisd$Species # Name taxa


data <- occ_data(scientificName = myspecies,
                   hasCoordinate = TRUE,
                   hasGeospatialIssue = FALSE,
                   limit = 10000)
  
data <-rbindlist(lapply(data, function(x) x$data), fill = TRUE, use.names = TRUE)

data <- data %>%
  dplyr::select(ddlat = decimalLatitude, ddlon = decimalLongitude, tax=species)%>% 
  drop_na()  %>% 
  group_by(tax) %>% 
  filter(n() >= 100)



#DENSITY MAP
plot.years <- ggplot(data = data, aes(x = ddlon, y = ddlat)) +  # plot the flickr data
  geom_map(data = world.inp,map = world.inp,aes(x = long, y = lat, map_id = region),            # plot the UK
               color = "black", fill = "gray82") + coord_fixed() +    # coord_fixed() ensures that one unit on the x-axis is the same length as one unit on the y-axis
  geom_point(color = "dodgerblue4",size = 2,shape = ".")+                   # graphical parameters for points
  stat_density2d(aes(x = ddlon,                           # create the density layer based on where the points are
                     y = ddlat,  fill = ..level.., alpha = ..level..),   # colour and transparency depend on density
                 geom = "polygon", colour = "grey95",size=0.3) +            # graphical parameters for the density layer
  scale_fill_gradient(low = "yellow", high = "red") +                 # set colour palette for density layer
  scale_alpha(range = c(.25, .5), guide = FALSE) +                    # set transparency for the density layer 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),  # don't display x and y axes labels, titles and tickmarks 
        axis.ticks.x=element_blank(),axis.title.y=element_blank(),   
        axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        text=element_text(size=18),legend.position = c(.9, .15),       # size of text and position of the legend
        panel.grid.major = element_blank(),                            # eliminates grid lines from background
        panel.background = element_blank())                            # set white background

# now plot, it takes a while!
plot.years



plots1 <- data %>% 
  group_by(tax) %>% 
  group_map(~tibble(plots=list(
    ggplot(.) + geom_map(
        data = world.inp,
        map = world.inp,
        aes(x = long, y = lat,
            map_id = region),
        fill = "grey80") + 
      xlim(min(data$ddlon, na.rm = T),
           max(data$ddlon, na.rm = T)) + 
      ylim(min(data$ddlat, na.rm = T),
           max(data$ddlat, na.rm = T)) + 
      coord_fixed() + aes(x=ddlon, y=ddlat) + geom_point() + ggtitle(.y[[1]]))))


 for (i in 1:length(plots1)) {
   jpeg(file = paste("map_", plots1[[i]][["plots"]][[1]][["labels"]][["title"]], '.jpeg'))
   print(plots1[[i]][["plots"]][[1]])
  dev.off()
}
