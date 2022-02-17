library(osmdata)
library(sf)

x <- opq("new salem massachusetts") %>% 
  add_osm_feature(key = 'highway') %>%
  osmdata_sf()

lines<-x$osm_lines

plot(st_geometry(lines))


sf::st_write(lines, "C:\\Users\\funke\\Documents\\ArcGIS\\Projects\\MyProject\\newsalemhwys.shp")

