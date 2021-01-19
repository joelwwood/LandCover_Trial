library(sf)          
library(raster)
library(tidyverse)


park_boundaries<-st_read("Data/TA_PEP_SVW_polygon.shp")


park_boundaries


plot(park_boundaries["geometry"])



wells_gray<-park_boundaries %>%
  filter(PROT_NAME=="WELLS GRAY PARK")

plot(wells_gray["geometry"])

#Code to download the .tif file for Land Cover in Canada
temp <- tempfile()
download.file("http://ftp.maps.canada.ca/pub/nrcan_rncan/Land-cover_Couverture-du-sol/canada-landcover_canada-couverture-du-sol/CanadaLandcover2015.zip",temp)
land_cover <- raster(unzip(temp)[1])
unlink(temp)

#code to load it locally
#land_cover <- raster("Data/CAN_LC_2015_CAL.tif")




wells_gray <- st_transform(wells_gray, projection(land_cover))

plot(land_cover)
plot(wells_gray["geometry"],add=TRUE)

land_cover_cropped <- as.factor(crop(land_cover, wells_gray))



type<-c("Temperate or sub-polar needleleaf forest",
        "Sub-polar taiga needleleaf forest",
        "Temperate or sub-polar broadleaf deciduous forest",
        "Mixed forest",
        "Temperate or sub-polar shrubland",
        "Temperate or sub-polar grassland", 
        "Sub-polar or polar shrubland-lichen-moss",
        #"Sub-polar or polar grassland-lichen-moss",(Wells Gray does not have)
        "Sub-polar or polar barren-lichen-moss",
        "Wetland",
        "Cropland",
        "Urban",
        "Barren lands",
        "Water", 
        "Snow and Ice")

colours<-c("darkgreen","darkolivegreen","palegreen4","forestgreen","tan4","tan2","khaki4","khaki1","cyan1","yellow","red","gray","blue","white")

x<-levels(land_cover_cropped)[[1]]
levels(land_cover_cropped)[[1]]<-cbind(levels(land_cover_cropped)[[1]], type, colours)
levels(land_cover_cropped)[[1]]$colours


plot(land_cover_cropped,col=levels(land_cover_cropped)[[1]]$colours,legend=FALSE)
plot(wells_gray["geometry"],lwd=3,add=TRUE)


zion_nlcd = raster::extract(nlcd, zion, df = TRUE, factors = TRUE) 
dplyr::select(zion_nlcd, ID, levels) %>% 
  tidyr::gather(key, value, -ID) %>%
  group_by(ID, key, value) %>%
  tally() %>% 
  tidyr::spread(value, n, fill = 0)

wells_gray_cover<- raster::extract(land_cover_cropped,wells_gray["geometry"], df=TRUE, factors=TRUE)


wells_gray_cover %>%
  dplyr::select(ID, type) %>%
  group_by(type) %>%
  tally() %>%
  mutate(area=n*30*30/1000) %>% #area in square kms
  arrange(desc(area)) %>%
  mutate(type=fct_reorder(type,area)) %>%
  ggplot(aes(type,area))+
  geom_col()+
  coord_flip()+
  ggtitle("Wells Gray Provincial Park")+
  ylab("area (km^2)")+
  xlab(NULL)+
  theme_light()
