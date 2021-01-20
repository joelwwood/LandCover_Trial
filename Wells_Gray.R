library(sf)          
library(raster)
library(tidyverse)

#Load the spatial polygon data for BC Parks and Protected Areas
park_boundaries<-st_read("Data/TA_PEP_SVW_polygon.shp")

length(park_boundaries$PROT_NAME)

plot(park_boundaries["geometry"])


#Select Wells Gray Park
wells_gray<-park_boundaries %>%
  filter(PROT_NAME=="WELLS GRAY PARK")

#plot the park boundary
plot(wells_gray["geometry"])



#Code to download the .tif file for Land Cover in Canada 2015. This is categorical raster data with 30mx30m squares
temp <- tempfile()
download.file("http://ftp.maps.canada.ca/pub/nrcan_rncan/Land-cover_Couverture-du-sol/canada-landcover_canada-couverture-du-sol/CanadaLandcover2015.zip",temp)
land_cover <- raster(unzip(temp)[1])
unlink(temp)

#code to load it locally
#land_cover <- raster("Data/CAN_LC_2015_CAL.tif")



#Change the CRS of the wells_gray geometry to match the CRS of the land_cover raster
wells_gray <- st_transform(wells_gray, projection(land_cover))

plot(land_cover)
plot(wells_gray["geometry"],add=TRUE)



#Crop the land_cover raster to be similar in size to the wells_gray polygon. 
#Cropping is useful for making nice plots, but it is also makes processing the raster much faster
#Currently R thinks that the raster data is numerical and continuous; as.factor() tells R that this data is categorical instead
land_cover_cropped <- as.factor(crop(land_cover, wells_gray))

#This text string defines the categorical data and corresponding colours.
x<-c("Value 1, Temperate or sub-polar needleleaf forest, RGB 0 0.24 0; Value 2, Sub-polar taiga needleleaf forest, RGB 0.58 0.61 0.44; Value 5, Temperate or sub-polar broadleaf deciduous forest, RGB 0.08 0.55 0.24; Value 6, Mixed forest, RGB 0.36 0.46 0.17; Value 8, Temperate or sub-polar shrubland, RGB 0.7 0.54 0.2; Value 10, Temperate or sub-polar grassland, RGB 0.88 0.81 0.54; Value 11, Sub-polar or polar shrubland-lichen-moss, RGB 0.61 0.46 0.33; Value 12, Sub-polar or polar grassland-lichen-moss, RGB 0.73 0.83 0.56; Value 13, Sub-polar or polar barren-lichen-moss, RGB 0.25 0.54 0.45; Value 14, Wetland, RGB 0.42 0.64 0.54; Value 15, Cropland, RGB 0.9 0.68 0.4; Value 16, Urban, RGB 0.86 0.13 0.15; Value 17, Barren lands, RGB 0.66 0.67 0.68; Value 18, Water, RGB 0.3 0.44 0.64; Value 19, Snow and Ice, RGB 1 0.98 1.")

#Pull out the information from the string into a usable format
x1<-x %>%
  str_replace_all(";","\n") %>%
  str_remove_all("Value ") %>%
  str_remove_all("RGB ") %>% 
  read_csv(col_names=FALSE) %>%
  rename(ID=X1,
         type=X2,
         colour=X3) %>%
  separate(colour,into=c("r","g","b"),sep=" ") %>%
  mutate(colour=rgb(r,g,b)) %>%
  select(ID,type,colour) %>%
  mutate(type=reorder(type,ID))


left_join(levels(land_cover_cropped)[[1]],as.data.frame(x1))

#add this information to the Raster Attrributes
levels(land_cover_cropped)[[1]]
levels(land_cover_cropped)[[1]]<-left_join(levels(land_cover_cropped)[[1]],as.data.frame(x1)) 
levels(land_cover_cropped)[[1]]

#Plot of the data
par(mfrow=c(1,2),xpd=TRUE)
plot(land_cover_cropped,col=levels(land_cover_cropped)[[1]]$colour,legend=FALSE)
plot(wells_gray["geometry"],lwd=3,add=TRUE,border="yellow")
plot.new()
legend("left",legend=levels(land_cover_cropped)[[1]]$type,fill=levels(land_cover_cropped)[[1]]$colour,y.intersp=0.85,cex=0.75,pch=22)


#Extract the raster data that is within the Wells Gray polygon
wells_gray_cover<- raster::extract(land_cover_cropped,wells_gray["geometry"], df=TRUE, factors=TRUE)


wells_gray_cover %>%
  dplyr::select(ID, type) %>%
  group_by(type) %>%
  tally() %>%
  mutate(hectares=n*0.09, #area in hectares (n is number of pixels; 1 pixel=900m^2=0.09Ha)
         sq_km= hectares/100) %>% #area in km^2 (1 Ha = 0.01 km^2)
  arrange(desc(sq_km)) %>%
  mutate(type=fct_reorder(type,sq_km)) %>%
  ggplot(aes(type,sq_km))+
  geom_col()+
  coord_flip()+
  ggtitle("Wells Gray Provincial Park")+
  ylab("area (km^2)")+
  xlab(NULL)+
  theme_light()
