
library(sf)          
library(raster)
library(tidyverse)
library(tmap)

#### Load the park boundaries shapefile (spatial vector data)
park_boundaries<-st_read("Data/TA_PEP_SVW_polygon.shp")


### List of all unique parks, protected areas, and ecological reserves

parks_areas_reserves<-park_boundaries %>%
  as_tibble() %>%
  dplyr::select(PROT_NAME,ORC_PRIMRY,AREA_SQM) %>%
  group_by(ORC_PRIMRY) %>%
  summarise(n=n(),
            area=sum(AREA_SQM)/10000,
            name=first(PROT_NAME)) %>%
  arrange(name) %>%
  arrange(-area) 


#### Code to get top5 BC Parks by area
parks<-park_boundaries %>%
  as_tibble() %>%
  filter(PROT_DESG=="PROVINCIAL PARK") %>%
  dplyr::select(PROT_NAME,ORC_PRIMRY,AREA_SQM) %>%
  group_by(ORC_PRIMRY) %>%
  summarise(n=n(),
            area=sum(AREA_SQM)/10000,
            name=first(PROT_NAME)) %>%
  arrange(name) %>%
  arrange(-area) 

#### Load the land cover raster data (if downloaded already)
land_cover <- raster("Data/CAN_LC_2015_CAL.tif")

#Add the attributes for the land cover data
#This text string defines the categorical data and corresponding colours.
x<-c("Value 1, Temperate or sub-polar needleleaf forest, RGB 0 0.24 0; Value 2, Sub-polar taiga needleleaf forest, RGB 0.58 0.61 0.44; Value 5, Temperate or sub-polar broadleaf deciduous forest, RGB 0.08 0.55 0.24; Value 6, Mixed forest, RGB 0.36 0.46 0.17; Value 8, Temperate or sub-polar shrubland, RGB 0.7 0.54 0.2; Value 10, Temperate or sub-polar grassland, RGB 0.88 0.81 0.54; Value 11, Sub-polar or polar shrubland-lichen-moss, RGB 0.61 0.46 0.33; Value 12, Sub-polar or polar grassland-lichen-moss, RGB 0.73 0.83 0.56; Value 13, Sub-polar or polar barren-lichen-moss, RGB 0.25 0.54 0.45; Value 14, Wetland, RGB 0.42 0.64 0.54; Value 15, Cropland, RGB 0.9 0.68 0.4; Value 17, Urban, RGB 0.86 0.13 0.15; Value 16, Barren lands, RGB 0.66 0.67 0.68; Value 18, Water, RGB 0.3 0.44 0.64; Value 19, Snow and Ice, RGB 1 0.98 1.")

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
  select(ID,type,colour)


#Change the CRS of the Park boundaries spatial vector to match the CRS of the land cover raster
park_boundaries <- st_transform(park_boundaries, projection(land_cover))



#### Function to get land cover totals for provincial park i

park_land_cover<-function(i){
  i_bound<-park_boundaries %>%
    filter(ORC_PRIMRY==i)
  i_cropped <- as.factor(crop(land_cover, i_bound))
  levels(i_cropped)[[1]]<-left_join(levels(i_cropped)[[1]],as.data.frame(x1))
  i_cover<- raster::extract(i_cropped,i_bound["geometry"], df=TRUE, factors=TRUE)
  
  i_cover %>%
    group_by(ID,type, colour) %>%
    tally() %>%
    mutate(hectares=n*0.09, #area in hectares (n is number of pixels; 1 pixel=900m^2=0.09Ha)
           sq_km= hectares/100) %>% #area in km^2 (1 Ha = 0.01 km^2)
    arrange(desc(sq_km)) %>%
    ungroup() %>%
    mutate(type=reorder(type,sq_km),
           ORC=i,
           name=parks_areas_reserves$name[parks_areas_reserves$ORC_PRIMRY==i]) 
}



#### Function applied to all BC Provincial Parks. Returns a tibble

park_cover_data<-parks$ORC_PRIMRY %>%
  map(park_land_cover) %>%
  bind_rows()

#calculate total area of all BC Parks (Class A, B, C) to check number with BC Gov numbers
park_cover_data %>%
    summarize(hectares=sum(hectares),
      sq_km=sum(sq_km)) #underestimates land area by 348029 hectares - but covers 96.7% of provincial park area. Could increase coverage by using weights

#summarize by each land cover type
park_cover_data %>%
  group_by(type) %>%
  summarize(sq_km=sum(sq_km)) %>%
  arrange(-sq_km)

#save the tibble
write_csv(park_cover_data, "Data/park_cover_data.csv")



#### Ecological Reserves
reserves<-park_boundaries %>%
  as_tibble() %>%
  filter(PROT_DESG=="ECOLOGICAL RESERVE") %>%
  dplyr::select(PROT_NAME,ORC_PRIMRY,AREA_SQM) %>%
  group_by(ORC_PRIMRY) %>%
  summarise(n=n(),
            area=sum(AREA_SQM)/10000,
            name=PROT_NAME) %>%
  arrange(name) %>%
  arrange(-area) 



reserve_cover_data<-reserves$ORC_PRIMRY %>%
  map(parks_land_cover) %>%
  bind_rows()

reserve_cover_data %>%
  summarize(hectares=sum(hectares),
            sq_km=sum(sq_km))

reserve_cover_data %>%
  group_by(type) %>%
  summarize(sq_km=sum(sq_km)) %>%
  arrange(-sq_km)

write_csv(reserve_cover_data, "Data/reserve_cover_data.csv")



#### Protected Areas

prot_areas<-park_boundaries %>%
  as_tibble() %>%
  filter(PROT_DESG=="PROTECTED AREA") %>%
  dplyr::select(PROT_NAME,ORC_PRIMRY,AREA_SQM) %>%
  group_by(ORC_PRIMRY) %>%
  summarise(n=n(),
            area=sum(AREA_SQM)/10000,
            name=PROT_NAME) %>%
  arrange(name) %>%
  arrange(-area) 

prot_area_cover_data<-prot_areas$ORC_PRIMRY %>%
  map(park_land_cover) %>%
  bind_rows()

prot_area_cover_data %>%
  summarize(hectares=sum(hectares),
            sq_km=sum(sq_km))

prot_area_cover_data %>%
  group_by(type) %>%
  summarize(sq_km=sum(sq_km)) %>%
  arrange(-sq_km)


write_csv(prot_area_cover_data,"Data/prot_area_cover_data.csv")
