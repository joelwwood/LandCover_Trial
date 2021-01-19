library(tidyverse)


x<-c("Value 1, Temperate or sub-polar needleleaf forest, RGB 0 0.24 0; Value 2, Sub-polar taiga needleleaf forest, RGB 0.58 0.61 0.44; Value 5, Temperate or sub-polar broadleaf deciduous forest, RGB 0.08 0.55 0.24; Value 6, Mixed forest, RGB 0.36 0.46 0.17; Value 8, Temperate or sub-polar shrubland, RGB 0.7 0.54 0.2; Value 10, Temperate or sub-polar grassland, RGB 0.88 0.81 0.54; Value 11, Sub-polar or polar shrubland-lichen-moss, RGB 0.61 0.46 0.33; Value 12, Sub-polar or polar grassland-lichen-moss, RGB 0.73 0.83 0.56; Value 13, Sub-polar or polar barren-lichen-moss, RGB 0.25 0.54 0.45; Value 14, Wetland, RGB 0.42 0.64 0.54; Value 15, Cropland, RGB 0.9 0.68 0.4; Value 16, Barren lands, RGB 0.66 0.67 0.68; Value 17, Urban, RGB 0.86 0.13 0.15; Value 18, Water, RGB 0.3 0.44 0.64; Value 19, Snow and Ice, RGB 1 0.98 1.")

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

colours<-x1$colour