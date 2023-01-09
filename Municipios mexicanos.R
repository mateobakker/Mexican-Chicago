## This is Step 1

library(tidyverse)
library(sf)


municipios_geo <- st_read("Data/Municipios mexicanos geo/muni_2018cw.shp") %>% 
  janitor::clean_names() %>% 
  select(cvegeo, geometry)

## Self-created codebook: created this to deal with the slight differences between the INEGI names/codes and those of Banxico
## state and municipality are those used in the Banxico data; inegi_municipality_name and inegi_state_name are from the INEGI names
crosswalk <- read_csv("Data/Inegi - Banxico codes crosswalk.csv")

municipios_geo <- municipios_geo  %>% 
  left_join(crosswalk) %>% 
  st_simplify()

municipios_centroids <- municipios_geo %>% 
  select(1, estado = 2, municipio = 3, geometry) %>% 
  st_centroid() %>% 
  st_transform(crs = 4326)