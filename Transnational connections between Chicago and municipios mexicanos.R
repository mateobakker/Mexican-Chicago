## This is Step 3

library(tidyverse)
library(sf)

consulado_chicago <- read_csv("https://transparencia.sre.gob.mx/datos_abiertos/DGSC/Ubicacion_geografica_2022.csv") %>% 
  filter(STATE == "IL") 

mcas_as_points <- matriculas_chicago_2021 %>% 
  st_drop_geometry() %>% 
  mutate(estado = if_else(str_detect(estado, "^Estado de México"), "México", estado)) %>% 
  left_join(municipios_centroids) %>% 
  st_as_sf %>% 
  filter(!st_is_empty(geometry)) %>% 
  as_tibble() %>% 
  separate(geometry, c("mex_long", "mex_lat"), sep = " ") %>% 
  mutate(across(mex_long:mex_lat, parse_number),
         chicago_long = consulado_chicago$LONGITUDE,
         chicago_lat = consulado_chicago$LATITUDE)

list_of_linestrings <- mcas_as_points %>% 
  filter(matrículas > 0) %>% 
  select(6:9) %>% 
  transpose() %>% 
  map(~ matrix(flatten_dbl(.), nrow = 2, byrow = TRUE)) %>% 
  map(st_linestring) %>% 
  st_sfc(crs = 4326)

ls_geo <- list_of_linestrings %>% 
  st_sf(geometry = .) %>% 
  bind_cols(mcas_as_points %>% 
              filter(matrículas > 0)) %>% 
  mutate(lwt = matrículas / 1000)