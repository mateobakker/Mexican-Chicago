## This is Step 2

library(tidyverse)
library(sf)
library(rmapshaper) ## for simplification of polygons

## Good data page now available here: https://ime.gob.mx/estadisticas
temp <- tempfile()
tempdir <- tempdir()
download.file("http://cloud-ime.sre.gob.mx/index.php/s/rb9cesfAr9CqoyR/download", temp)
zl <- zip::zip_list(temp)
zip::unzip(temp, zl$filename[9], exdir = tempdir)

matriculas_chicago_2021 <- readxl::read_excel(file.path(tempdir, zl$filename[9]), sheet = 3) %>% 
  set_names(c("estado", "municipio", "matrículas", "porcentaje")) %>% 
  filter(municipio != "Total") %>% 
  fill(estado, .direction = "down")

## 19 need fixin
fixed <- matriculas_chicago_2021 %>%
  mutate(estado = if_else(str_detect(estado, "^Estado de México"), "México", estado)) %>%
  left_join(municipios_geo, by = c("estado" = "state", "municipio" = "municipality")) %>%
  st_as_sf %>%
  filter(st_is_empty(geometry)) %>%
  st_drop_geometry() %>%
  select(1:4) %>%
  left_join(municipios_geo, by = c("estado" = "state_name_inegi", "municipio" = "municipality_name_inegi")) %>%
  bind_cols(tibble(cvegeo = c("08063", 15001, 15091, 11003, 11014, 14098, 17013, 17024, 17031, 17032, 19014, 19022, 19023, 19020, 19038, 20229, 20549, 29004, 30105))) %>% 
  select(cvegeo = 9, 1:3) %>%
  left_join(municipios_geo, by = "cvegeo") %>% 
  select(1:4, geometry)

## the good ones
mcas_geo <- matriculas_chicago_2021 %>% 
  mutate(estado = if_else(str_detect(estado, "^Estado de México"), "México", estado)) %>% 
  select(state = 1, municipality = 2, 3) %>% 
  left_join(municipios_geo) %>% 
  mutate(state = if_else(is.na(state), state_name_inegi, state),
         municipality = if_else(is.na(municipality), municipality_name_inegi, municipality)) %>% 
  st_as_sf %>% 
  filter(!st_is_empty(geometry)) %>% 
  bind_rows(fixed) %>% 
  st_simplify() %>% 
  select(cvegeo, 1:3, geometry) %>% 
  st_drop_geometry() %>% 
  left_join(municipios_geo, ., by = "cvegeo")  %>% 
  st_transform(crs = 4485) %>% 
  select(1, state = 2, municipality = 3, matrículas, geometry) %>% 
  mutate(matrículas = replace_na(matrículas, 0))

## What are the largest municipios de orígen?
mcas_geo %>% 
  st_drop_geometry() %>% 
  slice_max(order_by = matrículas, prop = .01)

mcas_geo_simp <- mcas_geo %>% 
  rmapshaper::ms_simplify(keep = 0.001,
                          keep_shapes = FALSE)