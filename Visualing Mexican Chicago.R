library(tidyverse)
library(sf)
library(tmap)
library(rmapshaper)

## Get and combine polygon outlines for Mexican states and the Chicago consular district
temp <- tempfile()
tempdir <- tempdir()
download.file("http://www.conabio.gob.mx/informacion/gis/maps/geo/destdv1gw.zip", temp)
zl <- zip::zip_list(temp)
zip::unzip(temp, exdir = tempdir)
estados_geo <- read_sf(file.path(tempdir, zl$filename[5])) %>% 
  group_by(cve_edo = CVE_EDO, state = ENTIDAD) %>% 
  summarise() %>% 
  ungroup()
chicago_geo <- read_sf("/Users/mjbakker/Library/Mobile Documents/com~apple~CloudDocs/New Syncing Folder/Mapping Practice/Works in Progress/Mexican consular districts geo/Mexican consular districts geo.shp") %>% 
  filter(consulado == "Chicago") %>% 
  select(state = consulado, geometry) 

north_america_geo <- bind_rows(chicago_geo, estados_geo) %>% 
  st_simplify() %>% 
  mutate(state = str_to_title(state),
         state = case_when(
           str_detect(state, "Coah") ~ "Coahuila",
           state == "Distrito Federal" ~ "CDMX",
           str_detect(state, "Micho") ~ "Michoacán",
           str_detect(state, "Quer") ~ "Querétero",
           state == "Mexico" ~ "México",
           str_detect(state, "Leon") ~ "Nuevo León",
           str_detect(state, "Potos") ~ "San Luis Potosí",
           str_detect(state, "Vera") ~ "Veracruz",
           TRUE ~ state
           
         ))

## Set style
tmap_style("cobalt")

## Create the separate layers we'll use
map_outline <- tm_shape(north_america_geo) +
  tm_borders()
map_outline2 <- tm_shape(north_america_geo) +
  tm_borders(col = "white", lwd = .15)
map_names <- tm_shape(north_america_geo) +
  tm_text(text = "state", size = .45)  
map_connections <- tm_shape(ls_geo %>% 
                              slice_max(order_by = lwt, n = 150)) +
  tm_lines(col = "white", lwd = "lwt", legend.lwd.show = F)
map_matrículas <- tm_shape(mcas_geo_simp) +
  tm_fill("matrículas", 
          breaks = c(0, 1, 100, 250, 500, 750, 1000, 2000),
          palette = "plasma",
          title = "Consular ID Cards emitted in 2021") 

## Put it all together 
mex_chicago_map <- map_outline +
  map_matrículas +
  map_connections + 
  map_outline2 +
  map_names +
  tm_layout(title = "\nMexican Chicago\n",
            title.color = "white",
            title.bg.color = T,
            legend.title.size = .75,
            legend.height = 15, 
            title.size = 2.5) +
  tm_credits('Source: Instituto de los Mexicanos en el Exterior, "Matrículas consulares expedidas a originarios de Chicago en el 2021"\n(https://cloud-ime.sre.gob.mx/index.php/s/rb9cesfAr9CqoyR)\nAccessed: January 8, 2023',
             col = "white",
             position = c("left" ,"bottom"),
             size = .45)

mex_chicago_map
