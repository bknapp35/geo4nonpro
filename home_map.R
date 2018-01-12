library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(mapview)
# library(rgdal)
library(sf)
library(htmltools)


dprk_sinpo <- st_point(c(128.10745, 39.95870))

russia_novaya_zemyla <- st_point(c(54.18457, 73.18384))

myanmar <- st_point(c(94.03198, 19.79772))

china_korla <- st_point(c(86.31271, 41.55253))

russia_kaliningrad <- st_point(c(21.00174, 54.47882))

dprk_punggyeri <- st_point(c(129.08421, 41.29483))

points <- st_sfc(dprk_sinpo, russia_novaya_zemyla, myanmar, china_korla, 
                 russia_kaliningrad, dprk_punggyeri) %>%
  as_tibble() %>%
  mutate(site = c("Sinpo, North Korea", "Novaya Zemlya", "Myanmar", "Korla, China",
                     "Kaliningrad, Russia", "Punggye-ri,  North Korea")) %>%
  st_as_sf()

bbox <- points %>%
  as("Spatial") %>%
  sp::bbox()
         

webmap <- leaflet(options = leafletOptions(worldCopyJump = TRUE)) %>%
  setView(0, 30, zoom = 2) %>%
  addTiles() %>%
  addMarkers(data = points,
             label = ~site, 
             popup = ~paste0("Go to ", site)) %>%
  addHomeButton(raster::extent(bbox),
                position = "topleft",
                layer.name =  "Zoom to Sites") %>%
  addMouseCoordinates(style = "basic") %>% # already appears on site
  addLogo(img = cns_logo,
          url = "http://www.geo4nonpro.org/",
          src = "remote",
          alpha = 0.75,
          width = 375 * 0.75, height = 82.5 * 0.75, position = "bottomleft")
  
             # popup = ~paste0("<b><a href='", url, "'>Go to ", site, "</a></b>")) %>%
  addHomeButton(raster::extent(points_bbox),
                position = "topleft",
                layer.name =  "Zoom to Sites") %>%
  addMouseCoordinates(style = "basic") %>% # already appears on site
  addLogo(img = cns_logo,
          url = "http://www.geo4nonpro.org/",
          src = "remote",
          alpha = 0.75,
          width = 375 * 0.75, height = 82.5 * 0.75, position = "bottomleft")



htmlwidgets::saveWidget(webmap, selfcontained = TRUE,
                        file = "~/geo4nonpro/site_imagery/R_maps/home_map/index.html")




