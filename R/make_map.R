library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(mapview)
library(sf)
library(htmltools)

source("not_for_git/url_vars.R")

# junk regex ====================================================================
junk_regex <-"[Tt]est|can i drop this pin"
# funs ==========================================================================
prep_pins <- function(shapefile){
  shapefile %>%
    st_read(stringsAsFactors = FALSE) %>%
    filter(!str_detect(str_trim(Comments), junk_regex)) %>%
    st_transform(crs = 4326) %>%
    mutate(Date = as.Date(Date)) %>%
    mutate(Comments = str_replace_all(Comments, "\u{00B0}|\u{FFFD}", "")) %>%
    mutate(Comments = ifelse(is.na(Comments) &
                               Source == "Tunnel entrance with signs of recent activity",
                             "Tunnel entrance with signs of recent activity",
                             Comments)) %>%
    mutate(Comments = {Comments %>% str_replace_all("\\?{2,}", "") %>%
        str_replace_all("\\(\\)", "")}) %>%
    mutate(Source = ifelse(Source == "Tunnel entrance with signs of recent activity",
                           NA, Source)) %>%
    rename(`Reported by` = ReportedBy) %>% 
    drop_na(Comments) %>%
    st_as_sf()
}

prep_pins_bbox <- function(cleaned_pins){
  cleaned_pins %>%
    as(., "Spatial") %>%
    raster::extent()
}

prep_tiles <- function(tiles_df){
  tiles_df %>%
    mutate(date = str_extract(wms, "\\d+-\\d+-\\d+|\\d{2}-\\d{4}"),
           date = case_when(str_detect(date, "-\\d{2}_") ~ str_replace(date, "-(\\d{2})$", "-20\\1"),
                            str_detect(date, "^\\d{2}-\\d{4}$") ~ str_replace(date, 
                                                                              "(\\d{2})-(\\d{4})", 
                                                                              "\\1-01-\\2"),
                            TRUE ~ date),
           date = as.Date(date, format = "%m-%d-%Y")) %>%
    mutate(source = str_extract(wms, "Airbus|DigitalGlobe|ImageSat")) %>%
    mutate(source = case_when(str_detect(source, "Airbus") ~ "Airbus",
                              str_detect(source, "DigitalGlobe") ~ "Digital Globe",
                              str_detect(source, "ImageSat") ~ "ImageSat")) %>%
    arrange(desc(date)) %>%
    mutate(date = as.character(date, format = "%Y-%m-%d")) %>%
    pmap(list)
}

prep_map <- function(){
  leaflet() %>%
    addProviderTiles(providers$OpenStreetMap.DE,
                     options = providerTileOptions(minZoom = 2, maxZoom = 13),
                     group = "base_tiles")
}

add_tiles <- function(prepped_map, prepped_tiles){
  leaf <- prepped_map
  layer <- prepped_tiles
  
  purrr::walk(prepped_tiles, function(layer) {
    leaf <<- addWMSTiles(leaf,
                         baseUrl = base_wms,
                         options = WMSTileOptions(format = "image/png",
                                                  transparent = T),
                         attribution = layer$source,
                         group = as.character(layer$date),
                         layers = layer$wms)
  })
  
  leaf %>%
    addLayersControl(baseGroups = c(map(prepped_tiles, "date")),
                     overlayGroups = "Expert Pins",
                     options = layersControlOptions(collapsed = FALSE),
                     position = "topright")
}

add_buttons <- function(tiled_map, prepped_pins,
                         map_extent, pins_extent, tiles_df){
  tiled_map %>%
    addMarkers(data = prepped_pins,
               popup = ~paste0("<b>Comments:</b> ", Comments, "<br>",
                               "<b>Reported by:</b> ", `Reported by`, "<br>",
                               "<b>Report Date:<b> ", Date, "<br>",
                               "<b>Source:<b> ", Source),
               group = "Expert Pins") %>%
    addHomeButton(pins_extent,
                  position = "topleft",
                  layer.name = "Expert Pins")
}

finish_map <- function(leaf){
  leaf %>%
    addScaleBar(position = "bottomright") %>%
    addMeasure(position = "bottomright",
               primaryLengthUnit = "meters",
               secondaryLengthUnit = "feet",
               primaryAreaUnit = "meters",
               secondaryAreaUnit = "feet") %>%
    addMouseCoordinates(style = "basic") %>% 
    addLogo(img = cns_logo,
            url = "http://www.geo4nonpro.org/",
            src = "remote",
            alpha = 0.75,
            width = 375 * 0.75, height = 82.5 * 0.75, position = "bottomleft")
}

make_map <- function(shapefile, tiles_df){
  pins <- prep_pins(shapefile)
  pins_extent <- prep_pins_bbox(pins)
  prepped_tiles <- prep_tiles(tiles_df)
  prep_map() %>%
    add_tiles(prepped_tiles) %>%
    add_buttons(tiled_map = ., 
                 prepped_pins = pins, 
                 pins_extent =  pins_extent,
                 tiles_df = tiles_df) %>%
    finish_map()
}


