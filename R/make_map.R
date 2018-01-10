library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(mapview)
library(sp)
library(sf)
library(htmltools)

source("not_for_git/url_vars.R")

# helper regex vars =============================================================
#* junk regex ===================================================================
junk_regex <- c("[Tt]est", "can i drop this pin\\?", "15SEP16", "test 15SEP16") %>%
  str_replace_all("^", "\\(\\^") %>%
  str_replace_all("(.)$", "\\1$\\)") %>%
  str_replace_all("\\s", "\\\\b\\\\s\\\\b") %>%
  str_c(collapse = "|")

#* problematic unicode ==========================================================
unicode_regex <- str_c("\u{00B0}", "\u{FFFD}",
                       sep = "|")

#* "mis-columned" comments ======================================================
bad_comments <- c("Tunnel entrance with signs of recent activity")

# funs ==========================================================================
prep_pins <- function(shapefile){
  shapefile %>%
    st_read(stringsAsFactors = FALSE) %>%
    `colnames<-`(c("ID", "Date", "ReportedBy", "Source", "Comments", "geometry")) %>%
    filter(!str_detect(str_trim(Comments), junk_regex)) %>%
    st_transform(crs = 4326) %>%
    mutate(Date = as.Date(Date)) %>%
    mutate(Comments = str_replace_all(Comments, unicode_regex, "")) %>%
    mutate(Comments = ifelse(is.na(Comments) & Source %in% bad_comments,
                             Source,
                             Comments)) %>%
    mutate(Comments = {Comments %>% str_replace_all("\\?{2,}", "") %>%
                                    str_replace_all("\\(\\)", "")}) %>%
    mutate(Source = ifelse(Source %in% bad_comments,
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
  attribution_regex <- "Airbus|DigitalGlobe|ImageSat"
  copy <-  "\uA9"
  
  tiles_df %>%
    mutate(date = str_extract(wms, "\\d+-\\d+-\\d+|\\d{2}-\\d{4}"),
           date = case_when(str_detect(date, "-\\d{2}_") ~ str_replace(date, "-(\\d{2})$", "-20\\1"),
                            str_detect(date, "^\\d{2}-\\d{4}$") ~ str_replace(date, 
                                                                              "(\\d{2})-(\\d{4})", 
                                                                              "\\1-01-\\2"),
                            TRUE ~ date),
           date = as.Date(date, format = "%m-%d-%Y")) %>%
    mutate(source = str_extract(wms, attribution_regex)) %>%
    mutate(source = str_extract(source, "[A-z]+")) %>%
    mutate(source = ifelse(str_detect(source, "DigitalGlobe"),
                           "Digital Globe", source)) %>%
    mutate(source = paste(copy, source, lubridate::year(date))) %>%
    arrange(desc(date)) %>%
    mutate(date = as.character(date, format = "%Y-%m-%d")) %>%
    pmap(list)
}

prep_map <- function(tile_extent){
  long <- mean(c(tile_extent$long1, tile_extent$long2))
  lat <- mean(c(tile_extent$lat1, tile_extent$lat2))
  
  leaflet(options = leafletOptions(minZoom = 3, maxZoom = 18)) %>%
    addTiles()
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
  
  if(str_detect(layer[[1]]$wms, "Myanmar")){
    leaf %>%
      addLayersControl(overlayGroups = c(map(prepped_tiles, "date"), "Expert Pins"),
                       options = layersControlOptions(collapsed = FALSE),
                       position = "topright")
  } else {
    leaf %>%
      addLayersControl(baseGroups = c(map(prepped_tiles, "date")),
                       overlayGroups = "Expert Pins",
                       options = layersControlOptions(collapsed = FALSE),
                       position = "topright")
  }
}

add_buttons <- function(tiled_map, prepped_pins, pins_extent, tiles_df, tile_extent){
  tile_bbox <- rbind(c(tile_extent$long1, tile_extent$lat1),
                     c(tile_extent$long2, tile_extent$lat2)) %>%
    as.matrix() %>%
    sp::SpatialPoints() %>%
    sp::bbox() %>%
    raster::extent()
  
  tiled_map %>%
    addMarkers(data = prepped_pins,
               popup = ~paste0("<b>Comments:</b> ", Comments, "<br>",
                               "<b>Reported by:</b> ", `Reported by`, "<br>",
                               "<b>Report Date:<b> ", Date, "<br>",
                               "<b>Source:<b> ", Source),
               group = "Expert Pins") %>%
    addHomeButton(pins_extent,
                  position = "topleft",
                  layer.name = "Expert Pins") %>%
    addHomeButton(ext = tile_bbox,
                  position = "topleft",
                  layer.name = "All Site Imagery")
}

finish_map <- function(leaf, tile_extent, prepped_line_bbox){
  line_bbox <- c(st_point(c(tile_extent$long1, tile_extent$lat1)),
                 st_point(c(tile_extent$long2, tile_extent$lat2)),
                 st_point(c(tile_extent$long1, tile_extent$lat2)),
                 st_point(c(tile_extent$long2, tile_extent$lat1))) %>% 
    st_convex_hull() %>% 
    st_cast("LINESTRING")
  
  leaf %>%
    addPolylines(data = line_bbox) %>%
    addMiniMap(zoomLevelOffset = -8,
               width = 175, height = 175,
               aimingRectOptions = list(weight = 5)) %>%
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

make_map <- function(shapefile, tiles_df, tile_extent){
  pins <- prep_pins(shapefile)
  pins_extent <- prep_pins_bbox(pins)
  prepped_tiles <- prep_tiles(tiles_df)

  prep_map(tile_extent = tile_extent) %>%
    add_tiles(prepped_tiles) %>%
    add_buttons(tiled_map = ., 
                prepped_pins = pins, 
                pins_extent =  pins_extent,
                tiles_df = tiles_df,
                tile_extent = tile_extent) %>%
    finish_map(tile_extent = tile_extent)
}


