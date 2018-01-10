source("R/make_maps.R")

russia_novaya_zemyla_tiles <- tibble(
  wms = c("Russia_NovayaZemyla:Russia_Novaya_Zemyla_08-2016_ImageSat",
          "Russia_NovayaZemyla:Russia_NovayaZemyla_05-27-2015_DigitalGlobe",
          "Russia_NovayaZemyla:Russia_NovayaZemyla_07-08-2015_DigitalGlobe",
          "Russia_NovayaZemyla:Russia_NovayaZemyla_7-28-2011_DigitalGlobe")
  )

shp <- "site_imagery/pins/Russia_NovayaZemlya_pins.shp"

map_coords_bbox <- list(long1 = 54.35486, lat1 = 73.21401, 
                        long2 = 55.23102, lat2 = 73.43469) 

make_map(shapefile = shp,
         map_coords_bbox = map_coords_bbox, 
         tiles_df = tiles_df)

