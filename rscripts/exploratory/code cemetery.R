
# testing land use in Berlin
landuse <- sf_fisbroker("https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_fnp")
landuse_test <- landuse 
landuse_test <- landuse_test %>% 
  filter(st_geometry_type(.) %in% c("POLYGON"))
landuse_test_percents <- aggregate()
landuse_test_percents <- landuse_test %>%
  group_by(NUTZUNGSART) %>% summarize()


#######################################################
########### Code cemetery
#######################################################

brw2003 <- st_transform(brw2003, 3035)
brw2003rast <- rasterize(brw2003, berlin_template, field = "BRW")

brw2003 <- st_as_sf(as.data.frame(brw2003))
brw2003_ext <- st_transform(brw2003, 3035)

brw2003_ext <- st_union(brw2003_ext)
brw2003_ext <- as_Spatial(brw2003, cast = TRUE)
template03 <- raster(extent(brw2003_ext), resolution = 1000, crs = st_crs(brw2003_ext)$proj4string)
brw2003_raster <- fasterize(brw2003_ext, template03, field = "BRW")

##### cycle data: testing
cycle_hire_osm_projected = st_transform(cycle_hire_osm, 27700)
raster_template = raster(extent(cycle_hire_osm_projected), resolution = 1000,
                         crs = st_crs(cycle_hire_osm_projected)$proj4string)
ch_raster3 = rasterize(cycle_hire_osm_projected, raster_template, 
                       field = 1)
ch_map <- tm_shape(ch_raster3) + tm_raster(alpha = .5)
