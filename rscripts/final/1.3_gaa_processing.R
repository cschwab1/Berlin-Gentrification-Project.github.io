########################################
# Thesis Script V: GAA Processing
########################################

##### Setting up environment
get_X_Y_coordinates <- function(x) {
  sftype <- as.character(sf::st_geometry_type(x, by_geometry = FALSE))
  if(sftype == "POINT") {
    xy <- as.data.frame(sf::st_coordinates(x))
    dplyr::bind_cols(x, xy)
  } else {
    x
  }
}

sf_fisbroker <- function(url) {
  typenames <- basename(url)
  url <- httr::parse_url(url)
  url$query <- list(service = "wfs",
                    version = "2.0.0",
                    request = "GetFeature",
                    srsName = "EPSG:25833",
                    TYPENAMES = typenames)
  request <- httr::build_url(url)
  print(request)
  out <- sf::read_sf(request)
  out <- sf::st_transform(out, 3035)
  out <- get_X_Y_coordinates(out)
  out <- st_as_sf(as.data.frame(out))
  return(out)
}

##### Loading GAA data
setClass("num.with.commas")
setAs("character", "num.with.commas", 
      function(from) as.numeric(gsub(",", "", from) ) )
gaa <- read.csv("~/Desktop/Code/Thesis/gaa_etwsab2000_clean.csv", 
                         colClasses=c('num.with.commas')) %>% 
  dplyr::select(1:45) %>% 
  dplyr::select("Block", "m_2000", "m_2001", "m_2002", "m_2003", "m_2004", "m_2005", 
                "m_2006", "m_2007", "m_2008", "m_2009", "m_2010", "m_2011",
                "m_2012", "m_2013", "m_2014", "m_2015", "m_2016", "m_2017", 
                "m_2018", "m_2019", "m_2020", "m_2021") 
##### Spatializing GAA data
#sb <- sf_fisbroker("https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_rbs_bloecke")
#st_write(sb, dsn="~/Desktop/Code/Thesis/shapefiles/sb.shp")
sb <-readOGR(dsn="~/Desktop/Code/Thesis/shapefiles/sb.shp") %>% st_as_sf() %>% dplyr::select("blknr", "ewk", "geometry")

#ewkfilt <- c("10-99 Einwohner", "1-9 Einwohner", "unbewohnt")
#`%notin%` <- Negate(`%in%`)
gaa <- left_join(gaa, sb, by=c("Block" = "blknr")) %>% 
  st_as_sf() #%>% 
 # filter(ewk %notin% ewkfilt)
gaa <- st_centroid(gaa) %>% filter(!st_is_empty(.))

##### Interpolating GAA data to raster
berlin_base <- readOGR("~/Desktop/Code/Thesis/shapefiles/berlin_sf.shp")
berlin_template <- raster(extent(berlin_base), 
                          resolution = 424, 
                          crs = st_crs(berlin_base)$proj4string)
bbase <- as(berlin_template, "SpatialPixels")

gaa_calc <- function(col1, col2, col3){
  newdf <- gaa[c("Block", col1, col2, col3)] %>% st_drop_geometry()
  newdf$newcol <- rowMeans(newdf[2:4], na.rm = TRUE)
  newdf <- newdf %>% filter(newcol != "NaN")
  newdf <- newdf[c("Block", "newcol")]
  newdf_sf <- left_join(newdf, gaa[-c(2:24)], by=c("Block")) %>% st_as_sf() %>% as_Spatial()
  crs(newdf_sf) <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
  x <- autoKrige(formula=newcol~1, newdf_sf, new_data=bbase, model = c("Sph", "Exp", "Gau", "Ste", "Nug"))
  y <- x$krige_output
  z <- raster(y)
  out <- mask(x = z, mask = berlin_base)
  return(out)
}

test <- gaa_calc("m_2000", "m_2001", "m_2002")
gaa01k <- gaa_calc("m_2000", "m_2001", "m_2002")
gaa03k <- gaa_calc("m_2002", "m_2003", "m_2004") 
gaa05k <- gaa_calc("m_2004", "m_2005", "m_2006")
gaa07k <- gaa_calc("m_2006", "m_2007", "m_2008")
gaa09k <- gaa_calc("m_2008", "m_2009", "m_2010")
gaa11k <- gaa_calc("m_2010", "m_2011", "m_2012")
gaa13k <- gaa_calc("m_2012", "m_2013", "m_2014")
gaa15k <- gaa_calc("m_2014", "m_2015", "m_2016")
gaa17k <- gaa_calc("m_2016", "m_2017", "m_2018")
gaa19k <- gaa_calc("m_2018", "m_2019", "m_2020")


save(gaa01k, gaa03k, gaa05k, gaa07k, gaa09k, gaa11k, gaa13k, gaa15k, gaa17k, gaa19k, file = "~/Desktop/Code/Thesis/Data_for_Analysis/gaakrigd.Rdata")

load("~/Desktop/Code/Thesis/Data_for_Analysis/gaakrigd.Rdata")

gaa01sf <- gaa01k %>% as('SpatialPolygonsDataFrame') %>% st_as_sf()
gaa03sf <- gaa03k %>% as('SpatialPolygonsDataFrame') %>% st_as_sf()
gaa05sf <- gaa05k %>% as('SpatialPolygonsDataFrame') %>% st_as_sf()
gaa07sf <- gaa07k %>% as('SpatialPolygonsDataFrame') %>% st_as_sf()
gaa09sf <- gaa09k %>% as('SpatialPolygonsDataFrame') %>% st_as_sf()
gaa11sf <- gaa11k %>% as('SpatialPolygonsDataFrame') %>% st_as_sf()
gaa13sf <- gaa13k %>% as('SpatialPolygonsDataFrame') %>% st_as_sf()
gaa15sf <- gaa15k %>% as('SpatialPolygonsDataFrame') %>% st_as_sf()
gaa17sf <- gaa17k %>% as('SpatialPolygonsDataFrame') %>% st_as_sf()
gaa19sf <- gaa19k %>% as('SpatialPolygonsDataFrame') %>% st_as_sf()

# usually this can be done via sf_fisbroker: see other final files for more info
lor <- st_as_sf(readOGR("~/Desktop/Code/Thesis/shapefiles/lor.shp"))
st_crs(lor) <- st_crs(gaa03sf)

gaalorjoin <- function(x){
  out <- st_join(lor, x, join = st_intersects, left = TRUE) %>% 
    group_by(gml_id) %>% 
    summarize(var1.pred = mean(var1.pred, na.rm=TRUE))
  return(out)
}

gaa01lor <- gaalorjoin(gaa01sf) %>% rename(gaa01 = var1.pred) %>% st_drop_geometry()
gaa03lor <- gaalorjoin(gaa03sf) %>% rename(gaa03 = var1.pred) %>% st_drop_geometry()
gaa05lor <- gaalorjoin(gaa05sf) %>% rename(gaa05 = var1.pred) %>% st_drop_geometry()
gaa07lor <- gaalorjoin(gaa07sf) %>% rename(gaa07 = var1.pred) %>% st_drop_geometry()
gaa09lor <- gaalorjoin(gaa09sf) %>% rename(gaa09 = var1.pred) %>% st_drop_geometry()
gaa11lor <- gaalorjoin(gaa11sf) %>% rename(gaa11 = var1.pred) %>% st_drop_geometry()
gaa13lor <- gaalorjoin(gaa13sf) %>% rename(gaa13 = var1.pred) %>% st_drop_geometry()
gaa15lor <- gaalorjoin(gaa15sf) %>% rename(gaa15 = var1.pred) %>% st_drop_geometry()
gaa17lor <- gaalorjoin(gaa17sf) %>% rename(gaa17 = var1.pred) %>% st_drop_geometry()
gaa19lor <- gaalorjoin(gaa19sf) %>% rename(gaa19 = var1.pred) %>% st_drop_geometry()

gaalorfull <- dplyr::left_join(gaa01lor, gaa03lor, by="gml_id") %>% 
  left_join(., gaa05lor, by="gml_id")  %>% 
  left_join(., gaa07lor, by="gml_id")  %>% 
  left_join(., gaa09lor, by="gml_id")  %>% 
  left_join(., gaa11lor, by="gml_id")  %>% 
  left_join(., gaa13lor, by="gml_id")  %>% 
  left_join(., gaa15lor, by="gml_id")  %>% 
  left_join(., gaa17lor, by="gml_id")  %>% 
  left_join(., gaa19lor, by="gml_id")  %>% 
  left_join(., lor, by="gml_id") %>% st_as_sf()

save(gaalorfull, gaa01lor, gaa03lor, gaa05lor, gaa07lor, gaa09lor, gaa11lor, gaa13lor, gaa15lor, gaa17lor, gaa19lor, file = "~/Desktop/Code/Thesis/Data_for_Analysis/gaadata.Rdata")





