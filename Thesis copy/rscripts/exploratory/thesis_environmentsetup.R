########################################
# Thesis Script I: Data Processing
########################################

# loading packages
library(tidyverse)
library(httr)
library(sf)
library(dplyr)
library(tmap)
library(rgdal)
library(geojsonsf)
library(raster)
library(speciesRaster)
library(fasterize)
library(ggstatsplot)
library(dismo)
library(deldir)
library(gstat)
library(automap)

setwd("~/Desktop/Code/Thesis")

####################
# creating functions to download data from FIS Broker 
####################

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

export_format <- c(
  "geojson", 
  "sqlite"
)

sf_save <- function(z, fname) {
  ifelse(!dir.exists(fname), dir.create(fname), "Folder exists already")
  ff <- paste(file.path(fname, fname), export_format, sep = ".")
  purrr::walk(ff, ~{ sf::st_write(z, .x, delete_dsn = TRUE)})
  saveRDS(z, paste0(file.path(fname, fname), ".rds"))
}

####################
# Downloading shapefiles + Bodenrichtwerte information
####################
# note: only shown to demonstrate how I obtained these shapefiles
# later in script these are loaded from environment (downloading shapefiles takes ~5 minutes)

##### Berlin polygon
# load(url("https://userpage.fu-berlin.de/soga/300/30100_data_sets/berlin_district.RData"))
# berlin.sf <- st_transform(berlin.sf, 3035)

##### Verkehrzehlen
# vz <- sf_fisbroker("https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_vz") 
# vz <- vz %>% separate(gml_id, c("s_vz.", "gml_id"), sep=5) %>% 
#  dplyr::select("gml_id", "geometry")
#vz$s_vz. <- NULL

##### LOR
#lor <- sf_fisbroker("https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_lor_plan")
#lor <- lor %>% 
#  separate(gml_id, c("s_lor_plan", "gml_id"), sep=11) %>%
#  dplyr::select("gml_id", "geometry")

##### Saving polygons for faster use
# note: not run in full script
#st_write(berlin.sf, dsn = "~/Desktop/Code/Thesis/shapefiles/berlin_sf.shp")
#st_write(vz, dsn = "~/Desktop/Code/Thesis/shapefiles/vz.shp")
#st_write(lor, dsn = "~/Desktop/Code/Thesis/shapefiles/lor.shp")

#### 
# not run
#brw2012 <- sf_fisbroker("https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_brw_2012")
#brw2011 <- sf_fisbroker("https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_brw_2011")
#brw2010 <- sf_fisbroker("https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_brw_2010")
#brw2004 <- sf_fisbroker("https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_brw_2004")
#brw2003 <- sf_fisbroker("https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_brw_2003") 
#brw2002 <- sf_fisbroker("https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_brw_2002") 

# saving brw files for offline analysis
# st_write(brw2002, dsn = "~/Desktop/Code/Thesis/brw_shapefiles/brw2002.shp")
# st_write(brw2003, dsn = "~/Desktop/Code/Thesis/brw_shapefiles/brw2003.shp")
# st_write(brw2004, dsn = "~/Desktop/Code/Thesis/brw_shapefiles/brw2004.shp")
# st_write(brw2010, dsn = "~/Desktop/Code/Thesis/brw_shapefiles/brw2010.shp")
# st_write(brw2011, dsn = "~/Desktop/Code/Thesis/brw_shapefiles/brw2011.shp")
# st_write(brw2012, dsn = "~/Desktop/Code/Thesis/brw_shapefiles/brw2012.shp")

####################
# Loading shapefiles
####################

berlin_base <- readOGR("~/Desktop/Code/Thesis/shapefiles/berlin_sf.shp")
vz <- st_as_sf(readOGR("~/Desktop/Code/Thesis/shapefiles/vz.shp"))
lor <- st_as_sf(readOGR("~/Desktop/Code/Thesis/shapefiles/lor.shp"))

brw2012 <- readOGR(dsn = "~/Desktop/Code/Thesis/brw_shapefiles/brw2012.shp")
brw2011 <- readOGR(dsn = "~/Desktop/Code/Thesis/brw_shapefiles/brw2011.shp")
brw2010 <- readOGR(dsn = "~/Desktop/Code/Thesis/brw_shapefiles/brw2010.shp")
brw2004 <- readOGR(dsn = "~/Desktop/Code/Thesis/brw_shapefiles/brw2004.shp")
brw2003 <- readOGR(dsn = "~/Desktop/Code/Thesis/brw_shapefiles/brw2003.shp")
brw2002 <- readOGR(dsn = "~/Desktop/Code/Thesis/brw_shapefiles/brw2002.shp")

# functions to make processing Bodenrichtwert data easier
brw_processing <- function(x){
  # casts objects from SpatialPolygons to sf dataframe
  x <- 
  # filters out only residential areas, only selects relevant columns, and removes empty geometries
  y <- x %>% st_as_sf() %>%
    st_transform(3035) %>%
    dplyr::filter(NUTZUNG == "W - Wohngebiet") %>%
    dplyr::select(gml_id, BRW, geometry) %>%
    filter(!st_is_empty(.))
  # removes outliers from data beyond the .95 threshold on a normal distribution
  val <- y$BRW
  yUL <- val %>% quantile(.95, na.rm = TRUE)
  out1 <- y %>%
    dplyr::filter(BRW < yUL)
  # casts the geometry of the dataframe from polygons to points for easier interpolation
  out <- st_centroid(out1)
  return(out)
}

# I think I could make this a few lines less code if I used lapply, but I can't figure out how to extract the objects from the list
brw2012 <- brw_processing(brw2012)
brw2011 <- brw_processing(brw2011)
brw2010 <- brw_processing(brw2010)

brw2004 <- brw_processing(brw2004)
brw2003 <- brw_processing(brw2003)
brw2002 <- brw_processing(brw2002)

brw2003comb <- rbind(brw2002, brw2003, brw2004) %>% as_Spatial()
brw2011comb <- rbind(brw2010, brw2011, brw2012) %>% as_Spatial()
rm(brw2002, brw2003, brw2004, brw2010, brw2011, brw2012)

####################
# Loading and cleaning demographic data from disk
####################

# For each dataset, I select relevant variables, rename them to standardized names, and convert values to numeric
########## MSS 2003
mss_2003 <- read.csv("~/Desktop/Code/Thesis/demographic/2003_MSS_cut.csv")
mss_2003 <- mss_2003 %>% separate(Verkehrszellen,
                                  c("VKZ_num", "VKZ_name"), 
                                  sep = 4)

mss_2003 <- mss_2003 %>% 
  dplyr::select("VKZ_num",
                "VKZ_name",
                "Langzeit.arbeitslose.über.1.Jahr.am.31.12.02.pro.100.EW.18.60.J.", 
                "Arbeitslose.insgesamt.31.12.02.pro.100.EW.18.60.J.",
                "Langzeitfälle..über.2.Jahre..unter.den.Sozialhilfebeziehern.pro.100.EW..",
                "Über.64.Jährige.pro.100.EW",
                "Unter.18.Jährige.pro.100.EW",
                "Ausländer.pro.100.EW",
                "EW.aus.EU.Staaten.pro.100.EW") %>%
  rename(long_unemp2003 = Langzeit.arbeitslose.über.1.Jahr.am.31.12.02.pro.100.EW.18.60.J.,
         unemp2003 = Arbeitslose.insgesamt.31.12.02.pro.100.EW.18.60.J.,
         gWelfare2003 = Langzeitfälle..über.2.Jahre..unter.den.Sozialhilfebeziehern.pro.100.EW..,
         seniors2003 = Über.64.Jährige.pro.100.EW,
         youth2003 = Unter.18.Jährige.pro.100.EW,
         foreign2003 = Ausländer.pro.100.EW,
         european2003 = EW.aus.EU.Staaten.pro.100.EW)
mss_2003[,3:9] <- lapply(mss_2003[,3:9], as.numeric)
mss_2003[339,1] <- c("Berlin")
mss_2003[339,2] <- c("Berlin")

########## MSS 2011
mss_2011 <- read.csv("~/Desktop/Code/Thesis/demographic/2011_MSS_cut.csv")
mss_2011 <- mss_2011 %>% 
  dplyr::select("Raumid",
                "Gebiet",
                "Status3", 
                "Status1",
                "E22",
                "E6",
                "E5",
                "E7",
                "E18") %>%
  rename(long_unemp2011 = Status3,
         unemp2011 = Status1,
         gWelfare2011 = E22,
         seniors2011 = E6,
         youth2011 = E5,
         foreign2011 = E7,
         european2011 = E18)
mss_2011[,3:9] <- lapply(mss_2011[,3:9], as.numeric)
