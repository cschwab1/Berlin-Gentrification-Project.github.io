########################################
# stats project code
########################################

####################
# downloading files
####################

##########
# Downloading base Berlin shapefiles
##########

##### Berlin polygon
load(url("https://userpage.fu-berlin.de/soga/300/30100_data_sets/berlin_district.RData"))
berlin.sf <- st_transform(berlin.sf, 3035)

##### Verkehrzehlen
vz <- sf_fisbroker("https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_vz") 
vz <- vz %>% separate(gml_id, c("s_vz.", "gml_id"), sep=5) %>% 
  dplyr::select("gml_id", "geometry")
vz$s_vz. <- NULL

##### LOR
lor <- sf_fisbroker("https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_lor_plan")
lor <- lor %>% 
  separate(gml_id, c("s_lor_plan", "gml_id"), sep=11) %>%
  dplyr::select("gml_id", "geometry")

##### Saving polygons for faster use
st_write(berlin.sf, dsn = "~/Desktop/Code/Thesis/shapefiles/berlin_sf.shp")
st_write(vz, dsn = "~/Desktop/Code/Thesis/shapefiles/vz.shp")
st_write(lor, dsn = "~/Desktop/Code/Thesis/shapefiles/lor.shp")

#brw2012 <- sf_fisbroker("https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_brw_2012")
#brw2011 <- sf_fisbroker("https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_brw_2011")
#brw2010 <- sf_fisbroker("https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_brw_2010")

#brw2004 <- sf_fisbroker("https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_brw_2004")
#brw2003 <- sf_fisbroker("https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_brw_2003") 
#brw2002 <- sf_fisbroker("https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_brw_2002") 

# saving brw files for offline analysis
st_write(brw2002, dsn = "~/Desktop/Code/Thesis/brw_shapefiles/brw2002.shp")
st_write(brw2003, dsn = "~/Desktop/Code/Thesis/brw_shapefiles/brw2003.shp")
st_write(brw2004, dsn = "~/Desktop/Code/Thesis/brw_shapefiles/brw2004.shp")
st_write(brw2010, dsn = "~/Desktop/Code/Thesis/brw_shapefiles/brw2010.shp")
st_write(brw2011, dsn = "~/Desktop/Code/Thesis/brw_shapefiles/brw2011.shp")
st_write(brw2012, dsn = "~/Desktop/Code/Thesis/brw_shapefiles/brw2012.shp")

##########
# Uploading base Berlin shapefiles
##########

berlin.sf <- st_as_sf(readOGR("~/Desktop/Code/Thesis/shapefiles/berlin_sf.shp"))
vz <- st_as_sf(readOGR("~/Desktop/Code/Thesis/shapefiles/vz.shp"))
lor <- st_as_sf(readOGR("~/Desktop/Code/Thesis/shapefiles/lor.shp"))

##########
# Downloading real estate data
##########

brw_processing <- function(x){
  x <- st_as_sf(x)
  y <- x %>%
    dplyr::filter(NUTZUNG != "G - Gewerbe") %>%
    dplyr::select(gml_id, BRW, geometry) %>%
    filter(!st_is_empty(.)) 
}

brw_outliers <- function(x){
  val <- x$BRW
  yUL <- val %>% quantile(.97, na.rm = TRUE)
  
  out <- x %>%
    dplyr::filter(BRW < yUL)
  return(out)
}


brw2012 <- readOGR(dsn = "~/Desktop/Code/Thesis/brw_shapefiles/brw2012.shp")
brw2011 <- readOGR(dsn = "~/Desktop/Code/Thesis/brw_shapefiles/brw2011.shp")
brw2010 <- readOGR(dsn = "~/Desktop/Code/Thesis/brw_shapefiles/brw2010.shp")
brw2004 <- readOGR(dsn = "~/Desktop/Code/Thesis/brw_shapefiles/brw2004.shp")
brw2003 <- readOGR(dsn = "~/Desktop/Code/Thesis/brw_shapefiles/brw2003.shp")
brw2002 <- readOGR(dsn = "~/Desktop/Code/Thesis/brw_shapefiles/brw2002.shp")

brw2012 <- brw_processing(brw2012) %>% brw_outliers()
brw2011 <- brw_processing(brw2011) %>% brw_outliers()
brw2010 <- brw_processing(brw2010) %>% brw_outliers()

brw2004 <- brw_processing(brw2004) %>% brw_outliers()
brw2003 <- brw_processing(brw2003) %>% brw_outliers()
brw2002 <- brw_processing(brw2002) %>% brw_outliers()

###########
# loading and cleaning demographic data
###########

##### MSS 2003

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

##### MSS 2011
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

####################
# Rasterizing Data
####################

##### template raster
berlin_template <- raster(extent(berlin.sf), 
                          resolution = 216, 
                          crs = st_crs(berlin.sf)$proj4string)
##### real estate data
# BRW raster function
brwrast <- function(x){
  out <- fasterize(x, berlin_template, field = "BRW", background = NA)
  return(out)
}

brwfocal <- function(x){
  fweight <- focalWeight(x, d=3, type="Gauss")
  out <- focal(x, w=fweight, fun = "mean", NAonly=TRUE)
  return(out)
}

brw2012rast <- brw2012 %>% brwrast() %>% brwfocal()
brw2011rast <- brw2011 %>% brwrast() %>% brwfocal()
brw2010rast <- brw2010 %>% brwrast() %>% brwfocal()

brw2004rast <- brw2004 %>% brwrast() %>% brwfocal()
brw2003rast <- brw2003 %>% brwrast() %>% brwfocal()
brw2002rast <- brw2002 %>% brwrast() %>% brwfocal()

brw2011ave <- stack(brw2012rast, brw2011rast, brw2010rast) %>% 
  approxNA(method = "linear", rule = 2)
brw2011ave <- overlay(brw2011ave, fun = "mean")

brw2003ave <- stack(brw2004rast, brw2003rast, brw2002rast) %>% 
  approxNA(method = "linear", rule = 2)
brw2003ave <- overlay(brw2003ave, fun = "mean") 

berlinbase <- tm_shape(berlin.sf) + tm_fill(alpha = .4)
testmap <- berlinbase + tm_shape(brw2012rast) + tm_raster(alpha = .7)
testmap

##### demographic data

mss_2003sf <- left_join(mss_2003, vz, by =  c("VKZ_num" = "gml_id")) %>% 
  st_as_sf() %>% st_transform(3035) %>% na.omit() %>% filter(!st_is_empty(.))

mss_2011sf <- left_join(mss_2011, lor, by =  c("Raumid" = "gml_id")) %>%
  st_as_sf() %>% 
  st_transform(3035) %>% 
  filter(!st_is_empty(.))

unemp2003r <- rasterize(mss_2003sf, berlin_template, field = "unemp2003")
longunemp2003r <- rasterize(mss_2003sf, berlin_template, field = "long_unemp2003")
gWelfare2003r <- rasterize(mss_2003sf, berlin_template, field = "gWelfare2003")
seniors2003r <- rasterize(mss_2003sf, berlin_template, field = "seniors2003")
youth2003r <- rasterize(mss_2003sf, berlin_template, field = "youth2003")
foreign2003r <- rasterize(mss_2003sf, berlin_template, field = "foreign2003")
eu2003r <- rasterize(mss_2003sf, berlin_template, field = "european2003")

unemp2011r <- rasterize(mss_2011sf, berlin_template, field = "unemp2011")
longunemp2011r <- rasterize(mss_2011sf, berlin_template, field = "long_unemp2011")
gWelfare2011r <- rasterize(mss_2011sf, berlin_template, field = "gWelfare2011")
seniors2011r <- rasterize(mss_2011sf, berlin_template, field = "seniors2011")
youth2011r <- rasterize(mss_2011sf, berlin_template, field = "youth2011")
foreign2011r <- rasterize(mss_2011sf, berlin_template, field = "foreign2011")
eu2011r <- rasterize(mss_2011sf, berlin_template, field = "european2011")

# gotta test difference between raster alg and manual subtraction based on rows
unempChange <- unemp2011r - unemp2003r
longunempChange <- longunemp2011r - longunemp2003r
gWelfareChange <- gWelfare2011r - gWelfare2003r
seniorsChange <- seniors2011r - seniors2003r
youthChange <- youth2011r - youth2003r
foreignChange <- foreign2011r - foreign2003r
euChange <- eu2011r - eu2003r
brwChange <- brw2011ave - brw2003ave

demo_change_stack = stack(list(
  unempC = unempChange,
  longunempC = longunempChange,
  gWelfareC = gWelfareChange,
  seniorsC = seniorsChange,
  youthC = youthChange,
  foreignC = foreignChange,
  euC = euChange,
  brwC = brwChange
  )
)

bdataC <- raster::as.data.frame(demo_change_stack, xy = TRUE) %>% na.omit()

####################
# joining datasets
####################

brwstack = stack(list(ave2003 = brw2003ave, ave2011 = brw2011ave))
brwdf <- raster::as.data.frame(brwstack, xy = TRUE)

mss03stack = stack(list(
  unemp03 = unemp2003r,
  longunemp03 = longunemp2003r,
  gWelfare03 = gWelfare2003r,
  senior03 = seniors2003r,
  youth03 = youth2003r,
  foreign03 = foreign2003r,
  eu03 = eu2003r)
  )
mss03df <- raster::as.data.frame(mss03stack, xy = TRUE)

mss11stack = stack(list(
  unemp11 = unemp2011r,
  longunemp11 = longunemp2011r,
  gWelfare11 = gWelfare2011r,
  senior11 = seniors2011r,
  youth11 = youth2011r,
  foreign11 = foreign2011r,
  eu11 = eu2011r)
)
mss11df <- raster::as.data.frame(mss11stack, xy = TRUE) 

mssChange <- inner_join(mss11df, mss03df)
test <- inner_join(mssChange, brwdf) %>% na.omit()
bdata_whole <- inner_join(mssChange, brwdf)

bdata_whole$unempC <- (bdata_whole$unemp11 - bdata_whole$unemp03)
bdata_whole$longunempC <- (bdata_whole$longunemp11 - bdata_whole$longunemp03)
bdata_whole$gWelfareC <- (bdata_whole$gWelfare11 - bdata_whole$gWelfare03)
bdata_whole$seniorC <- (bdata_whole$senior11 - bdata_whole$senior03)
bdata_whole$youthC <- (bdata_whole$youth11 - bdata_whole$youth03)
bdata_whole$foreignC <- (bdata_whole$foreign11 - bdata_whole$foreign03)
bdata_whole$euC <- (bdata_whole$eu11 - bdata_whole$eu03)
bdata_whole$brwPriceC <- (bdata_whole$ave2011 - bdata_whole$ave2003)

bdataC <- bdata_whole %>% 
  dplyr::select(x, unempC, longunempC, gWelfareC, seniorC, youthC, foreignC, euC, brwPriceC) %>%
  na.omit()

summary(bdataC[,2:9]) %>% kable()
summary(demo_change[,3:10]) %>% kable()

# they produce the same results LOL

write.csv(bdataC, file="~/Desktop/Code/Thesis/bdataC.csv", row.names = FALSE)

test <- read.csv("~/Desktop/Code/Thesis/bdataC.csv")

##### cleaning environment
rm(brw2002rast, brw2003rast, brw2004rast, brw2010rast, brw2011rast, brw2012rast,
   brw2002, brw2003, brw2004, brw2010, brw2011, brw2012, 
   eu2003r, eu2011r, foreign2003r, foreign2011r, gWelfare2003r, gWelfare2011r, 
   longunemp2003r, longunemp2011r, seniors2003r, seniors2011r,
   unemp2003r, unemp2011r, lor, vz, berlin_template, berlin.sf, 
   mss_2003, mss_2011, mss_2003sf, mss_2011sf, 
   euChange, foreignChange, gWelfareChange, longunempChange, unempChange, seniorsChange,
   brwstack, brw2003ave, brw2011ave, mss03df, mss03stack, mss11df, mss11stack, mssChange)

q03 <- getbb("Berlin") %>%
  opq(datetime = "2003-01-01T00:00:000", datetime2 = "2011-01-01T00:00:000") %>%
  add_osm_feature("amenity", value = c("cafe", "nightclub", "bar"))
str(q)
gpoints <- osmdata_sf(q)




load("~/Desktop/Code/Thesis/Data_for_Analysis/eg.Rdata")
load("~/Desktop/Code/Thesis/Data_for_Analysis/lor.Rdata")

