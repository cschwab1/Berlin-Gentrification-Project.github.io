library(tidyverse)
library(httr)
library(sf)
library(dplyr)
library(tmap)
library(rgdal)
library(geojsonsf)
library(raster)
library(speciesRaster)
library(ggstatsplot)
library(dismo)
library(automap)
library(gstat)

brw2012 <- readOGR(dsn = "~/Desktop/Code/Thesis/brw_shapefiles/brw2012.shp")
brw2011 <- readOGR(dsn = "~/Desktop/Code/Thesis/brw_shapefiles/brw2011.shp")
brw2010 <- readOGR(dsn = "~/Desktop/Code/Thesis/brw_shapefiles/brw2010.shp")
brw2004 <- readOGR(dsn = "~/Desktop/Code/Thesis/brw_shapefiles/brw2004.shp")
brw2003 <- readOGR(dsn = "~/Desktop/Code/Thesis/brw_shapefiles/brw2003.shp")
brw2002 <- readOGR(dsn = "~/Desktop/Code/Thesis/brw_shapefiles/brw2002.shp")
berlin_base <- readOGR("~/Desktop/Code/Thesis/shapefiles/berlin_sf.shp")

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

brw2012 <- brw_processing(brw2012)
brw2011 <- brw_processing(brw2011)
brw2010 <- brw_processing(brw2010)

brw2004 <- brw_processing(brw2004)
brw2003 <- brw_processing(brw2003)
brw2002 <- brw_processing(brw2002)

brw2003comb <- rbind(brw2002, brw2003, brw2004) %>% as_Spatial()
brw2011comb <- rbind(brw2010, brw2011, brw2012) %>% as_Spatial()

berlin_template <- raster(extent(berlin_base), 
                          resolution = 212, 
                          crs = st_crs(berlin_base)$proj4string)

RMSE <- function(observed, predicted) { #source: Hiljmans 2016
  sqrt(mean((predicted - observed)^2, na.rm=TRUE))
}

null <- RMSE(mean(brw2003comb$BRW), brw2003comb$BRW)
null

berlin_v <- voronoi(brw2003comb)
berlin_agr <- raster::aggregate(berlin_base)
vberlin <- raster::intersect(berlin_v, berlin_agr)
b02 <- rasterize(vberlin, berlin_template, "BRW")
plot(b02)

set.seed(5132015)
kf <- kfold(nrow(brw2003comb))
rmse <- rep(NA, 5)
for (k in 1:5) {
  test <- brw2003comb[kf == k, ]
  train <- brw2003comb[kf != k, ]
  v <- voronoi(train)
  p <- raster::extract(v, test)
  rmse[k] <- RMSE(test$BRW, p$BRW)
}
1 - (mean(rmse) / null)

brw03 <- spTransform(brw2003comb, CRS=CRS("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"))


berlin_template <- raster(extent(berlin_base), 
                          resolution = 212, 
                          crs = st_crs(berlin_base)$proj4string)
bbase <- as(berlin_template, "SpatialPixelsDataFrame")
crs(bbase) <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"

brw_krigd <- autoKrige(formula=BRW~1, input_data= brw2003comb, new_data=bbase, model = vgm()$short)
brw03_pred <- brw_krigd$krige_output
brw03_pred <- raster(brw03_pred)
brw03_pred <- mask(x = brw03_pred, mask = berlin_base)

save(brw03_pred, file = "brw03_pred.rda")

save(brw_krigd, file = "brw03_krigd.rda")

gs <- gstat(formula=BRW~1, locations = brw2003comb)
v <- variogram(gs, width=500)
fve <- fit.variogram(v, vgm(c("Exp", "Sph")), fit.kappa = TRUE)
manualkrigd_brw <- gstat(formula=BRW~1, locations = brw2003comb, model=fve)
manualkrigd_brw <- predict(manualkrigd_brw, bbase)

load(file = "~/Desktop/Code/Thesis/brw03_krigd.rda")

brw03_krigd = brw_krigd$krige_output
tmap_mode("view")
qtm(brw03_krigd)

RMSE <- function(observed, predicted) {
  sqrt(mean((predicted - observed)^2, na.rm=TRUE))
}
f1 <- function(x, test, train) {
  nmx <- x[1]
  idp <- x[2]
  if (nmx < 1) return(Inf)
  if (idp < .001) return(Inf)
  m <- gstat(formula=BRW~1, locations=brw2003comb, nmax=nmx, set=list(idp=idp))
  p <- predict(m, newdata=test, debug.level=0)$var1.pred
  RMSE(test$BRW, p)
}
set.seed(20150518)
i <- sample(nrow(brw2003comb), 0.2 * nrow(brw2003comb))
tst <- brw2003comb[i,]
trn <- brw2003comb[-i,]
opt <- optim(c(8, .5), f1, test=tst, train=trn)
opt

g_idw <- gstat(formula=BRW~1, locations=brw2003comb, nmax=opt$par[1], set=list(idp = opt$par[2]))
brw_idw <- interpolate(berlin_template, g_idw)
brw_idw <- mask(brw_idw, berlin_base)
plot(brw_idw)

library(dismo)
nfolds <- 5
k <- kfold(brw2003comb, nfolds)
ensrmse <- krigrmse <- idwrmse <- rep(NA, 5)
for (i in 1:nfolds) {
  test <- brw2003comb[k!=i,]
  train <- brw2003comb[k==i,]
  m <- gstat(formula=BRW~1, locations=train, nmax=opt$par[1], set=list(idp=opt$par[2]))
  p1 <- predict(m, newdata=test, debug.level=0)$var1.pred
  idwrmse[i] <-  RMSE(test$BRW, p1)
  p2 <- predict(brw03_krigd, newdata=test, debug.level=0)$var1.pred
  krigrmse[i] <-  RMSE(test$BRW, p2)
  w <- c(idwrmse[i], krigrmse[i])
  weights <- w / sum(w)
  ensemble <- p1 * weights[1] + p2 * weights[2]
  ensrmse[i] <-  RMSE(test$BRW, ensemble)
}
rmi <- mean(idwrmse)
rmk <- mean(krigrmse)
rmt <- mean(tpsrmse)
rms <- c(rmi, rmt, rmk)

# Kriging

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

brw2004 <- readOGR(dsn = "~/Desktop/Code/Thesis/brw_shapefiles/brw2004.shp")
brw2003 <- readOGR(dsn = "~/Desktop/Code/Thesis/brw_shapefiles/brw2003.shp")
brw2002 <- readOGR(dsn = "~/Desktop/Code/Thesis/brw_shapefiles/brw2002.shp")
berlin_base <- readOGR("~/Desktop/Code/Thesis/shapefiles/berlin_sf.shp")

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


brw2004 <- brw_processing(brw2004)
brw2003 <- brw_processing(brw2003)
brw2002 <- brw_processing(brw2002)

brw2003comb <- rbind(brw2002, brw2003, brw2004) %>% as_Spatial()

berlin_template <- raster(extent(berlin_base), 
                          resolution = 212, 
                          crs = st_crs(berlin_base)$proj4string)

bbase <- as(berlin_template, "SpatialPixelsDataFrame")
crs(bbase) <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
crs(brw2003comb) <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"

gs <- gstat(formula=BRW~1, locations = brw2003comb)
v <- variogram(gs, width=500)
fve <- fit.variogram(v, vgm(c("Exp", "Sph")), fit.kappa = TRUE)
manualkrigd_brw <- gstat(formula=BRW~1, locations = brw2003comb, model=fve)
manualkrigd_brw <- predict(manualkrigd_brw, bbase)



brw2018 <- sf_fisbroker("https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_brw_2012")
brw2017 <- sf_fisbroker("https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_brw_2011")
brw2016 <- sf_fisbroker("https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_brw_2010")
brw2015 <- sf_fisbroker("https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_brw_2011")
brw2014 <- sf_fisbroker("https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_brw_2010")
brw2013 <- sf_fisbroker("https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_brw_2011")
brw2009 <- sf_fisbroker("https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_brw_2003") 
brw2008 <- sf_fisbroker("https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_brw_2004")
brw2007 <- sf_fisbroker("https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_brw_2003") 
brw2006 <- sf_fisbroker("https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_brw_2004")
brw2005 <- sf_fisbroker("https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_brw_2003") 
brw2004 <- sf_fisbroker("https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_brw_2002")

st_write(brw2005, dsn = "~/Desktop/Code/Thesis/brw_shapefiles/brw2005.shp")
st_write(brw2006, dsn = "~/Desktop/Code/Thesis/brw_shapefiles/brw2006.shp")
st_write(brw2007, dsn = "~/Desktop/Code/Thesis/brw_shapefiles/brw2007.shp")

st_write(brw2008, dsn = "~/Desktop/Code/Thesis/brw_shapefiles/brw2008.shp")
st_write(brw2009, dsn = "~/Desktop/Code/Thesis/brw_shapefiles/brw2009.shp")
st_write(brw2010, dsn = "~/Desktop/Code/Thesis/brw_shapefiles/brw2010.shp")

st_write(brw2011, dsn = "~/Desktop/Code/Thesis/brw_shapefiles/brw2011.shp")
st_write(brw2012, dsn = "~/Desktop/Code/Thesis/brw_shapefiles/brw2012.shp")
st_write(brw2013, dsn = "~/Desktop/Code/Thesis/brw_shapefiles/brw2013.shp")

st_write(brw2014, dsn = "~/Desktop/Code/Thesis/brw_shapefiles/brw2014.shp")
st_write(brw2015, dsn = "~/Desktop/Code/Thesis/brw_shapefiles/brw2015.shp")
st_write(brw2016, dsn = "~/Desktop/Code/Thesis/brw_shapefiles/brw2016.shp")

st_write(brw2017, dsn = "~/Desktop/Code/Thesis/brw_shapefiles/brw2017.shp")
st_write(brw2018, dsn = "~/Desktop/Code/Thesis/brw_shapefiles/brw2018.shp")
st_write(brw2019, dsn = "~/Desktop/Code/Thesis/brw_shapefiles/brw2019.shp")

st_write(brw2020, dsn = "~/Desktop/Code/Thesis/brw_shapefiles/brw2020.shp")

save(brw2003_krigd, file = "BRW_Krigd_rda/brw2003_krigd.rda")
save(brw2005_krigd, file = "BRW_Krigd_rda/brw2005_krigd.rda")
save(brw2007_krigd, file = "BRW_Krigd_rda/brw2007_krigd.rda")
save(brw2009_krigd, file = "BRW_Krigd_rda/brw2009_krigd.rda")
save(brw2011_krigd, file = "BRW_Krigd_rda/brw2011_krigd.rda")
save(brw2013_krigd, file = "BRW_Krigd_rda/brw2013_krigd.rda")
save(brw2015_krigd, file = "BRW_Krigd_rda/brw2015_krigd.rda")
save(brw2017_krigd, file = "BRW_Krigd_rda/brw2017_krigd.rda")
save(brw2019_krigd, file = "BRW_Krigd_rda/brw2019_krigd.rda")

