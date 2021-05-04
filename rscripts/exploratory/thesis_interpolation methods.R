####################
# Thesis Script II: Rasterization and Interpolation
####################

########## Setting up environment for rasterization
berlin_template <- raster(extent(berlin_base), 
                          resolution = 212, 
                          crs = st_crs(berlin_base)$proj4string)
##### creating Root Mean Square Error function for assessing results
RMSE <- function(observed, predicted) {
  sqrt(mean((predicted - observed)^2, na.rm=TRUE))
}
##### creating null raster to compare to different interpolation methods
null <- RMSE(mean(brw2003comb$BRW), brw2003comb$BRW)

########## METHOD 1: PROXIMITY POLYGONS

##### turning points to Thiessien 
berlin_v <- voronoi(brw2003comb)
berlin_agr <- raster::aggregate(berlin_base)
vberlin <- raster::intersect(berlin_v, berlin_agr)
b02 <- rasterize(vberlin, berlin_template, "BRW")
# plot(b02)

##### cross validating results 
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

# no idea why but I need to reinput CRS for both objects here — I think an issue with the comment system on this specific CRS
brw03_nn <- spTransform(brw2003comb, CRS=CRS("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"))
bbase_nn <- berlin_template
crs(bbase_nn) <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"

########## METHOD 2: Kriging
gs_krig <- gstat(formula=BRW~1, locations=brw2003comb)
v <- variogram(gs_krig, width=212)
fve <- fit.variogram(v, vgm(85, "Exp", 75, 20))

brw_krigd <- autoKrige(formula=BRW~1, brw03, model = vgm()$short)
brw_krigd

brw03_krigd = brw_krigd$krige_output
tmap_mode("view")
qtm(brw03_krigd)

krigrmse <- rep(NA, 5)
autok <- autofitVariogram(formula=BRW~1, brw03, model = vgm()$short)
for (k in 1:5) {
  test <- brw03[kf == k, ]
  train <- brw03[kf != k, ]
  k <- gstat(formula=OZDLYAV~1, locations=train, model=autok)
  p2 <- predict(k, newdata=test, debug.level=0)$var1.pred
  krigrmse[i] <-  RMSE(test$OZDLYAV, p2)
}


