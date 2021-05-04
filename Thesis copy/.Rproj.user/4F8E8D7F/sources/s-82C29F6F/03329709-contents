########################################
# Thesis Script II: Erhaltungsgebiete Processing
########################################

##### loading packages
library(tidyverse)
library(sf)
library(rgdal)
library(lubridate)
library(lazyeval)
setwd("~/Desktop/Code/Thesis")

##### loading base shapefiles

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

eg <- sf_fisbroker("https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_erhaltgeb_em")
eg <- eg
eg <- eg %>% separate(gml_id, c("gml_id", "ID"), sep=17)
eg$ID <- eg$ID %>% as.numeric()

lor <- sf_fisbroker("https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_lor_plan")
lor <- lor %>% 
  separate(gml_id, c("s_lor_plan", "gml_id"), sep=11) %>%
  dplyr::select("gml_id", "PLANUNGSRAUM", "geometry")

eg$F_IN_KRAFT <- as.Date(eg$F_IN_KRAFT, format = "%d.%m.%Y")

timecalc <- function(df, from_year){
  x <- as_date(from_year)
  df$y <- as.numeric(difftime(x, df$F_IN_KRAFT, unit="weeks"))/52.25
  df$y[df$y <= 0] <- NA
  return(df)
}

eg <- timecalc(eg, "2000-12-31") %>% rename(yrs01 = y)
eg <- timecalc(eg, "2002-12-31") %>% rename(yrs03 = y)
eg <- timecalc(eg, "2004-12-31") %>% rename(yrs05 = y)
eg <- timecalc(eg, "2006-12-31") %>% rename(yrs07 = y)
eg <- timecalc(eg, "2008-12-31") %>% rename(yrs09 = y)
eg <- timecalc(eg, "2010-12-31") %>% rename(yrs11 = y)
eg <- timecalc(eg, "2012-12-31") %>% rename(yrs13 = y)
eg <- timecalc(eg, "2014-12-31") %>% rename(yrs15 = y)
eg <- timecalc(eg, "2016-12-31") %>% rename(yrs17 = y)
eg <- timecalc(eg, "2018-12-31") %>% rename(yrs19 = y)
eg <- timecalc(eg, "2021-01-01") %>% rename(yrs21 = y)

eg <- st_centroid(eg)

eg_lor <- st_join(lor, eg, join=st_intersects) %>% distinct(PLANUNGSRAUM, .keep_all = TRUE)

eg_lor_500 <- st_join(lor, st_as_sf(eg), join = st_is_within_distance, dist = 500) %>% 
  distinct(PLANUNGSRAUM, .keep_all = TRUE)
eg_lor_500 <- eg_lor_500 %>% dplyr::select("gml_id.x", "gml_id.y", "PLANUNGSRAUM", "GEBIETSNAME", "F_IN_KRAFT", "geometry")

eg_lor_500 <- timecalc(eg_lor_500, "2000-12-31") %>% rename(yrs01_500 = y)
eg_lor_500 <- timecalc(eg_lor_500, "2002-12-31") %>% rename(yrs03_500 = y)
eg_lor_500 <- timecalc(eg_lor_500, "2004-12-31") %>% rename(yrs05_500 = y)
eg_lor_500 <- timecalc(eg_lor_500, "2006-12-31") %>% rename(yrs07_500 = y)
eg_lor_500 <- timecalc(eg_lor_500, "2008-12-31") %>% rename(yrs09_500 = y)
eg_lor_500 <- timecalc(eg_lor_500, "2010-12-31") %>% rename(yrs11_500 = y)
eg_lor_500 <- timecalc(eg_lor_500, "2012-12-31") %>% rename(yrs13_500 = y)
eg_lor_500 <- timecalc(eg_lor_500, "2014-12-31") %>% rename(yrs15_500 = y)
eg_lor_500 <- timecalc(eg_lor_500, "2016-12-31") %>% rename(yrs17_500 = y)
eg_lor_500 <- timecalc(eg_lor_500, "2018-12-31") %>% rename(yrs19_500 = y)
eg_lor_500 <- timecalc(eg_lor_500, "2021-01-01") %>% rename(yrs21_500 = y)
eg_lor_500 <- eg_lor_500 %>% dplyr::select(1:3, 5:16)

eg_lor_full <- left_join(as.data.frame(eg_lor), eg_lor_500, by=c("gml_id.x"))

egdummy <- function(df, fromyear){
  x <- as_date(fromyear)
  df$y <- if_else(df$F_IN_KRAFT.x <= x, 1, 0)
  return(df)
}

eg_lor_full <- egdummy(eg_lor_full, "2000-12-31") %>% rename(iseg01 = y)
eg_lor_full <- egdummy(eg_lor_full, "2002-12-31") %>% rename(iseg03 = y)
eg_lor_full <- egdummy(eg_lor_full, "2004-12-31") %>% rename(iseg05 = y)
eg_lor_full <- egdummy(eg_lor_full, "2006-12-31") %>% rename(iseg07 = y)
eg_lor_full <- egdummy(eg_lor_full, "2008-12-31") %>% rename(iseg09 = y)
eg_lor_full <- egdummy(eg_lor_full, "2010-12-31") %>% rename(iseg11 = y)
eg_lor_full <- egdummy(eg_lor_full, "2012-12-31") %>% rename(iseg13 = y)
eg_lor_full <- egdummy(eg_lor_full, "2014-12-31") %>% rename(iseg15 = y)
eg_lor_full <- egdummy(eg_lor_full, "2016-12-31") %>% rename(iseg17 = y)
eg_lor_full <- egdummy(eg_lor_full, "2018-12-31") %>% rename(iseg19 = y)

egdumm.1 <- function(df, fromyear){
  x <- as_date(fromyear)
  df$y <- if_else(df$F_IN_KRAFT.y <= x, 1, 0)
  return(df)
}

eg_lor_full <- egdumm.1(eg_lor_full, "2000-12-31") %>% rename(iseg01_500 = y)
eg_lor_full <- egdumm.1(eg_lor_full, "2002-12-31") %>% rename(iseg03_500 = y)
eg_lor_full <- egdumm.1(eg_lor_full, "2004-12-31") %>% rename(iseg05_500 = y)
eg_lor_full <- egdumm.1(eg_lor_full, "2006-12-31") %>% rename(iseg07_500 = y)
eg_lor_full <- egdumm.1(eg_lor_full, "2008-12-31") %>% rename(iseg09_500 = y)
eg_lor_full <- egdumm.1(eg_lor_full, "2010-12-31") %>% rename(iseg11_500 = y)
eg_lor_full <- egdumm.1(eg_lor_full, "2012-12-31") %>% rename(iseg13_500 = y)
eg_lor_full <- egdumm.1(eg_lor_full, "2014-12-31") %>% rename(iseg15_500 = y)
eg_lor_full <- egdumm.1(eg_lor_full, "2016-12-31") %>% rename(iseg17_500 = y)
eg_lor_full <- egdumm.1(eg_lor_full, "2018-12-31") %>% rename(iseg19_500 = y)

eg_lor_full <- eg_lor_full %>% dplyr::select(1:3, 9, 14:24, 26:39, 41:60) %>% 
  rename(ID = gml_id.x, is_eg = gml_id.y.x, is_within_500eg = gml_id.y.y)
eg_lor_full$is_eg[!is.na(eg_lor_full$is_eg)] <- 1
eg_lor_full$is_eg[is.na(eg_lor_full$is_eg)] <- 0
eg_lor_full$is_within_500eg[!is.na(eg_lor_full$is_within_500eg)] <- 1
eg_lor_full$is_within_500eg[is.na(eg_lor_full$is_within_500eg)] <- 0
eg_lor_full$is_within_500eg[!is.na(eg_lor_full$is_within_500eg)] <- 1
eg_lor_full$is_within_500eg[is.na(eg_lor_full$is_within_500eg)] <- 0

eg_lor_full$is_eg <- as.numeric(eg_lor_full$is_eg)
eg_lor_full$is_within_500eg <- as.numeric(eg_lor_full$is_within_500eg)

eg_lor_full$is_within_500eg[eg_lor_full$is_eg==1] <- 2

save(eg_lor_full, file = "~/Desktop/Code/Thesis/Data_for_Analysis/eg.rda")
