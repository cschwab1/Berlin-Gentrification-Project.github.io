########################################
# Thesis Script 3.1: Visualization Maker
########################################


# setup -------------------------------------------------------------------

# loading environment 
setwd("~/Desktop/Code/Thesis")
load("~/Desktop/Code/Thesis/Data_for_Analysis/bfinal.Rdata")
load("~/Desktop/Code/Thesis/Data_for_Analysis/eg.rda")
load("~/Desktop/Code/Thesis/Data_for_Analysis/pdau.Rdata")
load("~/Desktop/Code/Thesis/Data_for_Analysis/blong.Rdata")

library(tidyverse)
library(sf)
library(tmap)
library(rgdal)
library(gifski)


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

lor1 <- sf_fisbroker("https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_lor_plan")
lor1 <- lor1 %>% 
  separate(gml_id, c("s_lor_plan", "gml_id"), sep=11) %>% 
  st_drop_geometry() %>% 
  as.data.frame()
# file previously downloaded with sf_fisbroker (see script 1.1)
lor <- st_as_sf(readOGR("~/Desktop/Code/Thesis/shapefiles/lor.shp")) %>% rename(RAUMID = gml_id)
lor <- left_join(as.data.frame(lor), lor1[c(2:6)], by=c("RAUMID" = "gml_id"))
lor$RAUMID <- as.numeric(lor$RAUMID)

# joining objects to shapefile
b_sf <- left_join(bfinal, lor, by=c("RAUMID")) %>% st_as_sf()
bfinal[, 8:16][is.na(bfinal[, 8:16])] <- 0
bwhole <- st_cast(lor, to="POLYGON")

bL_sf <- left_join(dfL_full, lor, by=c("RAUMID")) %>% st_as_sf()
bL_sf$gstatus <- as.factor(bL_sf$gstatus)


# basic maps --------------------------------------------------------------
# making basic tmap facet
breaks = c(0, 1, 2, 3, 4, 5)
stagenames = c("0 — Not yet susceptible (high-value)",
               "1 — Pre-gentrification ",
               "2 — Pioneers Invade",
               "3 — Pioneers dominate, Initial residents displaced", 
               "4 — Gentrifiers invade",
               "5 — Gentrifiers dominate, pioneers displaced")

gg <- ggplot() + 
  geom_sf(data=bL_sf, aes(fill=gstatus), size=.1) + 
  facet_wrap(~Year) + 
  scale_fill_brewer(palette = "OrRd", name="Gentrification Status", labels=stagenames) + 
  theme(legend.position = c(0.7, 0.2)) + 
  theme(axis.text.x = element_blank(), axis.text.y = element_blank()) + 
  theme(legend.key.size = unit(.3, "cm"))

# general_facetbyyear <- tm_shape(bL_sf) + 
#   tm_polygons(col = "gstatus", breaks = breaks, palette = "Oranges", title="Gentrification Status", labels=stagenames) + 
#   tm_facets(by=c("Year"), nrow = 4) + 
#   tm_layout(title = "Gentrification Status Bi-Yearly: 2003-2019",
#             # main.title.size = 20, 
#             # legend.outside=TRUE,
#             # legend.outside.size = .3,
#             # legend.text.size = 8,
#             legend.outside.position = c("right", "bottom"))
# 
# bL_sf <- bL_sf %>% rename(id = RAUMID)



  
  geom_map(data=bL_sf, map=bL_sf, aes(fill=gstatus, map_id=bL_sf, color=gstatus))


# facets aggregated to districts (Bezirk) 
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

bezirks_byyear <- bL_sf %>% 
  group_by(BEZIRKSNAME, Year) %>% 
  summarise(modestatus = Mode(gstatus))
bezirksfacet <- tm_shape(bezirks_byyear) + 
  tm_polygons(col = "modestatus", breaks = breaks, palette = "Oranges") + 
  tm_facets(by=c("Year"), nrow = 3, scale.factor = 2) + 
  tm_layout(title = "Gentrification Status Bi-Yearly: 2003-2019, by district",
            legend.position = c("right", "bottom"), 
            title.size = 5,
            frame = F)
bezirksfacet

# facet map of each district
facet_neighborhoods <- function(x){
  tm_shape(x) + 
    tm_polygons(col = "gstatus", breaks = breaks, palette = "Oranges") + 
    tm_facets(by=c("Year"), along = "BEZIRKSNAME", nrow = 3) + 
    tm_layout(title = "Gentrification Status Bi-Yearly: 2003-2019",
              legend.position = c("right", "bottom"), 
              title.size = 5,
              frame = F)
}
district_list <- split(bL_sf, bL_sf$BEZIRKSNAME)

split_bezirksfacet <- lapply(district_list, facet_neighborhoods)
save(z, file="~/Desktop/Code/Thesis/Data_for_Analysis/z.Rdata")

# animated facet maps
tmap_mode("plot")
general_gif <- tm_shape(bL_sf) + 
  tm_polygons(col = "gstatus", breaks = breaks, palette = "Oranges") + 
  tm_facets(along=c("Year"), nrow = 3) + 
  tm_layout(title = "Gentrification Status Bi-Yearly: 2003-2019",
            legend.position = c("right", "bottom"),
            frame = F)
tmap_animation(general_gif, filename = "urb_anim.gif", delay = 50)

tmap_animation(urb_anim, filename = "urb_anim.gif", delay = 50)

load("~/Desktop/Code/Thesis/Data_for_Analysis/eg.rda")
egbyyear <- eg_lor_full %>% dplyr::select(c(1, 27:35))
egbyyear[is.na(egbyyear)] <- 0
sapply(egbyyear[2:10], table) %>% kable()












