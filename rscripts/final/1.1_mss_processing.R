########################################
# Thesis Script IV: MSS Processing
########################################

library(tidyverse)
library(sf)
library(areal)
setwd("~/Desktop/Code/Thesis")

########## Subsetting variables and cleaning data
##### MSS03 
mss_2003 <- read.csv("~/Desktop/Code/Thesis/demographic/2003_MSS_cut.csv")
mss_2003 <- mss_2003 %>% separate(Verkehrszellen,
                                  c("VKZ_num", "VKZ_name"), 
                                  sep = 4)
mss_2003 <- mss_2003 %>% 
  dplyr::select("VKZ_name",
                "VKZ_num",
                "Wanderungs.saldo.pro.100.EW",
                "EW.mit.türkischer.Staatsangehörigkeit.pro.100.EW",
                "EW.aus.ausgewählten.arabischen.Staaten.pro.100.EW...",
                "EW.aus.den.GUS.Staaten.pro.100.EW",
                "EW.mit.polnischer.Staatsangehörigkeit.pro.100.EW",
                "EW.aus.Jugoslawien.und.dessen.Nach.folgestaaten.pro.100.EW",
                "EW.aus.EU.Staaten.pro.100.EW",
                "Arbeitslose.insgesamt.31.12.02.pro.100.EW.18.60.J.",
                "Deutsche.Sozialhilfe.Empfänger.pro.100.deutsche.EW",
                "Ausländische.Sozialhilfe.Empfänger.pro.100.ausl..EW") %>%
  rename(
    WA = Wanderungs.saldo.pro.100.EW,
    turk = EW.mit.türkischer.Staatsangehörigkeit.pro.100.EW,
    arab = EW.aus.ausgewählten.arabischen.Staaten.pro.100.EW...,
    easteuro = EW.aus.den.GUS.Staaten.pro.100.EW,
    pol = EW.mit.polnischer.Staatsangehörigkeit.pro.100.EW,
    yugo = EW.aus.Jugoslawien.und.dessen.Nach.folgestaaten.pro.100.EW,
    eu = EW.aus.EU.Staaten.pro.100.EW,
    gwelf = Deutsche.Sozialhilfe.Empfänger.pro.100.deutsche.EW,
    awelf = Ausländische.Sozialhilfe.Empfänger.pro.100.ausl..EW,
    unemp = Arbeitslose.insgesamt.31.12.02.pro.100.EW.18.60.J.
      )
mss_2003[,2:12] <- lapply(mss_2003[,2:12], as.numeric)
mss_2003 <- mss_2003[1:338,]

mss_2003$aus_noneu <- mss_2003$turk + mss_2003$arab + mss_2003$easteuro + mss_2003$pol + mss_2003$yugo
mss_2003$welf <- mss_2003$gwelf + mss_2003$awelf
mss_2003 <- dplyr::select(mss_2003, 
                   "VKZ_num",
                   "VKZ_name",
                   "WA",
                   "aus_noneu",
                   "eu",
                   "welf",
                   "unemp")

##### MSS 05 
mss_2005 <- read.csv("~/Desktop/Code/Thesis/demographic/2005_MSS_cut.csv")
mss_2005 <- mss_2005 %>% separate(Gebiet,
                                  c("VKZ_num", "VKZ_name"), 
                                  sep = 4)
mss_2005 <- mss_2005 %>% 
  dplyr::select("VKZ_name",
                "VKZ_num",
                "Wanderungs.saldo.pro.100.EW.2004",
                "EW.mit.türkischer.Staatsangehörigkeit.pro.100.EW.am.31.12.2004",
                "EW.aus.ausgewählten.arabischen.Staaten.pro.100.EW.am.31.12.2004.",
                "EW.aus.den.GUS.Staaten.pro.100.EW.am.31.12.2004",
                "EW.mit.polnischer.Staatsangehörigkeit.pro.100.EW.am.31.12.2004",
                "EW.aus.Jugoslawien.und.dessen.Nach.folgestaaten.pro.100.EW.am.31.12.2004",
                "EW.aus.EU.Staaten.pro.100.EW.am.31.12.2004",
                "Arbeitslose.insgesamt.pro.100.EW.18.60.J..am.31.12.2004",
                "Deutsche.Sozialhilfe.Empfänger.pro.100.deutsche.EW.am.31.12.2004",
                "Ausländische.Sozialhilfe.Empfänger.pro.100.ausl..EW.am.31.12.2004") %>%
  rename(
    WA = Wanderungs.saldo.pro.100.EW.2004,
    turk = EW.mit.türkischer.Staatsangehörigkeit.pro.100.EW.am.31.12.2004,
    arab = EW.aus.ausgewählten.arabischen.Staaten.pro.100.EW.am.31.12.2004.,
    easteuro = EW.aus.den.GUS.Staaten.pro.100.EW.am.31.12.2004,
    pol = EW.mit.polnischer.Staatsangehörigkeit.pro.100.EW.am.31.12.2004,
    yugo = EW.aus.Jugoslawien.und.dessen.Nach.folgestaaten.pro.100.EW.am.31.12.2004,
    eu = EW.aus.EU.Staaten.pro.100.EW.am.31.12.2004,
    gwelf = Deutsche.Sozialhilfe.Empfänger.pro.100.deutsche.EW.am.31.12.2004,
    awelf = Ausländische.Sozialhilfe.Empfänger.pro.100.ausl..EW.am.31.12.2004,
    unemp = Arbeitslose.insgesamt.pro.100.EW.18.60.J..am.31.12.2004
  )
mss_2005[,2:12] <- lapply(mss_2005[,2:12], as.numeric)
mss_2005 <- mss_2005[1:339,]

mss_2005$aus_noneu <- mss_2005$turk + mss_2005$arab + mss_2005$easteuro + mss_2005$pol + mss_2005$yugo
mss_2005$welf <- mss_2005$gwelf + mss_2005$awelf
mss_2005 <- dplyr::select(mss_2005, 
                   "VKZ_num",
                   "VKZ_name",
                   "WA",
                   "aus_noneu",
                   "eu",
                   "welf",
                   "unemp")

##### MSS 2007 
##### think I gotta skip this year — not enough data available
mss_2007 <- read.csv("~/Desktop/Code/Thesis/demographic/2007_MSS_cut.csv")
mss_2007 <- mss_2007[1:447,]
mss_2007 <- mss_2007 %>% 
  dplyr::select("Gebiet",
                "NR",
                "Dynamik2",
                "E4",
                "E5",
                "E6",
                "E7",
                "E10",
                "E8",
                "Dynamik4",
                "Dynamik5",
                "Status1") %>% 
  rename(
    WA = Dynamik2,
    turk = E4,
    arab = E5,
    easteuro = E6,
    pol = E10,
    yugo = E7,
    eu = E8,
    unemp = Status1
  )
mss_2007[,2:12] <- lapply(mss_2007[,2:12], as.numeric)
mss_2007$aus_noneu <- mss_2007$turk + mss_2007$arab + mss_2007$easteuro + mss_2007$pol + mss_2007$yugo
mss_2007$welf_change <- mss_2007$Dynamik4 + mss_2007$Dynamik5
mss_2007 <- dplyr::select(mss_2007, 
                   "NR",
                   "Gebiet",
                   "WA",
                   "aus_noneu",
                   "eu",
                   "welf_change",
                   "unemp")

##### MSS 2009
mss_2009 <- read.csv("~/Desktop/Code/Thesis/demographic/2009_MSS_cut.csv")
mss_2009 <- mss_2009[1:447,]
mss_2009 <- mss_2009 %>% 
  dplyr::select("Nr.",
                "Planungsraum",
                "Dynamik.2",
                "E.11",
                "E.12",
                "E.13",
                "E.14",
                "E.17",
                "E.15",
                "Status.1",
                "E.22", "E.23") %>% 
  rename(
    WA = Dynamik.2,
    turk = E.11,
    arab = E.12,
    easteuro = E.13,
    pol = E.17,
    yugo = E.14,
    eu = E.15,
    unemp = Status.1,
    gwelf = E.22,
    awelf = E.23
  )
mss_2009[,2:12] <- lapply(mss_2009[,2:12], as.numeric)
mss_2009$aus_noneu <- mss_2009$turk + mss_2009$arab + mss_2009$easteuro + mss_2009$pol + mss_2009$yugo
mss_2009$welf <- mss_2009$gwelf + mss_2009$awelf
mss_2009 <- dplyr::select(mss_2009, 
                   "Nr.",
                   "Planungsraum",
                   "WA",
                   "aus_noneu",
                   "eu",
                   "welf",
                   "unemp")

########## MSS 2011
mss_2011 <- read.csv("~/Desktop/Code/Thesis/demographic/2011_MSS_cut.csv")
mss_2011 <- mss_2011[1:447,]
mss_2011 <- mss_2011 %>% 
  dplyr::select("Gebiet",
                "Raumid",
                "Dynamik2",
                "E11",
                "E12",
                "E13",
                "E14",
                "E17",
                "E15",
                "E22",
                "E23",
                "Status1") %>%
  rename(    WA = Dynamik2,
             turk = E11,
             arab = E12,
             easteuro = E13,
             pol = E17,
             yugo = E14,
             eu = E15,
             unemp = Status1,
             gwelf = E22,
             awelf = E23)
mss_2011[,2:12] <- lapply(mss_2011[,2:12], as.numeric)
mss_2011$aus_noneu <- mss_2011$turk + mss_2011$arab + mss_2011$easteuro + mss_2011$pol + mss_2011$yugo
mss_2011$welf <- mss_2011$gwelf + mss_2011$awelf
mss_2011 <- dplyr::select(mss_2011, 
                   "Raumid",
                   "Gebiet",
                   "WA",
                   "aus_noneu",
                   "eu",
                   "welf",
                   "unemp")

##### MSS 2013
mss_2013 <- read.csv("~/Desktop/Code/Thesis/demographic/2013_MSS_cut.csv")
mss_2013 <- mss_2013[1:447,]
mss_2013 <- mss_2013 %>% 
  dplyr::select("Name",
                "Nummer",
                "K.12",
                "S3",
                "S1") %>%
  rename(    WA = K.12,
             unemp = S1,
             welf = S3)
mss_2013[,2:5] <- lapply(mss_2013[,2:5], as.numeric)

##### MSS 2015
mss_2015 <- read.csv("~/Desktop/Code/Thesis/demographic/2015_MSS_cut.csv")
mss_2015 <- mss_2015 %>% 
  dplyr::select("Name",
                "Nummer",
                "K.12",
                "K.17",
                "S3",
                "S1") %>%
  rename(    WA = K.12,
             noEUfor = K.17,
             unemp = S1,
             welf = S3)
mss_2015[,2:6] <- lapply(mss_2015[,2:6], as.numeric)

##### MSS 2017
mss_2017 <- read.csv("~/Desktop/Code/Thesis/demographic/2017_MSS_cut.csv")
mss_2017 <- mss_2017 %>% 
  dplyr::select("Name",
                "Nummer",
                "K.12",
                "K.17",
                "S3",
                "S1") %>%
  rename(    WA = K.12,
             noEUfor = K.17,
             unemp = S1,
             welf = S3)
mss_2017[,2:6] <- lapply(mss_2017[,2:6], as.numeric)

##### MSS 2019
mss_2019 <- read.csv("~/Desktop/Code/Thesis/demographic/2019_MSS_cut.csv")
mss_2019 <- mss_2019 %>% 
  dplyr::select("Name",
                "Nummer",
                "K.12",
                "K.17",
                "S3",
                "S1") %>%
  rename(    WA = K.12,
             noEUfor = K.17,
             unemp = S1,
             welf = S3)
mss_2019[,2:6] <- lapply(mss_2019[,2:6], as.numeric)

########## Spatially aggregating to the same scale
##### Verkehrzehlen
# vz <- sf_fisbroker("https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_vz") 
# vz <- vz %>% separate(gml_id, c("s_vz.", "gml_id"), sep=5) %>% 
#  dplyr::select("gml_id", "geometry")
#vz$s_vz. <- NULL

##### LOR
# lor <- sf_fisbroker("https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_lor_plan")
# lor <- lor %>% 
#  separate(gml_id, c("s_lor_plan", "gml_id"), sep=11) %>%
#  dplyr::select("gml_id", "geometry")

lor <- st_as_sf(readOGR("~/Desktop/Code/Thesis/shapefiles/lor.shp"))
vz <- st_as_sf(readOGR("~/Desktop/Code/Thesis/shapefiles/vz.shp"))
vz$gml_id <- as.numeric(vz$gml_id)

mss_2003 <- left_join(mss_2003, as.data.frame(vz), by=c("VKZ_num" = "gml_id")) %>% st_as_sf()
mss_2005 <- left_join(mss_2005, as.data.frame(vz), by=c("VKZ_num" = "gml_id")) %>% st_as_sf()

varnames <- c("WA", "aus_noneu","eu", "welf", "unemp")

mss_2003 <- areal::aw_interpolate(lor, mss_2003, 
                       sid = "VKZ_num", 
                       tid = "gml_id", 
                       intensive = varnames, 
                       weight="sum", 
                       output="sf")

mss_2005 <- areal::aw_interpolate(lor, mss_2005, 
                           sid = "VKZ_num", 
                           tid = "gml_id", 
                           intensive = varnames, 
                           weight="sum", 
                           output="sf")

save(mss_2003, mss_2005, mss_2007, mss_2009, mss_2011, mss_2013, mss_2015, mss_2017, mss_2019, file = "~/Desktop/Code/Thesis/Data_for_Analysis/mss.Rdata")

