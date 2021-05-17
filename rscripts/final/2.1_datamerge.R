########################################
# Thesis Script VI: Data combining
########################################

##### loading environment
setwd("~/Desktop/Code/Thesis")
library(tidyverse)
load("Data_for_Analysis/einR.Rdata")
load("Data_for_Analysis/gaadata.Rdata")
load("Data_for_Analysis/mss.Rdata")


# merging to one dataset per year  ----------------------------------------

gaa01lor$gml_id <- as.numeric(gaa01lor$gml_id)
b01 <- left_join(einW01, gaa01lor, by=c("RAUMID" = "gml_id")) %>% 
  mutate(aus_noneu = (((E_A)/(E_E))*100)) %>% 
  rename(gaa = gaa01) %>% dplyr::select(1:6, 9, 8)

mss_2003$gml_id <- mss_2003$gml_id %>% as.numeric()
gaa03lor$gml_id <- as.numeric(gaa03lor$gml_id)
b03 <- left_join(einW03, mss_2003, by=c("RAUMID" = "gml_id")) %>% 
  left_join(., gaa03lor, by=c("RAUMID" = "gml_id")) %>% 
  dplyr::select(-"geometry") %>% 
  rename(gaa = gaa03) %>% dplyr::select(1:6, 11, 9, 8, 12, 10, 13)
b03eu <- b03[c(1, 8)]
b03wa <- b03[c(1, 7)]

mss_2005$gml_id <- as.numeric(mss_2005$gml_id)
gaa05lor$gml_id <- as.numeric(gaa05lor$gml_id)
b05 <- left_join(einW05, mss_2005, by=c("RAUMID" = "gml_id")) %>% 
  left_join(., gaa05lor, by=c("RAUMID" = "gml_id")) %>% dplyr::select(-"geometry")

gaa07lor$gml_id <- as.numeric(gaa07lor$gml_id)
b07 <- left_join(einW07, mss_2007, by=c("RAUMID" = "NR")) %>% 
  left_join(., gaa07lor, by=c("RAUMID" = "gml_id"))

mss_2009$Nr. <- as.numeric(mss_2009$Nr.)
gaa09lor$gml_id <- as.numeric(gaa09lor$gml_id)
b09 <- left_join(einW09, mss_2009, by=c("RAUMID" = "Nr.")) %>% 
  left_join(., gaa09lor, by=c("RAUMID" = "gml_id"))

mss_2011$Raumid <- as.numeric(mss_2011$Raumid)
gaa11lor$gml_id <- as.numeric(gaa11lor$gml_id)
b11 <- left_join(einW11, mss_2011, by=c("RAUMID" = "Raumid")) %>% 
  left_join(., gaa11lor, by=c("RAUMID" = "gml_id"))

gaa13lor$gml_id <- as.numeric(gaa13lor$gml_id)
b13 <- left_join(einW13, mss_2013, by=c("RAUMID" = "Nummer")) %>% 
  left_join(., gaa13lor, by=c("RAUMID" = "gml_id"))

gaa15lor$gml_id <- as.numeric(gaa15lor$gml_id)
b15 <- left_join(einW15, mss_2015, by=c("RAUMID" = "Nummer")) %>% 
  left_join(., gaa15lor, by=c("RAUMID" = "gml_id"))

gaa17lor$gml_id <- as.numeric(gaa17lor$gml_id)
b17 <- left_join(einW17, mss_2017, by=c("RAUMID" = "Nummer")) %>% 
  left_join(., gaa17lor, by=c("RAUMID" = "gml_id"))

gaa19lor$gml_id <- as.numeric(gaa19lor$gml_id)
b19 <- left_join(einW19, mss_2019, by=c("RAUMID" = "Nummer")) %>% 
  left_join(., gaa19lor, by=c("RAUMID" = "gml_id"))

rm(einW01, einW03, einW05, einW07, einW09, einW11, einW13, einW15, einW17, einW19,
   gaa01lor, gaa03lor, gaa05lor, gaa07lor, gaa09lor, gaa11lor, gaa13lor, gaa15lor, gaa17lor, gaa19lor, gaalorfull, 
   mss_2003, mss_2005, mss_2007, mss_2009, mss_2011, mss_2013, mss_2015, mss_2017, mss_2019)


# creating change dataframes ----------------------------------------------

b03c <- left_join(b03, b01, by=c("RAUMID")) %>% dplyr::select(1:6, 9, 12:19)
b03c[2:8] <- b03c[2:8] - b03c[9:15]
b03c <- b03c[1:8]
names(b03c) <- gsub(x = names(b03c), pattern = ".x", replacement = "")

b05[which.max(b05$welf),] <- mean(b05$welf)
b05$welf[max(b05$welf, na.rm=TRUE)] <- mean(b05$welf, na.rm=TRUE)
b05c <- left_join(b05, b03, by=c("RAUMID")) %>% dplyr::select(1:6, 11, 9, 8, 12, 10, 13:24) 
b05c[2:12] <- b05c[2:12] - b05c[13:23]
b05c <- b05c[1:12]
names(b05c) <- gsub(x = names(b05c), pattern = ".x", replacement = "") 
b05c <- b05c %>% rename(gaa = gaa05)

b07c <- left_join(b07, b05, by=c("RAUMID")) %>% dplyr::select(1:6, 9, 11, 10, 12:14, 15:19, 24, 22, 21, 25, 23, 26)
b07c[2:9] <- b07c[2:9] - b07c[13:20]
b07c[11:12] <- b07c[11:12] - b07c[22:23]
b07c <- b07c[1:12]

b09c <- left_join(b09, b07, by=c("RAUMID")) %>% dplyr::select(1:6, 13, 15, 14, 16:18, 8:11, 19:23, 26, 28, 27, 29:31)
b09c$welf_change <- b05$welf
b09c[2:12] <- b09c[2:12] - b09c[17:27]
b09c <- b09c[1:12]
colnames(b09c) = colnames(b07c) = colnames(b05c)

b11c <- left_join(b11, b09, by=c("RAUMID")) %>% dplyr::select(1:6, 13, 15, 14, 16:18, 8:11, 19:23, 30, 32, 31, 33:35, 25:28)
b11c[2:16] <- b11c[2:16] - b11c[17:31]
b11c <- b11c[1:16]
names(b11c) <- gsub(x = names(b11c), pattern = ".x", replacement = "")
b11c <- b11c %>% rename(gaa = gaa11)

b13c <- left_join(b13, b11, by=c("RAUMID")) %>% dplyr::select(1:6, 14, 7, 12, 15:17, 8:11, 18:22, 29, 31, 30, 32:34, 24:27)
b13c$ausnoneu <- b13c$ausnoneu*100
data.frame(colnames(b13c))
b13c[2:16] <- b13c[2:16] - b13c[17:31] 
b13c <- b13c[c(1:16)]

b15c <- left_join(b15, b13, by=c("RAUMID")) %>% dplyr::select(1:6, 14, 7, 12, 16:18, 8:11, 19:23, 31, 24, 29, 32:34, 25:28)
b15c$Name.x <- b15c$Name.y <- NULL
b15c$ausnoneu.x <- b15c$ausnoneu.x*100
b15c$ausnoneu.y <- b15c$ausnoneu.y*100
data.frame(colnames(b15c))
b15c[2:16] <- b15c[2:16] - b15c[17:31]
b15c <- b15c[c(1:16)]

b17c <- left_join(b17, b15, by=c("RAUMID")) %>% dplyr::select(1:6, 14, 7, 12, 16:18, 8:11, 19:23, 31, 24, 29, 33:35, 25:28)
data.frame(colnames(b17c))
b17c$ausnoneu.x <- b17c$ausnoneu.x*100
b17c$ausnoneu.y <- b17c$ausnoneu.y*100
b17c[2:16] <- b17c[2:16] - b17c[17:31]
b17c <- b17c[1:16]

b19c <- left_join(b19, b17, by=c("RAUMID")) %>% dplyr::select(1:6, 14, 7, 12, 16:18, 8:11, 19:23, 31, 24, 29, 33:35, 25:28)
b19c$ausnoneu.x <- b19c$ausnoneu.x*100
b19c$ausnoneu.y <- b19c$ausnoneu.y*100
b19c[2:16] <- b19c[2:16] - b19c[17:31]
b19c <- b19c[1:16]


colnames(b19c) = colnames(b17c) = colnames(b15c) = colnames(b13c) = colnames(b11c)

l <- list(b19c, b17c, b15c, b13c, b11c)
lapply(l, names)


n01 <- names(b01)
n03 <- names(b03c)
n05 <- names(b05c)
n07 <- names(b07c)
n09 <- names(b09c)
n11 <- names(b11c)
n13 <- names(b13c)
n15 <- names(b15c)
n17 <- names(b17c)
n19 <- names(b19c)
length(n01) = length(n03) = length(n05) = length(n07) = length(n09) = length(n19)

namesdf <- cbind(n01, n03, n05, n07, n09, n11, n13, n15, n17, n19) %>% as.data.frame()

save(b01, b03c, b05c, b07c, b09c, b11c, b13c, b15c, b17c, b19c, namesdf, b03eu, b03wa, file = "~/Desktop/Code/Thesis/Data_for_Analysis/datacomb.Rdata")
load("~/Desktop/Code/Thesis/Data_for_Analysis/datacomb.Rdata")
load("Data_for_Analysis/eg.rda")


# separate 10-yr resident dataframe wide-format ---------------------------

b09d <- b09 %>% dplyr::select(c(1, 10)) %>% rename(pda.09 = PDAU10)
b11d <- b11 %>% dplyr::select(c(1, 10)) %>% rename(pda.11 = PDAU10)
b13d <- b13 %>% dplyr::select(c(1, 10)) %>% rename(pda.13 = PDAU10)
b15d <- b15 %>% dplyr::select(c(1, 10)) %>% rename(pda.15 = PDAU10)
b17d <- b17 %>% dplyr::select(c(1, 10)) %>% rename(pda.17 = PDAU10)
b19d <- b19 %>% dplyr::select(c(1, 10)) %>% rename(pda.19 = PDAU10)

bpdau <- left_join(b09d, b11d, by=c("RAUMID")) %>% 
  left_join(., b13d, by=c("RAUMID")) %>% 
  left_join(., b15d, by=c("RAUMID")) %>% 
  left_join(., b17d, by=c("RAUMID")) %>% 
  left_join(., b19d, by=c("RAUMID"))

save(bpdau, file = "~/Desktop/Code/Thesis/Data_for_Analysis/pdau.Rdata")

# creating population by year wide-format
b01p <- b09 %>% dplyr::select(c(1, 2)) %>% rename(e.09 = E_E)
b03p <- b11 %>% dplyr::select(c(1, 2)) %>% rename(e.11 = E_E)
b05p <- b13 %>% dplyr::select(c(1, 2)) %>% rename(e.13 = E_E)
b07p <- b13 %>% dplyr::select(c(1, 2)) %>% rename(e.13 = E_E)
b09p <- b09 %>% dplyr::select(c(1, 2)) %>% rename(e.09 = E_E)
b11p <- b11 %>% dplyr::select(c(1, 2)) %>% rename(e.11 = E_E)
b13p <- b13 %>% dplyr::select(c(1, 2)) %>% rename(e.13 = E_E)
b15p <- b15 %>% dplyr::select(c(1, 2)) %>% rename(e.15 = E_E)
b17p <- b17 %>% dplyr::select(c(1, 2)) %>% rename(e.17 = E_E)
b19p <- b19 %>% dplyr::select(c(1, 2)) %>% rename(e.19 = E_E)

b_einwohner <- left_join(b01p, b03p, by=c("RAUMID")) %>% 
  left_join(., b05p, by=c("RAUMID")) %>% 
  left_join(., b07p, by=c("RAUMID")) %>% 
  left_join(., b09p, by=c("RAUMID")) %>% 
  left_join(., b11p, by=c("RAUMID")) %>% 
  left_join(., b13p, by=c("RAUMID")) %>% 
  left_join(., b15p, by=c("RAUMID")) %>% 
  left_join(., b17p, by=c("RAUMID")) %>% 
  left_join(., b19p, by=c("RAUMID")) 

save(b_einwohner, file = "~/Desktop/Code/Thesis/Data_for_Analysis/b_einwohner.Rdata")

btotalc <- left_join(b03, b19, by=c("RAUMID")) %>% dplyr::select(1:12, 13:17, 25, 18, 23, 27:29)
btotalc$ausnoneu <- btotalc$ausnoneu*100
btotalc$HK_EU15 <- ((btotalc$HK_EU15 / btotalc$E_E.y)*100)
btotalc[2:12] <- btotalc[2:12] - btotalc[13:23]
btotalc <- btotalc[1:12]
names(btotalc) <- names(b05c)

save(btotalc, file="~/Desktop/Code/Thesis/Data_for_Analysis/btotalc.Rdata")



