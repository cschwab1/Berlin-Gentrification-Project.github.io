########################################
# Thesis Script 2.3: Reformatting to long
########################################

library(tidyverse)
library(lubridate)
library(reshape2)

setwd("~/Desktop/Code/Thesis")
load("~/Desktop/Code/Thesis/Data_for_Analysis/bfinal.Rdata")
load("~/Desktop/Code/Thesis/Data_for_Analysis/eg.rda")
load("~/Desktop/Code/Thesis/Data_for_Analysis/pdau.Rdata")

##### some minor formatting before analysis
eg_lor_full$ID <- as.numeric(eg_lor_full$ID)
df <- left_join(bfinal, eg_lor_full, by=c("RAUMID" = "ID"))
df$F_IN_KRAFT.x <- round_date(df$F_IN_KRAFT.x, unit = "years") %>% lubridate::year() %>% as.numeric()
df$F_IN_KRAFT.y <- round_date(df$F_IN_KRAFT.y, unit = "years") %>% lubridate::year() %>% as.numeric()
df <- df[-c(2:7)]
df[is.na(df)] <- 0

# df <- df %>% filter(gen.01 != 0)

##### converting data into long format
# status and protected status 
dfg <- df[c(1, 3:12, 20:21, 33:35)]
dfL_gstatus <- reshape(dfg,
                       direction = "long",
                       varying = c("gen.01", "gen.03", "gen.05", "gen.07", "gen.09", "gen.11",
                                   "gen.13", "gen.15", "gen.17", "gen.19"),
                       v.names = "gstatus",
                       idvar = "RAUMID",
                       timevar = "Year",
                       times = c("2001", "2003", "2005", "2007", "2009", "2011", "2013", "2015", "2017", "2019")) %>%
tibble::rowid_to_column(., "ID")

dfttime <- df %>% dplyr::select(c(1, 22:31))
dfL_tt <- reshape(dfttime, 
                  direction = "long",
                  varying = list(names(dfttime)[c(2:11)]),
                  v.names = "treattime", 
                  idvar = "RAUMID",
                  timevar = "Year",
                  times = c("2001", "2003", "2005", "2007", "2009", "2011", "2013", "2015", "2017", "2019")) %>%
  tibble::rowid_to_column(., "ID") %>% dplyr::select(c(1, 4))

dfttime500 <- df %>% dplyr::select(1, 36:45)
dfL_tt500 <- reshape(dfttime500, 
                     direction = "long",
                     varying = list(names(dfttime500)[c(2:11)]),
                     v.names = "treattime500", 
                     idvar = "RAUMID",
                     timevar = "Year",
                     times = c("2001", "2003", "2005", "2007", "2009", "2011", "2013", "2015", "2017", "2019")) %>%
  tibble::rowid_to_column(., "ID") %>%  dplyr::select(c(1, 4))

dfL_isegL <- reshape(df, 
                     direction = "long",
                     varying = list(names(df)[c(47:56)]),
                     v.names = "is_eg_byyear", 
                     idvar = "RAUMID",
                     timevar = "Year",
                     times = c("2001", "2003", "2005", "2007", "2009", "2011", "2013", "2015", "2017", "2019")) %>%
  tibble::rowid_to_column(., "ID") %>%  dplyr::select(c(1, 59))

dfL_isegL500 <- reshape(df, 
                        direction = "long",
                        varying = list(names(df)[c(57:66)]),
                        v.names = "is_eg_byyear500", 
                        idvar = "RAUMID",
                        timevar = "Year",
                        times = c("2001","2003", "2005", "2007", "2009", "2011", "2013", "2015", "2017", "2019")) %>%
  tibble::rowid_to_column(., "ID") %>%  dplyr::select(c(1, 59))

load("~/Desktop/Code/Thesis/Data_for_Analysis/b_einwohner.Rdata")
pop_L <- reshape(b_einwohner,
                 direction="long", 
                 varying = list(names(b_einwohner)[c(2:11)]),
                 v.names = "pop",
                 idvar = "RAUMID",
                 timevar = "Year",
                 times = c("2001", "2003", "2005", "2007", "2009", "2011", "2013", "2015", "2017", "2019"))

# %>%
#   tibble::rowid_to_column(., "ID") %>% dplyr::select(c(1, 4))

dfL_full <- left_join(dfL_gstatus, dfL_tt, by=c("ID")) %>%
  left_join(., dfL_tt500, by=c("ID")) %>% 
  # left_join(., pdauL, by=c("ID")) %>% 
  left_join(., dfL_isegL, by=c("ID")) %>% 
  left_join(., dfL_isegL500, by=c("ID")) %>% 
  # left_join(., gaaL, by=c("ID")) %>% 
  # left_join(., euL, by=c("ID")) %>% 
  # left_join(., waL, by=c("ID")) %>% 
  left_join(., pop_L, by=c("RAUMID" = "RAUMID", "Year" = "Year"))
dfL_full[is.na(dfL_full)] <- 0
dfL_full$Year <- as.numeric(dfL_full$Year)
dfL_full$is_eg_byyear500[dfL_full$is_eg_byyear == 1] <- 2

save(dfL_full, df, file="~/Desktop/Code/Thesis/Data_for_Analysis/blong.Rdata")

#  long data for context vars ---------------------------------------------

load("~/Desktop/Code/Thesis/Data_for_Analysis/contextvars.Rdata")

dfL_aus <- reshape(baus,
                   direction = "long",
                   varying = list(names(baus)[c(3:10)]),
                   v.names = "auschange",
                   idvar = "RAUMID",
                   timevar = "Year",
                   times = c("2005", "2007", "2009", "2011", "2013", "2015", "2017", "2019"))

gaaL <- reshape(bgaa,
                direction="long",
                varying = list(names(bgaa)[c(3:10)]),
                v.names = "gaa_change",
                idvar = "RAUMID",
                timevar = "Year",
                times = c("2005", "2007", "2009", "2011", "2013", "2015", "2017", "2019"))

euL <- reshape(beu,
               direction="long",
               varying = list(names(beu)[c(3:10)]),
               v.names = "eu_change",
               idvar = "RAUMID",
               timevar = "Year",
               times = c("2005", "2007", "2009", "2011", "2013", "2015", "2017", "2019"))

waL <- reshape(bwa,
               direction="long",
               varying = list(names(bwa)[c(3:10)]),
               v.names = "WA_change",
               idvar = "RAUMID",
               timevar = "Year",
               times = c("2005", "2007", "2009", "2011", "2013", "2015", "2017", "2019"))

unempL <- reshape(bunemp,
               direction="long",
               varying = list(names(bunemp)[c(2:9)]),
               v.names = "unemp_change",
               idvar = "RAUMID",
               timevar = "Year",
               times = c("2005", "2007", "2009", "2011", "2013", "2015", "2017", "2019"))

welfL <- reshape(bwelf,
               direction="long",
               varying = list(names(bwelf)[c(2:9)]),
               v.names = "welf_change",
               idvar = "RAUMID",
               timevar = "Year",
               times = c("2005", "2007", "2009", "2011", "2013", "2015", "2017", "2019"))

E_18U25L <- reshape(bE_18U25,
                 direction="long",
                 varying = list(names(bE_18U25)[c(2:9)]),
                 v.names = "E_18U25_change",
                 idvar = "RAUMID",
                 timevar = "Year",
                 times = c("2005", "2007", "2009", "2011", "2013", "2015", "2017", "2019"))

E_25U55L <- reshape(bE_25U55,
                    direction="long",
                    varying = list(names(bE_25U55)[c(2:9)]),
                    v.names = "E_25U55_change",
                    idvar = "RAUMID",
                    timevar = "Year",
                    times = c("2005", "2007", "2009", "2011", "2013", "2015", "2017", "2019"))

E_0U6L <- reshape(bE_0U6,
                    direction="long",
                    varying = list(names(bE_0U6)[c(2:9)]),
                    v.names = "E_0U6_change",
                    idvar = "RAUMID",
                    timevar = "Year",
                    times = c("2005", "2007", "2009", "2011", "2013", "2015", "2017", "2019"))

dfL_aus$aus.03 <- euL$eu03 <- gaaL$gaa03 <- waL$WA03 <- NULL

context_varsL <- left_join(dfL_aus, euL, by=c("RAUMID", "Year")) %>% 
  left_join(., gaaL, by=c("RAUMID", "Year")) %>% 
  left_join(., waL, by=c("RAUMID", "Year")) %>% 
  left_join(., unempL, by=c("RAUMID", "Year")) %>% 
  left_join(., welfL, by=c("RAUMID", "Year")) %>% 
  left_join(., E_18U25L, by=c("RAUMID", "Year")) %>% 
  left_join(., E_25U55L, by=c("RAUMID", "Year")) %>% 
  left_join(., E_0U6L, by=c("RAUMID", "Year"))

save(context_varsL, file="~/Desktop/Code/Thesis/Data_for_Analysis/contextL.Rdata")
  





# pdauL <- reshape(bpdau,
#                  direction="long",
#                  varying = list(names(bpdau)[c(2:7)]),
#                  v.names = "pdau",
#                  idvar = "RAUMID",
#                  timevar = "Year",
#                  times = c("2009", "2011", "2013", "2015", "2017", "2019")) %>%
#   tibble::rowid_to_column(., "ID") %>% dplyr::select(c(1, 4))

# ldfs <- list(dfL_gstatus, dfL_tt, dfL_tt500, dfL_gstatus, dfg, dfttime, dfttime500, pdauL, dfL_isegL, dfL_isegL500, waL, euL, gaaL, bpdau, baus, beu, bgaa)

rm(dfL_aus, dfL_tt, dfL_tt500, dfL_gstatus, dfa, dfg, dfttime, dfttime500, pdauL, dfL_isegL, dfL_isegL500, waL, euL, gaaL, bpdau, baus, beu, bgaa)

