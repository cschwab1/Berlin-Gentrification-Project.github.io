########################################
# Thesis Script 2.3: Reformatting to long
########################################

library(tidyverse)s
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
df <- df[-c(2:6)]
df[is.na(df)] <- 0

##### converting data into long format
# status and protected status 
dfg <- df[c(1, 3:12, 19:20, 32:34)]
dfL_gstatus <- reshape(dfg,
                       direction = "long",
                       varying = c("gen.01", "gen.03", "gen.05", "gen.07", "gen.09", "gen.11",
                                   "gen.13", "gen.15", "gen.17", "gen.19"),
                       v.names = "gstatus",
                       idvar = "RAUMID",
                       timevar = "Year",
                       times = c("2001", "2003", "2005", "2007", "2009", "2011", "2013", "2015", "2017", "2019")) %>%
  tibble::rowid_to_column(., "ID")

dfttime <- df %>% dplyr::select(c(1, 21:30))
dfL_tt <- reshape(dfttime, 
                  direction = "long",
                  varying = list(names(dfttime)[c(2:11)]),
                  v.names = "treattime", 
                  idvar = "RAUMID",
                  timevar = "Year",
                  times = c("2001", "2003", "2005", "2007", "2009", "2011", "2013", "2015", "2017", "2019")) %>%
  tibble::rowid_to_column(., "ID") %>% dplyr::select(c(1, 4))

dfttime500 <- df %>% dplyr::select(1, 35:44)
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
                     varying = list(names(df)[c(46:55)]),
                     v.names = "is_eg_byyear", 
                     idvar = "RAUMID",
                     timevar = "Year",
                     times = c("2001", "2003", "2005", "2007", "2009", "2011", "2013", "2015", "2017", "2019")) %>%
  tibble::rowid_to_column(., "ID") %>%  dplyr::select(c(1, 58))

dfL_isegL500 <- reshape(df, 
                        direction = "long",
                        varying = list(names(df)[c(51:59)]),
                        v.names = "is_eg_byyear500", 
                        idvar = "RAUMID",
                        timevar = "Year",
                        times = c("2003", "2005", "2007", "2009", "2011", "2013", "2015", "2017", "2019")) %>%
  tibble::rowid_to_column(., "ID") %>%  dplyr::select(c(1, 58))

# number foreigners
dfL_aus <- reshape(baus, 
                   direction = "long",
                   varying = list(names(baus)[c(2:10)]),
                   v.names = "auschange", 
                   idvar = "RAUMID",
                   timevar = "Year",
                   times = c("2003", "2005", "2007", "2009", "2011", "2013", "2015", "2017", "2019")) %>%
  tibble::rowid_to_column(., "ID") %>% dplyr::select(c(1, 4))

bpdau[8:10] <- NA 
bpdau <- bpdau %>% rename(pda.03 = V8, pda.05 = V9, pda.07 = V10) %>% dplyr::select(c(1, 8:10, 2:7))
pdauL <- reshape(bpdau,
                 direction="long", 
                 varying = list(names(bpdau)[c(2:10)]),
                 v.names = "pdau",
                 idvar = "RAUMID",
                 timevar = "Year",
                 times = c("2003", "2005", "2007", "2009", "2011", "2013", "2015", "2017", "2019")
) %>% tibble::rowid_to_column(., "ID") %>% dplyr::select(c(1, 4))

gaaL <- reshape(bgaa,
                direction="long", 
                varying = list(names(bgaa)[c(2:10)]),
                v.names = "gaa_change",
                idvar = "RAUMID",
                timevar = "Year",
                times = c("2003", "2005", "2007", "2009", "2011", "2013", "2015", "2017", "2019")
) %>% tibble::rowid_to_column(., "ID") %>% dplyr::select(c(1, 4))

euL <- reshape(beu,
               direction="long", 
               varying = list(names(beu)[c(2:10)]),
               v.names = "eu_change",
               idvar = "RAUMID",
               timevar = "Year",
               times = c("2003", "2005", "2007", "2009", "2011", "2013", "2015", "2017", "2019")
) %>% tibble::rowid_to_column(., "ID") %>% dplyr::select(c(1, 4))

waL <- reshape(bwa,
               direction="long", 
               varying = list(names(bwa)[c(2:10)]),
               v.names = "WA_change",
               idvar = "RAUMID",
               timevar = "Year",
               times = c("2003", "2005", "2007", "2009", "2011", "2013", "2015", "2017", "2019")
) %>% tibble::rowid_to_column(., "ID") %>% dplyr::select(c(1, 4))

dfL_full <- left_join(dfL_gstatus, dfL_aus, by=c("ID")) %>%
  left_join(., dfL_tt, by=c("ID")) %>%
  left_join(., dfL_tt500, by=c("ID")) %>% 
  left_join(., pdauL, by=c("ID")) %>% 
  left_join(., dfL_isegL, by=c("ID")) %>% 
  left_join(., dfL_isegL500, by=c("ID")) %>% 
  left_join(., gaaL, by=c("ID")) %>% 
  left_join(., euL, by=c("ID")) %>% 
  left_join(., waL, by=c("ID"))
dfL_full[is.na(dfL_full)] <- 0
dfL_full$Year <- as.numeric(dfL_full$Year)
dfL_full$is_eg_byyear500[dfL_full$is_eg_byyear == 1] <- 2

rm(dfL_aus, dfL_tt, dfL_tt500, dfL_gstatus, dfa, dfg, dfttime, dfttime500, pdauL, dfL_isegL, dfL_isegL500, waL, euL, gaaL, bpdau, baus, beu, bgaa)

save(dfL_full, df, file="~/Desktop/Code/Thesis/Data_for_Analysis/blong.Rdata")