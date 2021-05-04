########################################
# Thesis Script III: Einwohnerregister Processing
########################################

library(tidyverse)
setwd("~/Desktop/Code/Thesis")

##### Reading in files
# 2019
w_2019 <- read.table("~/Desktop/Code/Thesis/EinR/2019/WHNDAUER2018_Matrix.csv", header=TRUE, sep = ";")
aus_2019 <- read.table("~/Desktop/Code/Thesis/EinR/2019/EWRMIGRA201812H_Matrix.csv", header=TRUE, sep = ";")
ein_2019 <- read.table("~/Desktop/Code/Thesis/EinR/2019/EWR201812E_Matrix.csv", header=TRUE, sep = ";")
einW19 <- merge(ein_2019, aus_2019, by="RAUMID") %>% merge(w_2019, by="RAUMID")
einW19 <- einW19 %>% mutate(E_0U6 = E_EU1 + E_E1U6) %>%
  dplyr::select("RAUMID", "E_E", 
                 # helpful age ranges
                 "E_E18U25", "E_E25U55", "E_EU1", "E_0U6",
                 # foreigners 
                 "HK_EU15", "HK_Polen", "HK_EheJug", "HK_EheSU",   
                 "HK_Turk", "HK_Arab", "HK_Sonst",
                 # people who have been living in the neighborhood for 5-10 years
                 "Dau10", "Dau5", "PDAU10", "PDAU5")
einW19$PDAU10 <- gsub(",", ".", einW19$PDAU10) %>% as.numeric()
einW19$PDAU5 <- gsub(",", ".", einW19$PDAU5) %>% as.numeric()
einW19$ausnoneu <- (einW19$HK_Arab + einW19$HK_Polen + einW19$HK_EheJug + einW19$HK_Turk + einW19$HK_Sonst)/einW19$E_E
einW19 <- einW19[c(1:7, 14:18)]
rm(w_2019, aus_2019, ein_2019)

# 2017
w_2017 <- read.table("~/Desktop/Code/Thesis/EinR/2017/WHNDAUER2016_Matrix.csv", header=TRUE, sep = ";")
aus_2017 <- read.table("~/Desktop/Code/Thesis/EinR/2017/EWRMIGRA201612H_Matrix.csv", header=TRUE, sep = ";")
ein_2017 <- read.table("~/Desktop/Code/Thesis/EinR/2017/EWR201612E_Matrix.csv", header=TRUE, sep = ";")
einW17 <- merge(ein_2017, aus_2017, by="RAUMID") %>% merge(w_2017, by="RAUMID")
einW17 <- einW17 %>% mutate(E_0U6 = E_EU1 + E_E1U6) %>%
  dplyr::select("RAUMID", "E_E", 
                 # helpful age ranges
                 "E_E18U25", "E_E25U55", "E_EU1", "E_0U6",
                 # foreigners 
                 "HK_EU15", "HK_Polen", "HK_EheJug", "HK_EheSU",   
                 "HK_Turk", "HK_Arab", "HK_Sonst",
                 # people who have been living in the neighborhood for 5-10 years
                 "Dau10", "Dau5", "PDAU10", "PDAU5")
einW17$PDAU10 <- gsub(",", ".", einW17$PDAU10) %>% as.numeric()
einW17$Dau10 <- as.numeric(einW17$Dau10)
einW17$Dau5 <- as.numeric(einW17$Dau5)
einW17$PDAU10 <- gsub(",", ".", einW17$PDAU10) %>% as.numeric()
einW17$PDAU5 <- gsub(",", ".", einW17$PDAU5) %>% as.numeric()
einW17$ausnoneu <- (einW17$HK_Arab + einW17$HK_Polen + einW17$HK_EheJug + einW17$HK_Turk + einW17$HK_Sonst)/einW17$E_E
einW17 <- einW17[c(1:7, 14:18)]
rm(ein_2017, aus_2017, w_2017)

# 2015
w_2015 <- read.table("~/Desktop/Code/Thesis/EinR/2015/WHNDAUER2014_Matrix.csv", header=TRUE, sep = ";")
aus_2015 <- read.table("~/Desktop/Code/Thesis/EinR/2015/EWRMIGRA201412H_Matrix.csv", header=TRUE, sep = ";")
ein_2015 <- read.table("~/Desktop/Code/Thesis/EinR/2015/EWR201412E_Matrix.csv", header=TRUE, sep = ";")
einW15 <- merge(ein_2015, aus_2015, by="RAUMID") %>% merge(w_2015, by="RAUMID")
einW15 <- einW15 %>% mutate(E_0U6 = E_EU1 + E_E1U6) %>%
  dplyr::select("RAUMID", "E_E", 
                 # helpful age ranges
                 "E_E18U25", "E_E25U55", "E_EU1", "E_0U6",
                 # foreigners 
                 "HK_EU15", "HK_Polen", "HK_EheJug", "HK_EheSU",   
                 "HK_Turk", "HK_Arab", "HK_Sonst",
                 # people who have been living in the neighborhood for 5-10 years
                 "DAU10", "DAU5", "PDAU10", "PDAU5")
einW15$PDAU10 <- gsub(",", ".", einW15$PDAU10) %>% as.numeric()
einW15$PDAU5 <- gsub(",", ".", einW15$PDAU5) %>% as.numeric()
einW15[1:17] <-lapply(einW15[1:17], as.numeric)
einW15$ausnoneu <- (einW15$HK_Arab + einW15$HK_Polen + einW15$HK_EheJug + einW15$HK_Turk + einW15$HK_Sonst)/(einW15$E_E)
einW15 <- einW15[c(1:7, 14:18)]
rm(ein_2015, aus_2015, w_2015)

# 2013
w_2013 <- read.table("~/Desktop/Code/Thesis/EinR/2013/WHNDAUER2012_Matrix.csv", header=TRUE, sep = ";")
aus_2013 <- read.table("~/Desktop/Code/Thesis/EinR/2013/EWRMIGRA201212H_Matrix.csv", header=TRUE, sep = ";")
ein_2013 <- read.table("~/Desktop/Code/Thesis/EinR/2013/EWR201212E_Matrix.csv", header=TRUE, sep = ";")
einW13 <- merge(ein_2013, aus_2013, by="RAUMID") %>% merge(w_2013, by="RAUMID")
einW13$E_U1 <- as.numeric(einW13$E_U1)
einW13$E_1U6 <- as.numeric(einW13$E_1U6)
einW13 <- einW13 %>% mutate(E_0U6 = E_U1 + E_1U6) %>%
  dplyr::select("RAUMID", "E_E", 
                 # helpful age ranges
                 "E_18U25", "E_25U55", "E_U1", "E_0U6",
                 # foreigners 
                 "HK_EU15", "HK_Polen", "HK_EheJug", "HK_EheSU",   
                 "HK_Turk", "HK_Arab", "HK_Sonst",
                 # people who have been living in the neighborhood for 5-10 years
                 "DAU10", "DAU5", "PDAU10", "PDAU5") %>% rename(eu = HK_EU15)
einW13[1:17] <-lapply(einW13[1:17], gsub, pattern = ",", replacement =  ".")
einW13[1:17] <-lapply(einW13[1:17], as.numeric)
einW13$ausnoneu <- (einW13$HK_Arab + einW13$HK_Polen + einW13$HK_EheJug + einW13$HK_Turk + einW13$HK_Sonst)/(einW13$E_E)
einW13 <- einW13[c(1:7, 14:18)]
rm(ein_2013, aus_2013, w_2013)

# 2011
w_2011 <- read.table("~/Desktop/Code/Thesis/EinR/2011/WHNDAUER2010_Matrix.csv", header=TRUE, sep = ";")
aus_2011 <- read.table("~/Desktop/Code/Thesis/EinR/2011/EWR201012A_Matrix.csv", header=TRUE, sep = ";")
ein_2011 <- read.table("~/Desktop/Code/Thesis/EinR/2011/EWR201012E_Matrix.csv", header=TRUE, sep = ";")
einW11 <- merge(ein_2011, aus_2011, by="RAUMID") %>% merge(w_2011, by="RAUMID")
einW11 <- einW11 %>% mutate(E_0U6 = E_U1 + E_1U6) %>%
  dplyr::select("RAUMID", "E_E", 
                 # helpful age ranges
                 "E_18U25", "E_25U55", "E_U1", "E_0U6",
                 # foreigners 
                 "E_A", 
                 # people who have been living in the neighborhood for 5-10 years
                 "DAU10", "DAU5", "PDAU10", "PDAU5")
einW11$PDAU10 <- gsub(",", ".", einW11$PDAU10) %>% as.numeric()
einW11$PDAU5 <- gsub(",", ".", einW11$PDAU5) %>% as.numeric()
rm(ein_2011, aus_2011, w_2011)
# 2009
w_2009 <- read.table("~/Desktop/Code/Thesis/EinR/2009/WHNDAUER2008_Matrix.csv", header=TRUE, sep = ";")
aus_2009 <- read.table("~/Desktop/Code/Thesis/EinR/2009/EWR200812A_Matrix.csv", header=TRUE, sep = ";")
ein_2009 <- read.table("~/Desktop/Code/Thesis/EinR/2009/EWR200812E_Matrix.csv", header=TRUE, sep = ";")
einW09 <- merge(ein_2009, aus_2009, by="RAUMID") %>% merge(w_2009, by="RAUMID")
einW09 <- einW09 %>% mutate(E_0U6 = E_U1 + E_1U6) %>%
  dplyr::select("RAUMID", "E_E", 
                 # helpful age ranges
                 "E_18U25", "E_25U55", "E_U1", "E_0U6",
                 # foreigners 
                 "E_A", 
                 # people who have been living in the neighborhood for 5-10 years
                 "DAU10", "DAU5", "PDAU10", "PDAU5")
einW09$PDAU10 <- gsub(",", ".", einW09$PDAU10) %>% as.numeric()
einW09$PDAU5 <- gsub(",", ".", einW09$PDAU5) %>% as.numeric()
rm(ein_2009, aus_2009, w_2009)

# 2007
aus_2007 <- read.table("~/Desktop/Code/Thesis/EinR/2007/EWR200612A_Matrix.csv", header=TRUE, sep = ";")
ein_2007 <- read.table("~/Desktop/Code/Thesis/EinR/2007/EWR200612E_Matrix.csv", header=TRUE, sep = ";")
einW07 <- merge(ein_2007, aus_2007, by="RAUMID")
einW07 <- einW07 %>% mutate(E_0U6 = E_U1 + E_1U6) %>%
  dplyr::select("RAUMID", "E_E", 
                 # helpful age ranges
                 "E_18U25", "E_25U55", "E_U1", "E_0U6",
                 # foreigners 
                 "E_A"
                 # people who have been living in the neighborhood for 5-10 years no longer available — not necessary for analysis
                 )
rm(ein_2007, aus_2007)

# 2005
aus_2005 <- read.table("~/Desktop/Code/Thesis/EinR/2005/EWR200412A_Matrix.csv", header=TRUE, sep = ";")
ein_2005 <- read.table("~/Desktop/Code/Thesis/EinR/2005/EWR200412E_Matrix.csv", header=TRUE, sep = ";")
einW05 <- merge(ein_2005, aus_2005, by="RAUMID")
einW05 <- einW05 %>% mutate(E_0U6 = E_U1 + E_1U6) %>%
  dplyr::select("RAUMID", "E_E", 
                 # helpful age ranges
                 "E_18U25", "E_25U55", "E_U1", "E_0U6",
                 # foreigners 
                 "E_A"
                 # people who have been living in the neighborhood for 5-10 years no longer available — not necessary for analysis
)
rm(ein_2005, aus_2005)

# 2003 
aus_2003 <- read.table("~/Desktop/Code/Thesis/EinR/2003/EWR200212A_Matrix.csv", header=TRUE, sep = ";")
ein_2003 <- read.table("~/Desktop/Code/Thesis/EinR/2003/EWR200212E_Matrix.csv", header=TRUE, sep = ";")
einW03 <- merge(ein_2003, aus_2003, by="RAUMID")
einW03 <-  einW03 %>% mutate(E_0U6 = E_U1 + E_1U6) %>% 
  dplyr::select("RAUMID", "E_E", 
                 # helpful age ranges
                 "E_18U25", "E_25U55", "E_U1", "E_0U6",
                 # foreigners 
                 "E_A"
                 # people who have been living in the neighborhood for 5-10 years no longer available — not necessary for analysis
)
rm(ein_2003, aus_2003)

# 2001
aus_2001 <- read.table("~/Desktop/Code/Thesis/EinR/2001/EWR200112A_Matrix.csv", header=TRUE, sep = ";")
ein_2001 <- read.table("~/Desktop/Code/Thesis/EinR/2001/EWR200112E_Matrix.csv", header=TRUE, sep = ";")
einW01 <- merge(ein_2001, aus_2001, by="RAUMID")
einW01 <- einW01 %>% mutate(E_0U6 = E_U1 + E_1U6) %>% 
  dplyr::select("RAUMID", "E_E", 
                        # helpful age ranges
                        "E_18U25", "E_25U55", "E_U1", "E_0U6",
                        # foreigners 
                        "E_A"
                        # people who have been living in the neighborhood for 5-10 years no longer available — not necessary for analysis
)

# Saving files
save(einW19, einW17, einW15, einW13, einW11, einW09, einW07, einW05, einW03, einW01, file = "~/Desktop/Code/Thesis/Data_for_Analysis/einR.Rdata")



