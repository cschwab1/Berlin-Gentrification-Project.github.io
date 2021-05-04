########################################
# Thesis Script 3.1: Regression Analysis
########################################

##### Loading environment 
library(tidyverse)
library(gplots)
library(lmtest)
library(sandwich)
library(sjPlot)

setwd("~/Desktop/Code/Thesis")
load("~/Desktop/Code/Thesis/Data_for_Analysis/blong.Rdata")

##### Pretesting: running panel analysis
# event study regression, 0 effect in pre-treatment period
df_p <- pdata.frame(dfL_full, index=c("RAUMID", "Year"))
df_p$gstatus <- as.numeric(as.character(df_p$gstatus))
plotmeans(gstatus ~ Year, data=df_p)

time.periods <- c(2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019)
x <- dfL_full
x$Year[x$Year == 2019] <- 9
x$Year[x$Year == 2017] <- 8
x$Year[x$Year == 2015] <- 7
x$Year[x$Year == 2013] <- 6
x$Year[x$Year == 2011] <- 5
x$Year[x$Year == 2009] <- 4
x$Year[x$Year == 2007] <- 3
x$Year[x$Year == 2005] <- 2
x$Year[x$Year == 2003] <- 1
x$F_IN_KRAFT.x[x$F_IN_KRAFT.x >= 2018 & x$F_IN_KRAFT.x < 2020] <- 9
x$F_IN_KRAFT.x[x$F_IN_KRAFT.x >= 2016 & x$F_IN_KRAFT.x < 2018] <- 8
x$F_IN_KRAFT.x[x$F_IN_KRAFT.x >= 2014 & x$F_IN_KRAFT.x < 2016] <- 7
x$F_IN_KRAFT.x[x$F_IN_KRAFT.x >= 2012 & x$F_IN_KRAFT.x < 2014] <- 6
x$F_IN_KRAFT.x[x$F_IN_KRAFT.x >= 2010 & x$F_IN_KRAFT.x < 2012] <- 5
x$F_IN_KRAFT.x[x$F_IN_KRAFT.x >= 2008 & x$F_IN_KRAFT.x < 2010] <- 4
x$F_IN_KRAFT.x[x$F_IN_KRAFT.x >= 2006 & x$F_IN_KRAFT.x < 2008] <- 3
x$F_IN_KRAFT.x[x$F_IN_KRAFT.x >= 2004 & x$F_IN_KRAFT.x < 2006] <- 2
x$F_IN_KRAFT.x[x$F_IN_KRAFT.x >= 2002 & x$F_IN_KRAFT.x < 2004] <- 1

x <- x %>% filter(x$F_IN_KRAFT.x < 10)
time.periods <- 9
te.e <- time.periods:1
dtl <- sapply(-(time.periods-1):(time.periods-2), function(l) {
  dtl <- 1*( (x$Year == x$F_IN_KRAFT.x + l) & (x$F_IN_KRAFT.x > 0) )
  dtl
})

dtl <- as.data.frame(dtl)
cnames1 <- paste0("Dtmin",(time.periods-1):1)
colnames(dtl) <- c(cnames1, paste0("Dt",0:(time.periods-2)))
data <- cbind.data.frame(x, dtl)
row.names(data) <- NULL
data$gstatus <- as.numeric(as.character(data$gstatus))

es <- plm(gstatus ~ Dtmin7 + Dtmin6 + Dtmin5 + Dtmin4 + Dtmin3 + Dtmin2 + Dtmin1 +
            Dt0 + Dt1 + Dt2 + Dt3 + Dt4 + Dt5 + Dt6 + Dt7,
          data=data, model="within", effect="twoways",
          index=c("RAUMID","Year"))
summary(es)

coefs1 <- coef(es)
ses1 <- sqrt(diag(summary(es)$vcov))
idx.pre <- 1:(time.periods-2)
idx.post <- (time.periods-1):length(coefs1)
coefs <- c(coefs1[idx.pre], 0, coefs1[idx.post])
ses <- c(ses1[idx.pre], 0, ses1[idx.post])
exposure <- -(time.periods-1):(time.periods-2)

cmat <- data.frame(coefs=coefs, ses=ses, exposure=exposure)

library(ggplot2)

ggplot(data=cmat, mapping=aes(y=coefs, x=exposure)) +
  geom_line(linetype="dashed") +
  geom_point() + 
  geom_errorbar(aes(ymin=(coefs-1.96*ses), ymax=(coefs+1.96*ses)), width=0.2) +
  ylim(c(-2,5)) +
  theme_bw()

fm=gstatus~is_eg_byyear+RAUMID+Year
R1.DD1<- plm(fm, data=df_p, model="pooling")
bptest(R1.DD1, studentize=F)

coeftest(R1.DD1, vcovHC(R1.DD1, type = "HC1")) # Heteroskedasticity consistent coefficients, type 3
t(sapply(c("HC0", "HC1", "HC2", "HC3", "HC4"), function(x) sqrt(diag(vcovHC(R1.DD1, type = x)))))

################################################################################
##### running DiD analysis #####################################################
################################################################################

didtest <- did::att_gt(
  yname = "gstatus",
  tname = "Year",
  idname = "RAUMID",
  gname = "F_IN_KRAFT.x",
  xformla=~1,
  data = dfL_full
)

outsimp <- aggte(didtest, type="simple")
outdyn <- aggte(didtest, type="dynamic")
did.e <- aggte(didtest, type="group")
did.c <- aggte(didtest, type="calendar")

save(didtest, outdyn, did.e, file="~/Desktop/Code/Thesis/Data_for_Analysis/didplot.Rdata")


st_write(df, dsn = "~/Desktop/Code/Thesis/Data_for_Analysis/df.shp")
write.csv(df, file="~/Desktop/Code/Thesis/Data_for_Analysis/df.csv")
write.csv(df_long, file="~/Desktop/Code/Thesis/Data_for_Analysis/df_long.csv")