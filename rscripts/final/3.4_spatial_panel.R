########################################
# Thesis Script 3.4: Spatial Panel Model
########################################
library(plm)
library(tidyverse)
library(ggplot2)

load("~/Desktop/Code/Thesis/Data_for_Analysis/panel.Rdata")

# making pdataframe -------------------------------------------------------
df_p <- pdata.frame(dfL_full, index=c("RAUMID", "Year"))
df_p$gstatus <- as.numeric(as.character(df_p$gstatus))
plotmeans(gstatus ~ Year, data=df_p)

time.periods <- c(2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019)
x <- dfL_full
x$Year[x$Year == 2019] <- 8
x$Year[x$Year == 2017] <- 7
x$Year[x$Year == 2015] <- 6
x$Year[x$Year == 2013] <- 5
x$Year[x$Year == 2011] <- 4
x$Year[x$Year == 2009] <- 3
x$Year[x$Year == 2007] <- 2
x$Year[x$Year == 2005] <- 1
x$F_IN_KRAFT.x[x$F_IN_KRAFT.x >= 2018 & x$F_IN_KRAFT.x < 2020] <- 8
x$F_IN_KRAFT.x[x$F_IN_KRAFT.x >= 2016 & x$F_IN_KRAFT.x < 2018] <- 7
x$F_IN_KRAFT.x[x$F_IN_KRAFT.x >= 2014 & x$F_IN_KRAFT.x < 2016] <- 6
x$F_IN_KRAFT.x[x$F_IN_KRAFT.x >= 2012 & x$F_IN_KRAFT.x < 2014] <- 5
x$F_IN_KRAFT.x[x$F_IN_KRAFT.x >= 2010 & x$F_IN_KRAFT.x < 2012] <- 4
x$F_IN_KRAFT.x[x$F_IN_KRAFT.x >= 2008 & x$F_IN_KRAFT.x < 2010] <- 3
x$F_IN_KRAFT.x[x$F_IN_KRAFT.x >= 2006 & x$F_IN_KRAFT.x < 2008] <- 2
x$F_IN_KRAFT.x[x$F_IN_KRAFT.x >= 2004 & x$F_IN_KRAFT.x < 2006] <- 1

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

cmat <- data.frame(coefs=coefs, ses=ses, exposure=exposure, check.rows=FALSE)

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