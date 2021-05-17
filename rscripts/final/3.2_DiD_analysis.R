########################################
# Thesis Script 3.2: Difference-in-difference Analysis
########################################


# loading environment -----------------------------------------------------

##### Loading environment 
library(tidyverse)
library(gplots)
library(lmtest)
library(sandwich)
library(sjPlot)
library(fixest)
library(did)

setwd("~/Desktop/Code/Thesis")
load("~/Desktop/Code/Thesis/Data_for_Analysis/blong.Rdata")


# narrowing down areas with logit -----------------------------------------

load("~/Desktop/Code/Thesis/Data_for_Analysis/btotalc.Rdata")
ptrends_df <- left_join(btotalc, df[c(1:2, 20:21)], by=c("RAUMID")) %>% filter(gcode != 0)
ptrends_logit <- femlm(is_eg ~ E_18U25 + E_25U55 + E_0U6 + WA + eu + aus_noneu + welf + unemp + gaa + gcode, 
                       data=ptrends_df, 
                       family="logit")
ptrends_df <- ptrends_df %>% 
  mutate(
    phat = predict(ptrends_logit, ptrends_df), 
    keep = phat > quantile(phat, probs = c(0.25), na.rm = TRUE)
  ) %>% filter(keep == "TRUE")

load("~/Desktop/Code/Thesis/Data_for_Analysis/contextL.Rdata")
context_varsL$Year <- as.numeric(context_varsL$Year)

# doing it with long data didn't really work
# load("~/Desktop/Code/Thesis/Data_for_Analysis/contextL.Rdata")
# context_varsL$Year <- as.numeric(context_varsL$Year)
# ptrends_dfL <- left_join(context_varsL, dfL_full, by=c("RAUMID", "Year")) %>% filter(gstatus != 0)
# ptrends_logitL <- feols(is_eg ~ auschange + eu_change + gaa_change + WA_change + unemp_change + welf_change + E_18U25_change + E_25U55_change +  E_0U6_change | RAUMID + Year, 
#                         data = ptrends_dfL)


# pre-setup on long data file ---------------------------------------------

dfL_full <- dfL_full %>% 
  filter(gstatus != 0) %>% 
  filter(RAUMID %in% ptrends_df$RAUMID) %>% 
  left_join(., context_varsL, by=c("RAUMID", "Year")) %>% 
  drop_na()

################################################################################
##### running DiD analysis #####################################################
################################################################################
# controlvars <- names(dfL_full[c(15:23)])

didtest <- did::att_gt(
  yname = "gstatus",
  tname = "Year",
  idname = "RAUMID",
  weightsname = "pop",
  gname = "F_IN_KRAFT.x",
  xformla= ~ gaa_change + welf_change,
  data = dfL_full
)

outdyn <- did::aggte(didtest, type="dynamic")
did::ggdid(outdyn)

es_pts_spillover_control <- tibble(time = unlist(outdyn["egt"]), estimate = unlist(outdyn["att.egt"]), std.error = unlist(outdyn["se.egt"])) %>% 
  mutate(
    ub = estimate + 1.96 * std.error,
    lb = estimate - 1.96 * std.error
  ) %>%
  filter(time != -8) 

didtest_500 <- did::att_gt(
  yname = "gstatus",
  tname = "Year",
  idname = "RAUMID",
  weightsname = "pop",
  gname = "F_IN_KRAFT.y",
  xformla= ~ gaa_change + welf_change,
  data = dfL_full
)

outdyn_500 <- did::aggte(didtest_500, type="dynamic", na.rm = TRUE)

did::ggdid(outdyn_500, type="dynamic")
did::ggdid(outdyn, type="dynamic")

es_pts_spillover_effect <- tibble(time = unlist(outdyn_500["egt"]), estimate = unlist(outdyn_500["att.egt"]), std.error = unlist(outdyn_500["se.egt"])) %>% 
  mutate(
    ub = estimate + 1.96 * std.error,
    lb = estimate - 1.96 * std.error
)

es_pts_combined <- bind_rows(
  es_pts_spillover_control %>% mutate(group = "Treatment Effect", time = time - 0.1),
  es_pts_spillover_effect %>% mutate(group = "Spillover Onto Control", time = time + 0.1)
) %>% filter(std.error != 0)

es_plot_combined <- ggplot(es_pts_combined) +
    geom_vline(xintercept = -0.5, color = "grey50") + 
    geom_hline(yintercept = 0, color = "black") +
    geom_point(aes(x = time, y = estimate, color = group)) +
    geom_errorbar(aes(x = time, ymin = lb, ymax = ub, color = group), alpha = 0.8) + 
    theme(
      legend.position = c(0.15, 0.25),
      legend.spacing.x = unit(0, "pt"),
      legend.spacing.y = unit(0, "pt")
    ) +
    scale_shape_manual(values = c(16, 18)) + 
    scale_color_manual(values = c("#5e81ac", "#bf616a")) +
    labs(y = "Treatment effect on gentrification status", x = "Years since protection area establishment", color = NULL)

es_plot_combined


\ggplot(es_pts_spillover_effect) +
  geom_point(aes(x = time, y = estimate)) +
  geom_errorbar(aes(x = time, ymin = lb, ymax = ub), alpha = 0.8) + 
  geom_vline(xintercept = -0.5, color = "grey50") + 
  geom_hline(yintercept = 0, color = "black") +
  labs(y = NULL, x = "Years since CHC establishment")



ggplot(es_pts_spillover_control) +
  geom_point(aes(x = time, y = estimate)) +
  geom_errorbar(aes(x = time, ymin = lb, ymax = ub), alpha = 0.8) + 
  geom_vline(xintercept = -0.5, color = "grey50") + 
  geom_hline(yintercept = 0, color = "black") +
  labs(y = NULL, x = "Years since CHC establishment")

outsimp <- aggte(didtest, type="simple")



did.e <- aggte(didtest, type="group")
did.c <- aggte(didtest, type="calendar")

save(didtest, outdyn, did.e, file="~/Desktop/Code/Thesis/Data_for_Analysis/didplot.Rdata")








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


st_write(df, dsn = "~/Desktop/Code/Thesis/Data_for_Analysis/df.shp")
write.csv(df, file="~/Desktop/Code/Thesis/Data_for_Analysis/df.csv")
write.csv(df_long, file="~/Desktop/Code/Thesis/Data_for_Analysis/df_long.csv")