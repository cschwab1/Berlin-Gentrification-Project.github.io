########################################
# Thesis Script 3.3: Survival Analysis
########################################

# environment setup -------------------------------------------------------

setwd("~/Desktop/Code/Thesis")

library(tidyverse)
library(lubridate)
library(survival)
library(survminer)
library(GGally)

load("~/Desktop/Code/Thesis/Data_for_Analysis/bfinal.Rdata")
load("~/Desktop/Code/Thesis/Data_for_Analysis/eg.rda")
rm(baus, beu, bgaa, bwa)

eg_lor_full$ID <- as.numeric(eg_lor_full$ID)
data <- left_join(bfinal[c(1:7)], eg_lor_full[c(1, 3:4, 16, 18)], by=c("RAUMID" = "ID"))

# converting to long event format -----------------------------------------
start <- as.Date("2000-12-31")
data$start_S1 <- time_length(difftime(data$gcode1yr, start), "years")
data$start_S2 <- time_length(difftime(data$gcode2yr, start), "years")
data$start_S3 <- time_length(difftime(data$gcode3yr, start), "years")
data$start_S4 <- time_length(difftime(data$gcode4yr, start), "years")
data$start_S5 <- time_length(difftime(data$gcode5yr, start), "years")
data <- dplyr::select(data, c(1:7, 12:16, 8:11))

stage1 <- data[c(1, 8:9)] %>% 
  rename(startTime = 2, endTime = 3) %>% 
  filter(!is.na(startTime))
stage1$event <- ifelse(!is.na(stage1$endTime), c("tg2"), c("cens"))

stage2 <- data[c(1, 9:10)] %>% 
  rename(startTime = 2, endTime = 3) %>% 
  filter(!is.na(startTime))
stage2$event <- ifelse(!is.na(stage2$endTime), c("tg3"), c("cens"))

stage3 <- data[c(1, 10:11)] %>% 
  rename(startTime = 2, endTime = 3) %>% 
  filter(!is.na(startTime))
stage3$event <- ifelse(!is.na(stage3$endTime), c("tg4"), c("cens"))

stage4 <- data[c(1, 11:12)] %>% 
  rename(startTime = 2, endTime = 3) %>% 
  filter(!is.na(startTime))
stage4$event <- ifelse(!is.na(stage4$endTime), c("tg5"), c("cens"))

dataL <- rbind(stage1, stage2, stage3, stage4) %>% group_by(RAUMID)
rm(stage1, stage2, stage3, stage4, bfinal)

# treatment dummy variable: method correct method --------------------------------------
data$treatment_starttime <- time_length(difftime(data$F_IN_KRAFT.x, start), "years")

end <- as.Date("2020-01-01")
dataL$endTime <- ifelse(
  is.na(dataL$endTime), time_length(difftime(end, start), "years"), dataL$endTime
)

dataL <- left_join(dataL, data[c(1, 17)], by=c("RAUMID")) %>% tibble::rowid_to_column(., "ID")
dataL$treatedduring <- ifelse(
  dataL$startTime > dataL$treatment_starttime, 1, ifelse(
    dataL$endTime > dataL$treatment_starttime, 1, 0
  )
)
dataL[, 7][is.na(dataL[, 7])] <- 0

labels=c(0:4)
dataL$event <- as.factor(dataL$event)
dataL$event_num <- as.numeric(as.character(factor(dataL$event, labels=c(0:4))))

# testing survival analysis -----------------------------------------------

survcheck(Surv(startTime, endTime, event) ~ 1, id=RAUMID, data=dataL)

nullsurv <- survfit(Surv(startTime, endTime, event_num) ~ as.factor(treatedduring), id=RAUMID, data=dataL)
survminer::ggcompetingrisks(nullsurv)

surv_summary(nullsurv)

test <- coxph(Surv(startTime, endTime, event) ~ as.factor(treatedduring), id=RAUMID, data=dataL)

survminer::ggcoxdiagnostics(test)
ggcoxfunctional(test, data=dataL, id=RAUMID)
survminer::ggcox
ggcoxzph(cox.zph(test))

# plot(test, col=c(1:5), lty=c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2),
#      mark.time=FALSE, lwd=2, xscale=12,
#      xlab="Time since last transition", ylab="Probability in Stage i")


# looking at which areas are censured -------------------------------------

# x <- dataL %>% filter(event=="cens") %>% left_join(., data[c(1:7)])
# x$censuredduring <- case_when(
#   !is.na(x$gcode4yr) & is.na(x$gcode5yr) ~ "post4",
#   !is.na(x$gcode3yr) & is.na(x$gcode4yr) ~ "post3",
#   !is.na(x$gcode2yr) & is.na(x$gcode3yr) ~ "post2",
#   is.na(x$gcode2yr) ~ "post1"
# )
# x <- x %>% dplyr::select(-c(8:13))

ggplot(data=x, aes(x=factor(censuredduring), fill=factor(treatedduring))) +
  geom_bar(position="dodge")















load("~/Desktop/Code/Thesis/Data_for_Analysis/blong.Rdata")
load("~/Desktop/Code/Thesis/Data_for_Analysis/eg.rda")

eg_lor_full$ID <- as.numeric(eg_lor_full$ID)
df <- left_join(bfinal, eg_lor_full[c(1, 3:4, 16, 18)], by=c("RAUMID" = "ID"))

# dummy variable creation -------------------------------------------------

# censored dummy
df$c_0to1 <- ifelse(!is.na(df$tg0), 1, 0)
df$c_1to2 <- ifelse(!is.na(df$tg1), 1, 0)
df$c_2to3 <- ifelse(!is.na(df$tg2), 1, 0)
df$c_3to4 <- ifelse(!is.na(df$tg3), 1, 0)
df$c_4to5 <- ifelse(!is.na(df$tg4), 1, 0)
df[, 19:23][is.na(df[, 19:23])] <- 0

# treated during stage x thru x+1 dummy
df$treat_1 <- ifelse((df$gcode1yr > df$F_IN_KRAFT.x), 1, 0)
df$treat_2 <- ifelse((df$gcode2yr > df$F_IN_KRAFT.x), 1, 0)
df$treat_3 <- ifelse((df$gcode3yr > df$F_IN_KRAFT.x), 1, 0)
df$treat_4 <- ifelse((df$gcode4yr > df$F_IN_KRAFT.x), 1, 0)
df$treat_5 <- ifelse((df$gcode5yr > df$F_IN_KRAFT.x), 1, 0)
df[, 34:38][is.na(df[, 34:38])] <- 0

# treatment time variable
df$tt1 <-  time_length(difftime(df$gcode1yr, df$F_IN_KRAFT.x), "years")
df$tt2 <-  time_length(difftime(df$gcode2yr, df$F_IN_KRAFT.x), "years")
df$tt3 <-  time_length(difftime(df$gcode3yr, df$F_IN_KRAFT.x), "years")
df$tt4 <-  time_length(difftime(df$gcode4yr, df$F_IN_KRAFT.x), "years")
df$tt5 <-  time_length(difftime(df$gcode5yr, df$F_IN_KRAFT.x), "years")
df$tt_total <- time_length(difftime(as.Date(c("2019-1-1")), df$F_IN_KRAFT.x), "years")
df[, 37:42][df[, 37:42] < 0] <- NA

data <- df[c(1, 8, 19:24)]
data[3:8] <- round(data[3:8], digits=3)

etime <- with(
  df, ifelse(
    gcode == 1, tg0, ifelse(
      gcode == 2, tg1, ifelse(
        gcode == 3, tg2, ifelse(
          gcode == 4, tg3, ifelse(
            gcode == 5, tg4, NA
          )
        )
      )
    )
  )
)

x <- df[c(1:8, 19:23, 29:33)]

event <- with(
  df, ifelse(
    gcode == 5, 5*censored_4to5, ifelse(
      gcode == 4, 4*censored_3to4, ifelse(
        gcode == 3, 3*censored_2to3, ifelse(
          gcode == 2, 2*censored_1to2, ifelse(
            gcode == 1, 1*censored_0to1, 0
          )
        )
      )
    )
  )
)
possibleoutcomes <- c("censored", "madeitto1", "madeitto2", "madeitto3", "madeitto4", "madeitto5")
event <- factor(event, 0:5, labels=possibleoutcomes)

# survival analysis, null model --------------------------
test <- survfit(Surv(etime, event) ~ 1, data=mgus2)
plot(test, col=c(1:6),
     mark.time=FALSE,
     xlab="Years post diagnosis", ylab="Probability in State")
legend(c("censored", "reached status 1", "reached status 2", "reached status 3", "reached status 4", "reached status 5"),
       col=c(1:6), bty='n')

f1 <- survfit(Surv(tg1, censored_0to1) ~ 1, data = df)
f2 <- survfit(Surv(tg2, censored_1to2) ~ 1, data = df)
f3 <- survfit(Surv(tg3, censored_2to3) ~ 1, data = df)
f4 <- survfit(Surv(tg4, censored_3to4) ~ 1, data = df)
f5 <- survfit(Surv(tg5, censored_4to5) ~ 1, data = df)
survL <- list(F1 = f1, F2 = f2, F3 = f3, F4 = f4, F5 = f5)

lapply(survL, summary)
ggsurvplot(survL, data=df, combine=TRUE)

# survival analysis,  time as EG variable ---------------------------------

f1 <- survreg(Surv(tg1, censored_0to1) ~ treat_1, data = df)
f2 <- survreg(Surv(tg2, censored_1to2) ~ treat_2, data = df)
f3 <- survreg(Surv(tg3, censored_2to3) ~ treat_3, data = df)
f4 <- survreg(Surv(tg4, censored_3to4) ~ treat_4, data = df)
f5 <- survreg(Surv(tg5, censored_4to5) ~ treat_5, data = df)
survL <- list(F1 = f1, F2 = f2, F3 = f3, F4 = f4, F5 = f5)

lapply(survL, summary)
ggsurvplot(survL, data=df, combine=TRUE)

etime <- with(mgus2, ifelse(pstat==0, futime, ptime))
event <- with(mgus2, ifelse(pstat==0, 2*death, 1))


# # treated during stage x thru x+1 dummy
# data$treat_pre1 <- ifelse((data$gcode1yr > data$F_IN_KRAFT.x), 1, 0)
# data$treat_1 <- ifelse((data$gcode2yr > data$F_IN_KRAFT.x), 1, 0)
# data$treat_2 <- ifelse((data$gcode3yr > data$F_IN_KRAFT.x), 1, 0)
# data$treat_3 <- ifelse((data$gcode4yr > data$F_IN_KRAFT.x), 1, 0)
# data$treat_4 <- ifelse((data$gcode5yr > data$F_IN_KRAFT.x), 1, 0)
# 
# data$treat_1 <- ifelse(
#   (data$treat_pre1 == 1), 1, data$treat_1
# )
# data$treat_2 <- ifelse(
#   (data$treat_1 == 1), 1, data$treat_2
# )
# data$treat_3 <- ifelse(
#   (data$treat_2 == 1), 1, data$treat_3
# )
# data$treat_4 <- ifelse(
#   (data$treat_3 == 1), 1, data$treat_4
# )
# 
# df <- data[c(1, 17:21)] %>% rename(cens = 2, tg2 = 3, tg3 = 4, tg4 = 5, tg5 = 6)
# treatedL <- gather(df,
#                    event,
#                    treatedduring, 
#                    cens:tg5)
# test <- merge(dataL, treatedL, by=c("RAUMID", "event"))

# test <- left_join(test, data[c(1, 22)], by=c("RAUMID"))
# x <- filter(test, treatedduring == 1)
# y <- filter(test1, treatedduring == 1)
# data[, "max"] <- do.call(pmax, c(data[2:7], list(na.rm=TRUE)))

