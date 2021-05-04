########################################
# Thesis Script 3.2: DID Analysis
########################################

library(did)
library(plm)
library(tidyverse)
library(foreign)
library(ggplot2)
library(MASS)
library(Hmisc)
library(reshape2)
library(brant)

load("~/Desktop/Code/Thesis/Data_for_Analysis/datacomb.Rdata")
load("~/Desktop/Code/Thesis/Data_for_Analysis/gtable.Rdata")
load("~/Desktop/Code/Thesis/Data_for_Analysis/blong.Rdata")

dfL_full$gstatus <- as.factor(dfL_full$gstatus)
dfL_full <- dfL_full %>% filter(F_IN_KRAFT.x < 2020)

m_simple <- MASS::polr(gstatus ~ treattime, data = dfL_full, Hess=TRUE)
summary(m_simple)
brant(m_simple)
(ctables <- coef(summary(m_simple)))
p <- pnorm(abs(ctables[, "t value"]), lower.tail = FALSE) * 2
ctables <- cbind(ctables, "p value" = p)
as.data.frame(ctables)
(ci <- confint(m_simple))
confint.default(m_simple)
exp(coef(m_simple))
exp(cbind(OR = coef(m_simple), ci))

df1 <- dfL_full %>% filter(dfL_full$is_eg == 1)
m_1 <- polr(gstatus ~ treattime, data = df1, Hess=TRUE)
equatiomatic::extract_eq(m_1)
summary(m_1)
brant(m_1)
(ctables <- coef(summary(m_1)))
p <- pnorm(abs(ctables[, "t value"]), lower.tail = FALSE) * 2
ctables <- cbind(ctables, "p value" = p)
as.data.frame(ctables)
(ci <- confint(m_1))
confint.default(m_1)
exp(coef(m_1))
exp(cbind(OR = coef(m_1), ci))

df2 <- dfL_full %>% filter(dfL_full$is_eg != 1)
m_500 <- polr(gstatus ~ treattime500, data = df2, Hess=TRUE)
equatiomatic::extract_eq(m_500)
summary(m_500)
brant(m_500)
(ctables <- coef(summary(m_500)))
p <- pnorm(abs(ctables[, "t value"]), lower.tail = FALSE) * 2
ctables <- cbind(ctables, "p value" = p)
as.data.frame(ctables)
(ci <- confint(m_500))
confint.default(m_500)
exp(coef(m_500))
exp(cbind(OR = coef(m_500), ci))

df3 <- df2 %>% filter(df2$is_within_500eg != 0)
m_500_1 <- polr(gstatus ~ treattime500, data = df3, Hess=TRUE)
equatiomatic::extract_eq(m_500_1)
summary(m_500_1)
brant(m_500_1)
(ctables <- coef(summary(m_500_1)))
p <- pnorm(abs(ctables[, "t value"]), lower.tail = FALSE) * 2
ctables <- cbind(ctables, "p value" = p)
as.data.frame(ctables)
(ci <- confint(m_500_1))
confint.default(m_500_1)
exp(coef(m_500_1))
exp(cbind(OR = coef(m_500_1), ci))

set.seed(456)
N.train <- ceiling(0.7 * nrow(df3))
N.test <- nrow(df3) - N.train
trainset <- sample(seq_len(nrow(df3)), N.train)
testset <- setdiff(seq_len(nrow(df3)), trainset)

m_500_1t <- polr(gstatus ~ treattime500, data = df3, Hess=TRUE, subset = trainset)
summary(m_500_1t)

res <- residuals(m_500_1t)
# ensure that x- and y-axis have the same range
pred <- m_500_1t$fitted.values
obs <- df3[trainset, "gstatus"]
# determine maximal range
val.range <- range(pred, obs)
plot(obs, pred, 
     xlim = val.range, ylim = val.range,  
     xlab = "observed gstatus", 
     ylab = "predicted gstatus",
     main = "Residuals of the linear model for the training data")
# show ideal prediction as a diagonal
abline(0,1, col = "red")
# add residuals to the plot

qqnorm(df3$treattime500, pch = 1, frame = FALSE)
qqline(df3$treattime500, col = "steelblue", lwd = 2)

# using regression analysis to see if treatment effects any particular stage more than another
eg_lor_full$ID <- as.numeric(eg_lor_full$ID)
df_gs <- left_join(bfinal, eg_lor_full, by=c("RAUMID" = "ID"))

treatedduringstage <- function(df, egmarker, secondstage){
  df <- df %>% 
    filter(!is.na(df[,secondstage])) %>%
    dplyr::select(c(1, 3:7, 24))
  df$x <- if_else(df[,c(egmarker)] < df[,c(secondstage)], 1, 0)
  df <- df %>% dplyr::select(c(1, 8))
  return(df)
}

t1 <- treatedduringstage(df_gs, "F_IN_KRAFT.x", "gcode3yr") %>% rename(t2to3 = x)
t2 <- treatedduringstage(df_gs, "F_IN_KRAFT.x", "gcode4yr") %>% rename(t3to4 = x)
t3 <- treatedduringstage(df_gs, "F_IN_KRAFT.x", "gcode5yr") %>% rename(t4to5 = x)

df_gs <- left_join(df_gs, t1, by=c("RAUMID")) %>%
  left_join(., t2, by=c("RAUMID")) %>%
  left_join(., t3, by=c("RAUMID")) %>% 
  dplyr::select(c(1, 23:24, 2:7, 17:21, 65:67))

dum <- df_gs %>% dplyr::select(c(1, 4, 10:14))
dum$censoredg1 <- if_else(is.na(dum$tg1), 0, 1)
dum$censoredg2 <- if_else(is.na(dum$tg2), 0, 1)
dum$censoredg3 <- if_else(is.na(dum$tg3), 0, 1)
dum$censoredg4 <- if_else(is.na(dum$tg4), 0, 1)
dum$censoredg5 <- if_else(is.na(dum$tg5), 0, 1)
df_gs <- left_join(df_gs, dum[c(1, 8:12)], by=c("RAUMID"))
df_gs[, 15:17][is.na(df_gs[, 15:17])] <- 0

eg_effects2to3 <- glm(t2to3 ~ tg3, family=binomial, data=df_gs)
eg_effects3to4 <- glm(t3to4 ~ tg4, family=binomial, data=df_gs)
eg_effects4to5 <- glm(t4to5 ~ tg5, family=binomial, data=df_gs)
eg_effects_onstages <- list(eg_effects2to3, eg_effects3to4, eg_effects4to5)
lapply(eg_effects_onstages, summary)

cor.test(df_gs$t2to3, df_gs$tg2, method = "kendall")
cor.test(df_gs$t3to4, df_gs$tg3, method = "kendall")
cor.test(df_gs$t4to5, df_gs$tg4, method = "kendall")

# spatial regression
h <- sp_data('houses2000')
hh <- aggregate(h, "County")
d1 <- data.frame(h)[, c("nhousingUn", "recHouses", "nMobileHom", "nBadPlumbi",
                        "nBadKitche", "Population", "Males", "Females", "Under5", "White",
                        "Black", "AmericanIn", "Asian", "Hispanic", "PopInHouse", "nHousehold", "Families")]
d1a <- aggregate(d1, list(County=h$County), sum, na.rm=TRUE)

d2 <- data.frame(h)[, c("houseValue", "yearBuilt", "nRooms", "nBedrooms",
                        "medHHinc", "MedianAge", "householdS",  "familySize")]
d2 <- cbind(d2 * h$nHousehold, hh=h$nHousehold)
d2a <- aggregate(d2, list(County=h$County), sum, na.rm=TRUE)
d2a[, 2:ncol(d2a)] <- d2a[, 2:ncol(d2a)] / d2a$hh
d12 <- merge(d1a, d2a, by='County')
hh <- merge(hh, d12, by='County')
y <- matrix(hh$houseValue)
X <- cbind(1, hh$age, hh$nBedrooms)


# survival analysis on time to reach next stage
library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)

test <- Surv(df_gs$tg2, df_gs$censoredg2)
tfit <- survfit(test ~ 1, data=df_gs)
