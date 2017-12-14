## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(tibble.print_min = 4L, tibble.print_max = 4L)
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)
library(flumodelr)
library(tibble)

## ---- results='asis', fig.width=7----------------------------------------
flu_ex <- flumodelr::flu_ex

g1 <- ggplot(flu_ex, aes(x=yrweek_dt, y=fludeaths)) + 
  geom_line(size=0.8, colour="#CC0000") +
  scale_x_date(labels = date_format("%Y"), date_breaks="1 year",
               expand=c(0, .9)) + 
  xlab("Year-month") + 
  ylab("Deaths per 100,000") + 
  theme_light(base_size=16)
g1

## ---- results='as.is', fig.width=7---------------------------------------
## Add fourier term
fit <- flu_ex %>%
  mutate(week2 = row_number(),
         theta = 2*week2/52,
         sin_f1 = sinpi(theta),
         cos_f1 = cospi(theta),
         week_2 = week2^2,
         week_3 = week2^3,
         week_4 = week2^4,
         week_5 = week2^5,
         pred_y = predict( lm(fludeaths ~ 
                                week2 + sin_f1 + cos_f1, data=.)),
         pred_ypoly = predict( lm(fludeaths ~ week2 + 
                                week_2 + week_3 + week_4 + week_5 +
                                sin_f1 + cos_f1, data=.)))

## ---- echo=F, results='as.is', fig.width=7-------------------------------
g1 <- ggplot(fit, aes(x=yrweek_dt)) + 
  geom_line(aes(y=fludeaths), size=0.8, colour="#CC0000") +
  geom_line(aes(y=pred_y), size=0.8, colour="black") +
  geom_line(aes(y=pred_ypoly), size=0.8, linetype=2, colour="blue") +
  scale_x_date(labels = date_format("%Y"), date_breaks="1 year",
               expand=c(0, .9)) + 
  xlab("Year") + 
  ylab("Deaths per 100,000") + 
  theme_light(base_size=16)
g1

## ------------------------------------------------------------------------
cat("Example data")
flu_ex %>% select(yrweek_dt, fludeaths)

## ------------------------------------------------------------------------
flu_ex <- flu_ex %>%
  mutate(epi = if_else(month(yrweek_dt)>=10 | month(yrweek_dt)<=5, T, F))  

flu_ex %>%
  filter(year>=2014)

## ------------------------------------------------------------------------
## Compute fourier terms
flu_ex_serfling <- flu_ex %>%
  mutate(week2 = row_number(),
         theta = 2*week2/52,
         sin_f1 = sinpi(theta),
         cos_f1 = cospi(theta),
         week_2 = week2^2,
         week_3 = week2^3,
         week_4 = week2^4,
         week_5 = week2^5)

## ------------------------------------------------------------------------
base_fit <- flu_ex_serfling %>%
  mutate(fludeaths = if_else(epi == T, NA_real_, fludeaths)) %>%
  lm(fludeaths ~ week2 + week_2 + week_3 + week_4 + sin_f1 + cos_f1, 
     data=., na.action = na.exclude)

## ------------------------------------------------------------------------
## Fitted values + prediction interval
df_pred <- flu_ex_serfling %>%
  predict(base_fit, newdata=., se.fit=TRUE, 
          interval="prediction", level=0.95)

pred_y0 <- df_pred$fit[,1] #fitted values
pred_y0_uci <- df_pred$fit[,3] #95% UCI
pred_y0_serf <- df_pred$fit[,1] + 1.64*sd(df_pred$fit[,1]) 

df_base <- flu_ex %>%
  add_column(., pred_y0, pred_y0_uci, pred_y0_serf)  

df_base %>% select(year, week, fludeaths, 
                   pred_y0, pred_y0_uci, pred_y0_serf)  

## ---- echo=F, results='as.is', fig.width=7-------------------------------
ggplot(df_base, aes(x=yrweek_dt)) + 
  geom_line(aes(y=fludeaths, colour="Observed Deaths", 
                linetype="Observed Deaths"), size=0.8) +
  geom_line(aes(y=pred_y0, colour="Predicted Deaths", 
                linetype="Predicted Deaths"), size=0.8) +
  geom_line(aes(y=pred_y0_uci, colour="95% PI", 
                linetype="95% PI"), size=0.8) +
  geom_line(aes(y=pred_y0_serf, colour="Serfling Threshold", 
                linetype="Serfling Threshold"), size=0.8) +
  scale_colour_manual("Line",
                      breaks=c("Observed Deaths", 
                                 "Predicted Deaths", 
                                 "95% PI",
                                 "Serfling Threshold"),
                      values = c("Observed Deaths"="#CC0000", 
                                 "Predicted Deaths"="black", 
                                 "95% PI"="black",
                                 "Serfling Threshold"="black")) +
  scale_linetype_manual("Line", 
                        breaks=c("Observed Deaths", 
                                 "Predicted Deaths", 
                                 "95% PI",
                                 "Serfling Threshold"),
                        values = c("Observed Deaths"=1,
                                   "Predicted Deaths"=1,
                                   "95% PI"=2, 
                                   "Serfling Threshold"=3)) +
  scale_x_date(labels = date_format("%Y"), date_breaks="1 year",
               expand=c(0, .9)) + 
  xlab("Year") + 
  ylab("Deaths per 100,000") + 
  theme_light(base_size=14) +
  theme(legend.text=element_text(size=10)) +
  labs(title="Influenza Deaths, Epidemic Thresholds") +
  guides(colour = guide_legend("Line"), linetype = guide_legend("Line"))

## ------------------------------------------------------------------------
df_epi <- df_base %>%
  mutate(threshold = if_else(fludeaths > pred_y0_serf, T, F),
         epidemic = if_else(threshold==T & lead(threshold)==T, T, NA),
         epidemic2 = if_else(lag(epidemic)==T & threshold==T, T, epidemic),
         epidemic3 = if_else(lag(epidemic2)==T & lead(threshold)==T, T, epidemic2),
         epidemic4 = if_else(lag(epidemic3)==T & threshold==T, T, epidemic3),
         epidemic5 = coalesce(epidemic4, F)) %>%
  rowwise() %>%
  mutate(epidemic = if_else(sum(epidemic, epidemic2, epidemic3, 
                                   epidemic4, epidemic5, na.rm=T)>0, T, F),
         excess = if_else(epidemic==T & (fludeaths - pred_y0_uci)>0, fludeaths - pred_y0_uci, 0))
df_epi %>% select(year, week, threshold, epidemic, excess)

## ---- echo=F, results='as.is', fig.width=7.0-----------------------------
ggplot(df_epi, aes(x=yrweek_dt)) + 
  geom_line(aes(y=excess, colour="Excess mortality"), size=0.8, linetype=2) +
  geom_line(aes(y=fludeaths, colour="Reported mortality"), size=0.8, linetype=1) +
  scale_x_date(labels = date_format("%Y"), date_breaks="1 year",
               expand=c(0, .9)) + 
  scale_colour_manual("Line",
                      values = c("Excess mortality"="#CC0000", 
                                 "Reported mortality"="black")) +
  xlab("Year") + 
  ylab("Deaths per 100,000") + 
  theme_light(base_size=16)

## ------------------------------------------------------------------------
##The following command completes the above steps 

##specify an epidemic period
flu_ex <- flu_ex %>%
  mutate(epi = if_else(month(yrweek_dt)>=10 | month(yrweek_dt)<=5, T, F))  

##fit serfling model
flu_ex_pred <- serflm(data=flu_ex, 
                      outc="fludeaths", epi="epi", time="yrweek_dt")

