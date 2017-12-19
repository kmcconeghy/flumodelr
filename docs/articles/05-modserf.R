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

## ------------------------------------------------------------------------
flu_ex <- flumodelr::flu_ex

## ---- echo=F, results='as.is', fig.width=7.0-----------------------------
ggplot(flu_ex, aes(x=yrweek_dt)) + 
  geom_line(aes(y=fludeaths, colour="Observed Deaths", 
                linetype="Observed Deaths"), size=0.8) +
  geom_line(aes(y=prop_flupos*10, colour="No. Positive per 10 isolates", 
                linetype="No. Positive per 10 isolates"), size=0.8) +
  scale_colour_manual("Line",
                      breaks=c("Observed Deaths", 
                                 "No. Positive per 10 isolates"),
                      values = c("Observed Deaths"="#CC0000", 
                                 "No. Positive per 10 isolates"="black")) +
  scale_linetype_manual("Line", 
                        breaks=c("Observed Deaths", 
                                 "No. Positive per 10 isolates"),
                        values = c("Observed Deaths"=1,
                                   "No. Positive per 10 isolates"=2)) +
  scale_x_date(labels = date_format("%Y"), date_breaks="1 year",
               expand=c(0, .9)) +
  xlab("Year") + 
  theme_light(base_size=16) +
  theme(plot.title = element_text(size=14)) +
  labs(title="Figure 1. Influenza (+) isolates over time")

## ------------------------------------------------------------------------
flu_ex <- flumodelr::flu_ex
flu_ex

## ------------------------------------------------------------------------
flu_ex_mod <- flu_ex %>%
  mutate(week2 = row_number(),
         theta = 2*week2/52,
         sin_f1 = sinpi(theta),
         cos_f1 = cospi(theta))

## ------------------------------------------------------------------------
flu_ex_mod <- flu_ex_mod %>%
  mutate(week_2 = week2^2,
         week_3 = week2^3,
         week_4 = week2^4,
         week_5 = week2^5)

## ------------------------------------------------------------------------
base_fit <- flu_ex_mod %>%
  lm(fludeaths ~ week2 + week_2 + week_3 + week_4 + prop_flupos + sin_f1 + cos_f1, data=., na.action = na.exclude)
summary(base_fit)

## ------------------------------------------------------------------------
base_pred <- flu_ex_mod %>%
  mutate(prop_flupos = 0) %>% #Note setting to zero
  predict(base_fit, newdata=., se.fit=TRUE, 
          interval="prediction", level=0.95)

## ------------------------------------------------------------------------
pred_y0 <- base_pred$fit[,1] #fitted values
pred_y0_uci <- base_pred$fit[,3] #95% Upper Prediction Interval

flu_ex <- flu_ex %>%
  add_column(., pred_y0, pred_y0_uci) 
flu_ex

## ---- echo=F, results='as.is', fig.width=7-------------------------------
ggplot(flu_ex, aes(x=yrweek_dt)) + 
  geom_line(aes(y=fludeaths, colour="Observed Deaths", 
                linetype="Observed Deaths"), size=0.8) +
  geom_line(aes(y=pred_y0, colour="Predicted Deaths", 
                linetype="Predicted Deaths"), size=0.8) +
  geom_line(aes(y=pred_y0_uci, colour="95% PI", 
                linetype="95% PI"), size=0.8) +
  scale_colour_manual("Line",
                      breaks=c("Observed Deaths", 
                                 "Predicted Deaths", 
                                 "95% PI"),
                      values = c("Observed Deaths"="#CC0000", 
                                 "Predicted Deaths"="black", 
                                 "95% PI"="black")) +
  scale_linetype_manual("Line", 
                        breaks=c("Observed Deaths", 
                                 "Predicted Deaths", 
                                 "95% PI"),
                        values = c("Observed Deaths"=1,
                                   "Predicted Deaths"=1,
                                   "95% PI"=2)) +
  scale_x_date(labels = date_format("%Y"), date_breaks="1 year",
               expand=c(0, .9)) + 
  xlab("Year") + 
  ylab("Deaths per 100,000") + 
  theme_light(base_size=14) +
  theme(legend.text=element_text(size=10), 
        plot.title = element_text(size=14)) +
  labs(title="Figure 2. Pneumonia and Influenza Deaths Over Time",
       caption="Modified Serfling Model") +
  guides(colour = guide_legend("Line"), linetype = guide_legend("Line"))

## ------------------------------------------------------------------------
flu_ex <- flu_ex %>%
  rowwise() %>%
  mutate(excess = if_else((fludeaths - pred_y0_uci)>0, 
                          fludeaths - pred_y0_uci, 0))
flu_ex

## ---- echo=F, results='as.is', fig.width=7.0-----------------------------
ggplot(flu_ex, aes(x=yrweek_dt)) + 
  geom_line(aes(y=excess, colour="Epidemic mortality"), size=0.8, linetype=2) +
  geom_line(aes(y=fludeaths, colour="Reported mortality"), size=0.8, linetype=1) +
  scale_x_date(labels = date_format("%Y"), date_breaks="1 year",
               expand=c(0, .9)) + 
  scale_colour_manual("Line",
                      values = c("Epidemic mortality"="#CC0000", 
                                 "Reported mortality"="black")) +
  xlab("Year") + 
  ylab("Deaths per 100,000") + 
  theme_light(base_size=16) +
  theme(plot.title = element_text(size=14)) +
  labs(title="Figure 3. Periods of influenza epidemics over time")

## ------------------------------------------------------------------------
flu_ex <- flu_ex %>%
  rowwise() %>%
  mutate(excess = if_else((fludeaths - pred_y0)>0, 
                          fludeaths - pred_y0, 0))
flu_ex %>% select(year, week, excess)

## ---- echo=F, results='as.is', fig.width=7.0-----------------------------
ggplot(flu_ex, aes(x=yrweek_dt)) + 
  geom_line(aes(y=excess, colour="Excess mortality"), size=0.8, linetype=2) +
  geom_line(aes(y=fludeaths, colour="Reported mortality"), size=0.8, linetype=1) +
  scale_x_date(labels = date_format("%Y"), date_breaks="1 year",
               expand=c(0, .9)) + 
  scale_colour_manual("Line",
                      values = c("Excess mortality"="#CC0000", 
                                 "Reported mortality"="black")) +
  xlab("Year") + 
  ylab("Deaths per 100,000") + 
  theme_light(base_size=16) +
  theme(plot.title = element_text(size=14)) +
  labs(title="Figure 3. Periods of excess mortality over time")

## ---- eval=F-------------------------------------------------------------
#  flu_ex <- flumodelr::flu_ex
#  
#  flu_ex_mod <- mflu(flu_ex, method="serfling",
#                     outc="fludeaths", time="yrweek_dt", epi="epi")
#  
#  flu_ex_mod %>% select(year, week, fludeaths, pred_y0, pred_y0_serf)

## ---- eval=F-------------------------------------------------------------
#  flu_ex <- flumodelr::flu_ex
#  
#  flu_ex_mod <- mflu(flu_ex, method="virology",
#                     outc="fludeaths", time="yrweek_dt", lab="prop_flupos")
#  
#  flu_ex_mod %>% select(year, week, fludeaths, pred_y0, pred_y0_uci)

## ---- eval=F-------------------------------------------------------------
#  ## Without polynomial terms
#  flu_ex_mod <- mflu(flu_ex, method="virology",
#                     outc="fludeaths", time="yrweek_dt", lab="prop_flupos",
#                     poly=F)
#  
#  ## Epidemic period specified (serfling model only)
#  flu_ex_mod <- mflu(flu_ex, method="serfling",
#                     outc="fludeaths", time="yrweek_dt", epi=c(40, 20)
#                     )

## ------------------------------------------------------------------------
sessioninfo::session_info()

