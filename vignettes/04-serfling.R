## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(tibble.print_min = 4L, tibble.print_max = 4L)
library(tidyverse)
library(lubridate)
library(scales)
library(flumodelr)

## ---- echo=F, results='asis', fig.width=7--------------------------------
df_cdc <- flumodelr::fludta %>%
  dplyr::filter(year>=2010) %>%
  mutate(t = row_number(), #set origin to october
         theta = 2*t / 52,
         sin_2 = sinpi(theta),
         cos_2 = cospi(theta)) 

g1 <- ggplot(df_cdc, aes(x=yrweek_dt, y=perc_fludeaths)) + 
  geom_line(size=0.8, colour="#CC0000") +
  scale_x_date(labels = date_format("%Y"), date_breaks="1 year",
               expand=c(0, .9)) + 
  xlab("Year") + 
  ylab("% of Deaths from P&I") + 
  theme_light(base_size=16) +
  theme(plot.title = element_text(size=14)) +
  labs(title="Figure 1. Pneumonia and Influenza Mortality")

## ---- echo=F, results='asis', fig.width=7--------------------------------
g1 <- ggplot(df_cdc, aes(x=yrweek_dt, y=perc_fludeaths)) + 
  geom_line(size=0.8, colour="#CC0000") +
  geom_smooth(method='lm') +
  scale_x_date(labels = date_format("%Y"), date_breaks="1 year",
               expand=c(0, .9)) + 
  xlab("Year") + 
  ylab("% of Deaths from P&I") + 
  theme_light(base_size=16) +
  theme(plot.title = element_text(size=14)) +
  labs(title="Figure 2. Pneumonia and Influenza Deaths Over Time",
       caption="Ordinary Least Squares")
g1

## ---- fig.width=7--------------------------------------------------------
g1 <- ggplot(df_cdc, aes(x=yrweek_dt, y=fludeaths)) + 
  geom_line(size=0.8) +
  scale_x_date(labels = date_format("%Y"), date_breaks="1 year",
               expand=c(0, .9)) + 
  xlab("Year") + 
  ylab("No. of Deaths from P&I") + 
  theme_light(base_size=16) +
  theme(plot.title = element_text(size=14)) +
  labs(title="Figure 1. Pneumonia and Influenza Mortality") +
  theme(axis.text.x=element_text(angle=90, hjust=1))

g1

## ------------------------------------------------------------------------
df_low <- df_cdc %>%
  dplyr::filter(yrweek_dt >= ymd('2011-10-01') & yrweek_dt <= ymd('2012-09-30')) %>%
  mutate(t = row_number(), #set origin to october
         theta = 2*t / 52,
         sin_2 = sinpi(theta),
         cos_2 = cospi(theta)) 

lm(fludeaths ~ t + sin_2 + cos_2, #note the 0 term
  data=df_low, na.action = na.exclude) -> mod_1_low 

summary(mod_1_low)

## ------------------------------------------------------------------------
df_low_pred <- suppressWarnings(predict(mod_1_low, se.fit=TRUE, 
          interval="prediction", level=0.90)) 

df_low <- bind_cols(df_low, pred_yhat_0 = df_low_pred$fit[, 'fit'])

g_mod_1 <- ggplot(df_low) + 
  geom_line(aes(x=yrweek_dt, y=fludeaths), size=0.8, alpha=0.5) +
  geom_line(aes(x=yrweek_dt, y=pred_yhat_0), linetype='dashed', size=1, color = 'blue') +
  scale_x_date(labels = date_format("%Y"), date_breaks="1 year",
               expand=c(0, .9)) + 
  xlab("Year") + 
  ylab("No. of deaths") + 
  theme_light(base_size=16) +
  theme(plot.title = element_text(size=14)) +
  labs(title="Figure 2. Original Serfling equation fit to 2011/12") +
  theme(axis.text.x=element_text(angle=90, hjust=1))

g_mod_1

## ------------------------------------------------------------------------
df_high <- df_cdc %>%
  dplyr::filter(yrweek_dt >= ymd('2012-10-01') & yrweek_dt <= ymd('2013-09-30')) %>%
  mutate(t = row_number(), #set origin to october
         theta = 2*t / 52,
         sin_2 = sinpi(theta),
         cos_2 = cospi(theta)) 

df_pred <- predict(mod_1_low, newdata=df_high, se.fit=TRUE, 
           interval="prediction", level=0.90)  

pred_yhat_0 <- df_pred$fit[,1] #fitted values
pred_yhat_upr <- df_pred$fit[,3] 

df_mod_1 <- df_high %>%
  add_column(., pred_yhat_0, pred_yhat_upr)  

## ---- fig.width=7--------------------------------------------------------
g_mod_1 <- ggplot(df_mod_1) + 
  geom_line(aes(x=yrweek_dt, y=fludeaths), size=0.8, alpha=0.5) +
  geom_line(aes(x=yrweek_dt, y=pred_yhat_0), linetype='dashed', size=1, color = 'blue') +
  geom_line(aes(x=yrweek_dt, y=pred_yhat_upr), linetype='dotted', color='red', size=1) +
  scale_x_date(labels = date_format("%Y"), date_breaks="1 year",
               expand=c(0, .9)) + 
  xlab("Year") + 
  ylab("No. of deaths") + 
  theme_light(base_size=16) +
  theme(plot.title = element_text(size=14)) +
  labs(title="Figure 3. 2011/12 estimates fitted to 2012/13 season") +
  theme(axis.text.x=element_text(angle=90, hjust=1))

g_mod_1

## ------------------------------------------------------------------------
df_cdc_2  <- df_cdc %>%
  dplyr::filter(fluyr<2014) %>%
  mutate(t = row_number(), #set origin to october
         theta = 2*t / 52,
         sin_2 = sinpi(theta),
         cos_2 = cospi(theta)) 

mod_1_sec_fit <- df_cdc_2 %>%
  lm(fludeaths ~ t + sin_2 + cos_2, #note the 0 term
    data=., na.action = na.exclude) 

df_pred <- predict(mod_1_sec_fit, newdata = df_cdc, se.fit=TRUE, 
           interval="prediction", level=0.90)  

pred_yhat_0 <- df_pred$fit[,1] #fitted values
pred_yhat_upr <- df_pred$fit[,3] 

df_mod_1_sec <- df_cdc %>%
  add_column(., pred_yhat_0, pred_yhat_upr)  

## ---- fig.width=7--------------------------------------------------------
g_mod_1 <- ggplot(df_mod_1_sec[df_mod_1_sec$fluyr<2014, ]) + 
  geom_line(aes(x=yrweek_dt, y=fludeaths), size=0.8, alpha=0.5) +
  geom_line(aes(x=yrweek_dt, y=pred_yhat_0), linetype='dashed', size=1, color = 'blue') +
  geom_line(aes(x=yrweek_dt, y=pred_yhat_upr), linetype='dotted', color='red', size=1) +
  scale_x_date(labels = date_format("%Y"), date_breaks="1 year",
               expand=c(0, .9)) + 
  xlab("Year") + 
  ylab("No. of deaths") + 
  theme_light(base_size=16) +
  theme(plot.title = element_text(size=14)) +
  labs(title="Figure 3. 2010 - 2014 estimates fitted") +
  theme(axis.text.x=element_text(angle=90, hjust=1))

g_mod_1

## ---- fig.width=7--------------------------------------------------------
g_mod_1 <- ggplot(df_mod_1_sec[df_mod_1_sec$fluyr==2014, ]) + 
  geom_line(aes(x=yrweek_dt, y=fludeaths), size=0.8, alpha=0.5) +
  geom_line(aes(x=yrweek_dt, y=pred_yhat_0), linetype='dashed', size=1, color = 'blue') +
  geom_line(aes(x=yrweek_dt, y=pred_yhat_upr), linetype='dotted', color='red', size=1) +
  scale_x_date(labels = date_format("%Y"), date_breaks="1 year",
               expand=c(0, .9)) + 
  xlab("Year") + 
  ylab("No. of deaths") + 
  theme_light(base_size=16) +
  theme(plot.title = element_text(size=14)) +
  labs(title="Figure 3. 2010 - 2014 estimates fitted to 2014/2015 season") +
  theme(axis.text.x=element_text(angle=90, hjust=1))

g_mod_1

## ------------------------------------------------------------------------
df_rpt <- df_mod_1_sec %>%
  dplyr::filter(fluyr==2014) %>%
  select(fluyr, week, fludeaths, pred_yhat_0, pred_yhat_upr) %>%
  rowwise() %>%
  mutate(excess_deaths = max(fludeaths - pred_yhat_upr, 0)) %>%
  round(., 0) 

head(df_rpt)

## ------------------------------------------------------------------------
total <- formatC(sum(df_rpt$fludeaths), format = 'd', big.mark = ',')
total_excess <- formatC(sum(df_rpt$excess_deaths), format = 'd', big.mark = ',')

## ---- results='as.is', fig.width=7---------------------------------------
## Add fourier term
fit <- fludta %>%
  dplyr::filter(year>=2010) %>%
  mutate(week2 = row_number(),
         theta = 2*week2/52,
         sin_f1 = sinpi(theta),
         cos_f1 = cospi(theta),
         pred_y = predict( lm(perc_fludeaths ~ 
                                week2 + sin_f1 + cos_f1, data=.)))

## ---- echo=F, results='as.is', fig.width=7-------------------------------
g1 <- ggplot(fit, aes(x=yrweek_dt)) + 
  geom_line(aes(y=perc_fludeaths), size=0.8, colour="#CC0000") +
  geom_line(aes(y=pred_y), size=0.8, colour="black") +
  scale_x_date(labels = date_format("%Y"), date_breaks="1 year",
               expand=c(0, .9)) + 
  xlab("Year") + 
  ylab("% of Deaths from P&I") + 
  theme_light(base_size=16) +
  theme(plot.title = element_text(size=14)) +
  labs(title="Figure 3. Pneumonia and Influenza Mortality",
       caption="Serfling Model")
g1

## ------------------------------------------------------------------------
##The following command completes the above steps 
##fit serfling model
flu_fit <- fluserf(data=df_cdc, outc=perc_fludeaths, time=yrweek_dt)
flu_fit

## ---- results='as.is', fig.width=7.0-------------------------------------
fluplot(flu_fit, xvar=yrweek_dt, perc_fludeaths, y0, y0_ul,
                    ylab="% Deaths from P&I", title="Serfling Model")

## ------------------------------------------------------------------------
sessioninfo::session_info()

