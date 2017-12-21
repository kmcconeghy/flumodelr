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
flu_ex <- flumodelr::flu_ex

g1 <- ggplot(flu_ex, aes(x=yrweek_dt, y=perc_fludeaths)) + 
  geom_line(size=0.8, colour="#CC0000") +
  scale_x_date(labels = date_format("%Y"), date_breaks="1 year",
               expand=c(0, .9)) + 
  xlab("Year") + 
  ylab("% of Deaths from P&I") + 
  theme_light(base_size=16) +
  theme(plot.title = element_text(size=14)) +
  labs(title="Figure 1. Pneumonia and Influenza Mortality")
g1

## ---- echo=F, results='asis', fig.width=7--------------------------------
g1 <- ggplot(flu_ex, aes(x=yrweek_dt, y=perc_fludeaths)) + 
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

## ---- results='as.is', fig.width=7---------------------------------------
## Add fourier term
fit <- flu_ex %>%
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
flu_ex %>% select(yrweek_dt, perc_fludeaths)

## ------------------------------------------------------------------------
flu_ex <- flu_ex %>%
  mutate(epi = if_else(month(yrweek_dt)>=10 | month(yrweek_dt)<=5, T, F))  

flu_ex %>%
  filter(year>=2010)

## ------------------------------------------------------------------------
## Compute fourier terms
flu_ex_serfling <- flu_ex %>%
  mutate(week2 = row_number(),
         theta = 2*week2/52,
         sin_f1 = sinpi(theta),
         cos_f1 = cospi(theta))

## ------------------------------------------------------------------------
base_fit <- flu_ex_serfling %>%
  dplyr::filter(epi==F & year<2015) %>%
  lm(perc_fludeaths ~ week2 + sin_f1 + cos_f1, 
     data=., na.action = na.exclude)

## ------------------------------------------------------------------------
## Fitted values + prediction interval
df_pred <- flu_ex_serfling %>%
  dplyr::filter(year==2014 & week>=40 | year>2014) %>%
  predict(base_fit, newdata=., se.fit=TRUE, 
          interval="prediction", level=0.90)

pred_y0 <- df_pred$fit[,1] #fitted values
pred_y0_serf <- df_pred$fit[,3] 

df_base <- flu_ex %>%
  dplyr::filter(year==2014 & week>=40 | year>2014) %>%
  add_column(., pred_y0, pred_y0_serf)  

df_base %>% select(year, week, perc_fludeaths, 
                   pred_y0, pred_y0_serf)  

## ---- echo=F, results='as.is', fig.width=7-------------------------------
#Set up graph labels, line specs
line_names <- c("% Deaths From P&I", "Expected %", "Epidemic Threshold")
line_cols <- c("#CC0000", "black", "black")
line_types <- c(1, 1, 2)
names(line_cols) <- line_names
names(line_types) <- line_names
  
ggplot(df_base, aes(x=yrweek_dt)) + 
  geom_line(aes(y=perc_fludeaths, colour=line_names[[1]], 
                linetype=line_names[[1]]), size=0.8) +
  geom_line(aes(y=pred_y0, colour=line_names[[2]], 
                linetype=line_names[[2]]), size=0.8) +
  geom_line(aes(y=pred_y0_serf, colour=line_names[[3]], 
                linetype=line_names[[3]]), size=0.8) +
  scale_colour_manual("Line", breaks=line_names, values = line_cols) +
  scale_linetype_manual("Line", breaks=line_names, values = line_types) +
  scale_x_date(labels = date_format("%b"), date_breaks="1 month",
               expand=c(0, .9)) + 
  xlab("2014 - 2015 Influenza Season") + ylab("% of Deaths from P&I") + 
  theme_light(base_size=14) +
  theme(legend.text=element_text(size=10), 
        plot.title = element_text(size=14)) +
  labs(title="Figure 4. Pneumonia and Influenza Mortality") +
  guides(colour = guide_legend("Line"), linetype = guide_legend("Line"))

## ------------------------------------------------------------------------
df_excess <- df_base %>%
  mutate(threshold = if_else(perc_fludeaths > pred_y0_serf, T, F),
         simple_excess = if_else((perc_fludeaths - pred_y0_serf)>0,
         perc_fludeaths - pred_y0_serf, 0))

## ------------------------------------------------------------------------
## Identify consecutive epidemic periods  
  df_serf_excess <- df_excess %>%
    mutate(serf_rule = if_else(threshold==T & 
                                 lead(threshold)==T & 
                                 lag(threshold)==T, T, F))
##reset beginning and end periods
  df_serf_excess <- df_serf_excess %>%
    mutate(serf_rule = if_else(serf_rule==F & 
                                 threshold==T & 
                                 lead(serf_rule)==T, T, serf_rule),
           serf_rule = if_else(serf_rule==F, 
                               threshold==T & 
                               lag(serf_rule)==T, T, serf_rule))
  
 ##compute excess deaths
   df_serf_excess <- df_serf_excess %>%
     mutate(serf_excess = if_else(serf_rule==T, 
                                  perc_fludeaths - pred_y0_serf, 0))


df_serf_excess %>% select(year, week, threshold, simple_excess, serf_rule, serf_excess)

## ---- echo=F, results='as.is', fig.width=7.0-----------------------------
ggplot(df_serf_excess, aes(x=yrweek_dt)) + 
  geom_line(aes(y=simple_excess, colour="Excess mortality"), size=0.8, linetype=2) +
  geom_line(aes(y=perc_fludeaths, colour="Reported mortality"), size=0.8, linetype=1) +
  scale_x_date(labels = date_format("%b"), date_breaks="1 month",
               expand=c(0, .9)) + 
  scale_colour_manual("Line",
                      values = c("Excess mortality"="#CC0000", 
                                 "Reported mortality"="black")) +
  xlab("2014- 2015 Influenza Year") + 
  ylab("% Deaths from P&I") + 
  theme_light(base_size=16) +
  theme(plot.title = element_text(size=14)) +
  labs(title="Figure 5. Periods of excess mortality over time")

## ------------------------------------------------------------------------
##The following command completes the above steps 
##fit serfling model
flu_fit <- serflm(data=flu_ex, outc=perc_fludeaths, time=yrweek_dt)
flu_fit

## ------------------------------------------------------------------------
fluplot(flu_fit, xvar=yrweek_dt, perc_fludeaths, y0, y0_ul,
                    ylab="% Deaths from P&I", title="Serfling Model")

