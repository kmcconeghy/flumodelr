## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(tibble.print_min = 4L, tibble.print_max = 4L)
library(flumodelr)
library(lubridate)
library(scales)

## ------------------------------------------------------------------------
df_cdc <- flumodelr::fludta %>%
  dplyr::filter(year>=2010 & year<2016) 

## ---- echo=F, results='as.is', fig.width=7.0-----------------------------
ggplot(df_cdc, aes(x=yrweek_dt)) + 
  geom_line(aes(y=perc_fludeaths, colour="% Deaths from P&I", 
                linetype="% Deaths from P&I"), size=0.8) +
  geom_line(aes(y=prop_flupos*10, colour="No. Positive per 10 isolates", 
                linetype="No. Positive per 10 isolates"), size=0.8) +
  scale_colour_manual("Line",
                      breaks=c("% Deaths from P&I", 
                                 "No. Positive per 10 isolates"),
                      values = c("% Deaths from P&I"="#CC0000", 
                                 "No. Positive per 10 isolates"="black")) +
  scale_linetype_manual("Line", 
                        breaks=c("% Deaths from P&I", 
                                 "No. Positive per 10 isolates"),
                        values = c("% Deaths from P&I"=1,
                                   "No. Positive per 10 isolates"=2)) +
  scale_x_date(labels = date_format("%Y"), date_breaks="1 year",
               expand=c(0, .9)) +
  xlab("Year") + ylab("%") +
  theme_light(base_size=16) +
  theme(plot.title = element_text(size=14)) +
  labs(title="Figure 1. Influenza (+) isolates over time")

## ------------------------------------------------------------------------
head(df_cdc)

## ------------------------------------------------------------------------
df_cdc_2 <- df_cdc %>%
  mutate(t = row_number(), #set origin to october
         theta = 2*t / 52,
         sin_2 = sinpi(theta),
         cos_2 = cospi(theta))

## ------------------------------------------------------------------------
df_cdc_2 <- df_cdc_2 %>%
  mutate(week_2 = t^2,
         week_3 = t^3,
         week_4 = t^4,
         week_5 = t^5)

## ------------------------------------------------------------------------
base_fit <- df_cdc_2 %>%
  lm(perc_fludeaths ~ t + week_2 + week_3 + week_4 + week_5 +
       prop_flupos + sin_2 + cos_2, data=., na.action = na.exclude)
summary(base_fit)

## ------------------------------------------------------------------------
base_pred <- df_cdc_2 %>%
  mutate(prop_flupos = 0) %>% #Note setting to zero
  predict(base_fit, newdata=., se.fit=TRUE, 
          interval="prediction", level=0.90)

## ------------------------------------------------------------------------
fludta_fitted <- df_cdc_2 %>%
  add_column(., y0=base_pred$fit[,1], y0_ul=base_pred$fit[,3]) 
head(fludta_fitted)

## ---- echo=F, results='as.is', fig.width=7-------------------------------
#Set up graph labels, line specs
line_names <- c("% Deaths From P&I", "Expected %", "Epidemic Threshold")
line_cols <- c("#CC0000", "black", "black")
line_types <- c(1, 1, 2)
names(line_cols) <- line_names
names(line_types) <- line_names

ggplot(fludta_fitted, aes(x=yrweek_dt)) + 
  geom_line(aes(y=perc_fludeaths, colour=line_names[[1]], 
                linetype=line_names[[1]]), size=0.8) +
  geom_line(aes(y=y0, colour=line_names[[2]], 
                linetype=line_names[[2]]), size=0.8) +
  geom_line(aes(y=y0_ul, colour=line_names[[3]], 
                linetype=line_names[[3]]), size=0.8) +
  scale_colour_manual("Line", breaks=line_names, values = line_cols) +
  scale_linetype_manual("Line",  breaks=line_names, values = line_types) +
  scale_x_date(labels = date_format("%Y"), date_breaks="1 year",
               expand=c(0, .9)) + 
  xlab("Year") + 
  ylab("% of Deaths from P&I") + 
  theme_light(base_size=14) +
  theme(legend.text=element_text(size=10), 
        plot.title = element_text(size=14)) +
  labs(title="Figure 2. Pneumonia and Influenza Mortality",
       caption="Modified Serfling Model") +
  guides(colour = guide_legend("Line"), linetype = guide_legend("Line"))

## ------------------------------------------------------------------------
df_excess <- fludiff(fludta_fitted, obsvar=perc_fludeaths, fitvar=y0_ul)
df_excess %>%
  head(.)

## ---- echo=F, results='as.is', fig.width=7.0-----------------------------
ggplot(df_excess, aes(x=yrweek_dt)) + 
  geom_line(aes(y=y_diff, colour="Epidemic mortality"), size=0.8, linetype=2) +
  geom_line(aes(y=perc_fludeaths, colour="Reported mortality"), size=0.8, linetype=1) +
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
df_excess <- fludiff(fludta_fitted, obsvar=perc_fludeaths, fitvar=y0)
df_excess %>%
  head(.)

## ---- echo=F, results='as.is', fig.width=7.0-----------------------------
ggplot(df_excess, aes(x=yrweek_dt)) + 
  geom_line(aes(y=y_diff, colour="Excess mortality"), size=0.8, linetype=2) +
  geom_line(aes(y=perc_fludeaths, colour="Reported mortality"), size=0.8, linetype=1) +
  scale_x_date(labels = date_format("%Y"), date_breaks="1 year",
               expand=c(0, .9)) + 
  scale_colour_manual("Line",
                      values = c("Excess mortality"="#CC0000", 
                                 "Reported mortality"="black")) +
  xlab("Year") + 
  ylab("% Deaths from P&I") + 
  theme_light(base_size=16) +
  theme(plot.title = element_text(size=14)) +
  labs(title="Figure 4. Periods of excess mortality over time")

## ------------------------------------------------------------------------
fludta_mod <- flum(df_cdc_2, model="ird", 
                   outc=perc_fludeaths, time=yrweek_dt,
                   viral=prop_flupos)

head(fludta_mod)

## ------------------------------------------------------------------------
fludta_mod <- flum(df_cdc_2, model="fluserf", 
                   outc=perc_fludeaths, time=yrweek_dt)

fludta_mod %>% 
  select(year, week, perc_fludeaths, y0, y0_ul) %>%
  head()

## ---- eval=F-------------------------------------------------------------
#  fludta_mod %>%
#    dplyr::filter(!is.na(prop_flupos)) %>%
#  flum(., model="fluglm",
#              outc=fludeaths, time=t,
#              viral = "prop_flupos") %>%
#  head(.)

## ------------------------------------------------------------------------
## Without polynomial terms
flum(df_cdc_2, model="fluglm", 
     outc=fludeaths, time=week_in_order, 
     viral = "prop_flupos", poly=F)

## ------------------------------------------------------------------------
## Epidemic period, non-specified
flum(df_cdc_2, model="fluglm", 
     outc=fludeaths, time=t, 
     season=T)

## ------------------------------------------------------------------------
## Epidemic period specified
fludta_mod <- ird(data=df_cdc_2, 
               outc = perc_fludeaths, viral=prop_flupos, time=t)

flum(data=fludta_mod, model="fluglm", outc=fludeaths, time=t, 
       season=high) %>%
  head(.)

## ------------------------------------------------------------------------
## Poisson model with offset term
flum(df_cdc_2, 
     model="fluglm", outc = fludeaths, 
     time = t, season=T, 
     family=poisson, offset=log(alldeaths)) %>%
  head(.)

## ------------------------------------------------------------------------
## Negative binomial model with offset term
flum(df_cdc_2, 
     model="fluglm", outc = fludeaths, 
     time = t, viral='prop_flupos',
     glmnb = T) %>%
head(.)

## ------------------------------------------------------------------------
sessioninfo::session_info()

