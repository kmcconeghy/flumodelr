setwd("~/GitHub/flumodelr/data-raw")
library('tidyverse')
library('readr')
library('devtools')
library('lubridate')

##--ILINET
df.ILI <- read_csv('ILINET.csv', skip=1)
head(df.ILI)

  #--Some editing
  ilinet <- df.ILI %>%
      select(REGION, YEAR, WEEK, `%UNWEIGHTED ILI`, 
             ILITOTAL, `NUM. OF PROVIDERS`, `TOTAL PATIENTS`) %>%
      rename(state = REGION,
             year = YEAR,
             week = WEEK,
             ili_perc = `%UNWEIGHTED ILI`,
             ili_tot = ILITOTAL,
             providers = `NUM. OF PROVIDERS`,
             patients = `TOTAL PATIENTS`) %>%
      mutate(ili_perc = as.numeric(ili_perc),
             ili_tot = as.integer(ili_tot),
             providers = as.integer(providers),
             patients = as.integer(patients))
    head(ilinet)
    summary(ilinet)
  #--Save for package
  use_data(ilinet, overwrite = T)
    
##--WHO_NREVSS_Labs
  df.Labs <- read_csv('WHO_NREVSS_Combined_prior_to_2015_16.csv', skip=1)
  nrevss <- df.Labs %>%
    select(REGION, YEAR, WEEK, `TOTAL SPECIMENS`, 
           `PERCENT POSITIVE`, 
           starts_with('A '), starts_with('B'), H3N2v) %>%
    rename(state = REGION,
           year = YEAR,
           week = WEEK,
           spec_tot = `TOTAL SPECIMENS`,
           spec_pos = `PERCENT POSITIVE`,
           type_A_h1n1 = `A (2009 H1N1)`,
           type_A_h1 = `A (H1)`,
           type_A_h3 = `A (H3)`,
           type_A_np = `A (Subtyping not Performed)`,
           type_A_us = `A (Unable to Subtype)`,
           type_B = B,
           type_A_h3n2 = H3N2v) %>%
    mutate(spec_tot = as.integer(spec_tot),
           spec_pos = as.numeric(spec_pos)) %>%
    mutate_at(vars(starts_with('type')), funs(as.integer))
    head(nrevss)
    summary(nrevss)
    #--Save for package
    use_data(nrevss, overwrite = T)
    
##--CDC 122-cities data
    df.122cities <- read_csv('Deaths_in_122_U.S._cities_-_1962-2016._122_Cities_Mortality_Reporting_System.csv')
    cdc122city <- df.122cities %>%
      select(REGION, State, City, Year, WEEK, 
             `Pneumonia and Influenza Deaths`,
             `All Deaths`,
             `65+ years (all cause deaths)`) %>%
      rename(region = REGION,
             state = State,
             city = City,
             year = Year,
             week = WEEK,
             deaths_pnaflu = `Pneumonia and Influenza Deaths`,
             deaths_allcause = `All Deaths`,
             deaths_65older = `65+ years (all cause deaths)`)
    head(cdc122city)
    summary(cdc122city)
    #--Save for package
    use_data(cdc122city, overwrite = T)

    EpiWeekToDates <- function(year, weeknum) {
      
      #Compute 4th day in January
      jan4 <- ymd(paste(year, 1, 4, sep="-"))
      
      #Compute day of week
      DofW <- wday(jan4, week_start = 7)-1 #Sunday start
      #The -1 is  correction for how wday() counts days  
      
      #If Jan 4th was Sun, starting week date is 01/04/xxxx
      startweek <- if_else(DofW == 7, jan4, (jan4 - (DofW)))
      
      #First Date of Week
      d0 = startweek + (weeknum - 1) * 7
      
      #Last Date of Week
      d1 = startweek + (weeknum - 1) * 7 + 6 
      
      return(list(d0 = ymd(d0), d1 = ymd(d1)))
    }
    
#--Make example time-series object  
  nrevss2 <- nrevss %>%
      mutate(spec_pos = rowSums(select(., starts_with('type_')), na.rm=T)) %>%
      group_by(year, week) %>%
      summarize(spec_tot = sum(spec_tot, na.rm=T),
                spec_pos = sum(spec_pos, na.rm=T),
                prop_flupos = spec_pos / spec_tot) %>%
    mutate(yrweek_dt = EpiWeekToDates(year, week)[[1]],
           yrweek_dt = yrweek_dt + dweeks(2)) #Add 2 weeks for delay
  
    fludta <- cdc122city %>%
      arrange(year, week) %>%
      group_by(year, week) %>%
      summarize(fludeaths = sum(deaths_pnaflu, na.rm=T),
                alldeaths = sum(deaths_allcause, na.rm=T),
                perc_fludeaths = fludeaths/alldeaths*100) %>%
      mutate(yrweek_dt = EpiWeekToDates(year, week)[[1]],
             fluyr = case_when(
               month(yrweek_dt)>=6 ~ year(yrweek_dt),
               month(yrweek_dt)<=5  ~ year(yrweek_dt)-1)
             ) %>%
      na.omit() %>% #drop missing
      ungroup() %>%
      inner_join(., nrevss2[,c('yrweek_dt', 'prop_flupos')], 
                 by=c('yrweek_dt')) %>% #visual inspection
      mutate(week_in_order = row_number(),
             epi = if_else(month(yrweek_dt)>=10 | month(yrweek_dt)<=5, T, F))

    #--Save for package
    use_data(fludta, overwrite = T)
    