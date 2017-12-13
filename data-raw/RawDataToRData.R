setwd("~/GitHub/flumodelr/data-raw")
library('dplyr')
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
    
#--Make example time-series object  
  nrevss2 <- nrevss %>%
      mutate(spec_pos = rowSums(select(., starts_with('type_')), na.rm=T)) %>%
      group_by(year, week) %>%
      summarize(spec_tot = sum(spec_tot, na.rm=T),
                spec_pos = sum(spec_pos, na.rm=T),
                prop_flupos = spec_pos / spec_tot) %>%
    mutate(yrweek_dt = as.POSIXct(paste0(year, "-", week, "-", "1"), format = "%Y-%U-%u"),
           yrweek_dt = as_date(yrweek_dt) + dweeks(2)) #Add 2 weeks for delay
                
    flu_ex <- cdc122city %>%
      arrange(year, week) %>%
      group_by(year, week) %>%
      summarize(fludeaths = mean(deaths_pnaflu, na.rm=T)) %>%
      mutate(yrweek_dt = as.POSIXct(paste0(year, "-", week, "-", "1"), format = "%Y-%U-%u"),
             yrweek_dt = as_date(yrweek_dt)) %>%
      na.omit() %>% #drop missing
      ungroup() %>%
      mutate(epi = if_else(yrweek_dt >= date(paste0(year(yrweek_dt), "-10-01")) |
                           yrweek_dt <= date(paste0(year(yrweek_dt), "-05-31")), T, F)) %>%
      inner_join(., nrevss2[,c('yrweek_dt', 'prop_flupos')], 
                 by=c('yrweek_dt')) #visual inspection

    #--Save for package
    use_data(flu_ex, overwrite = T)
    