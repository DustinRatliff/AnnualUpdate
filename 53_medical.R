# Import using SAScii read.SAScii function from the SAScii
# package, then save as .rda
# 
# ahrf <- read.SAScii(fn = "data/ahrf/ahrf2018.asc",
#                     sas_ri = "data/ahrf/AHRF2017-18.sas")
# 
# save(ahrf, file="data/ahrf/ahrf.rda")
# 
# Use the AHRF Technical Documentation to find the appropriate
# columns to extract

load(file="data/ahrf/ahrf.rda")

tidyAHRFCounty <- function(data, state_fips, county_fips) {

#tidy the data

ahrf_pop <- ahrf %>%
  #select relevent data points
  select(
    F00011,  #state fips
    F00012,   #county fips
    F1198417, # population
    F1198416, # population
    F1198415, # population
    F1198414, # population
    F1198413, # population
    F1198412, # population
    F1198411, # population
    F0453010) %>% # population
  rename(F1198410 = F0453010) %>% #get consistent numbering for population
  rename(state_fips_code = F00011) %>% #rename state fips code
  rename(county_fips_code = F00012) %>% #rename county fips code
  filter(state_fips_code %in% state_fips) %>% #filter state
  filter(county_fips_code %in% county_fips) %>% #filter county
  gather(key = specialty, value = population, -state_fips_code, -county_fips_code) %>%
  separate(col = specialty, into = c("code", "year"), sep = -2) %>% 
  mutate(year = paste0("20", year)) %>%
  select(-code)

ahrf_sub <- ahrf %>%
#select relevent data points
  select(
    F00011,  #state fips
    F00012,   #county fips
    F1467516, #Phys,Primary Care, Patient Care Non-Fed;Excl Hsp Res & 75+ Yrs
    F1467515, #Phys,Primary Care, Patient Care Non-Fed;Excl Hsp Res & 75+ Yrs
    F1467514, #Phys,Primary Care, Patient Care Non-Fed;Excl Hsp Res & 75+ Yrs
    F1467513, #Phys,Primary Care, Patient Care Non-Fed;Excl Hsp Res & 75+ Yrs
    F1467512, #Phys,Primary Care, Patient Care Non-Fed;Excl Hsp Res & 75+ Yrs
    F1467511, #Phys,Primary Care, Patient Care Non-Fed;Excl Hsp Res & 75+ Yrs
    F1467510, #Phys,Primary Care, Patient Care Non-Fed;Excl Hsp Res & 75+ Yrs
    F1465017, # Dentists w/NPI
    F1465016, # Dentists w/NPI
    F1465015, # Dentists w/NPI
    F1465014, # Dentists w/NPI
    F1465013, # Dentists w/NPI
    F1465012, # Dentists w/NPI
    F1465011, # Dentists w/NPI
    F1465010, # Dentists w/NPI
    F0477316, # psychiatrists
    F0477315, # psychiatrists
    F0477310, # psychiatrists
    F1170416, # pediatricians
    F1170415, # pediatricians
    F1170410 ) %>% #pediatricians
    rename(state_fips_code = F00011) %>% #rename state fips code
    rename(county_fips_code = F00012) %>% #rename county fips code
  filter(state_fips_code %in% state_fips) %>% #filter state
  filter(county_fips_code %in% county_fips) %>% #filter county
    gather(key = specialty, value = count, -state_fips_code, -county_fips_code) %>%
 separate(col = specialty, into = c("specialty", "year"), sep = -2) %>% 
  mutate(year = paste0("20", year))
  
  
ahrf_tidy <-
  left_join(ahrf_sub, ahrf_pop, 
            by=c("state_fips_code", "county_fips_code", "year")) %>%
  mutate(ratio = population/count) %>%
  mutate(ratio = round(ratio, digits = 0)) %>%
  mutate(specialty = 
           str_replace_all(specialty, 
                           c("F14675" ="Primary Care Physicians",
                             "F14650" = "Dentists",
                             "F04773" = "Psychiatrists",
                             "F11704" = "Pediatricians")))
ahrf_tidy
}

tidyAHRFState <- function(data, state_fips) {
  
  #tidy the data
  
  ahrf_pop <- ahrf %>%
    #select relevent data points
    select(
      F00011,  #state fips
      F00012,   #county fips
      F1198417, # population
      F1198416, # population
      F1198415, # population
      F1198414, # population
      F1198413, # population
      F1198412, # population
      F1198411, # population
      F0453010) %>% # population
    rename(F1198410 = F0453010) %>% #get consistent numbering for population
    rename(state_fips_code = F00011) %>% #rename state fips code
    filter(state_fips_code %in% state_fips) %>% #filter state
    gather(key = specialty, value = population, -state_fips_code) %>%
    separate(col = specialty, into = c("code", "year"), sep = -2) %>% 
    mutate(year = paste0("20", year)) %>%
    select(-code)
  
  ahrf_sub <- ahrf %>%
    #select relevent data points
    select(
      F00011,  #state fips
      F1467516, #Phys,Primary Care, Patient Care Non-Fed;Excl Hsp Res & 75+ Yrs
      F1467515, #Phys,Primary Care, Patient Care Non-Fed;Excl Hsp Res & 75+ Yrs
      F1467514, #Phys,Primary Care, Patient Care Non-Fed;Excl Hsp Res & 75+ Yrs
      F1467513, #Phys,Primary Care, Patient Care Non-Fed;Excl Hsp Res & 75+ Yrs
      F1467512, #Phys,Primary Care, Patient Care Non-Fed;Excl Hsp Res & 75+ Yrs
      F1467511, #Phys,Primary Care, Patient Care Non-Fed;Excl Hsp Res & 75+ Yrs
      F1467510, #Phys,Primary Care, Patient Care Non-Fed;Excl Hsp Res & 75+ Yrs
      F1465017, # Dentists w/NPI
      F1465016, # Dentists w/NPI
      F1465015, # Dentists w/NPI
      F1465014, # Dentists w/NPI
      F1465013, # Dentists w/NPI
      F1465012, # Dentists w/NPI
      F1465011, # Dentists w/NPI
      F1465010, # Dentists w/NPI
      F0477316, # psychiatrists
      F0477315, # psychiatrists
      F0477310, # psychiatrists
      F1170416, # pediatricians
      F1170415, # pediatricians
      F1170410 ) %>% #pediatricians
    rename(state_fips_code = F00011) %>% #rename state fips code
    filter(state_fips_code %in% state_fips) %>% #filter state
    gather(key = specialty, value = count, -state_fips_code) %>%
    separate(col = specialty, into = c("specialty", "year"), sep = -2) %>% 
    mutate(year = paste0("20", year))
  
  
  ahrf_tidy <-
    left_join(ahrf_sub, ahrf_pop, 
              by=c("state_fips_code", "year")) %>%

    mutate(specialty = 
             str_replace_all(specialty, 
                             c("F14675" ="Primary Care Physicians",
                               "F14650" = "Dentists",
                               "F04773" = "Psychiatrists",
                               "F11704" = "Pediatricians"))) %>%
    group_by(specialty, year) %>%
    select(-state_fips_code) %>%
    #filter(year == 2011) %>%
    summarise(count = sum(count), 
              population = sum(as.double(population)))

    mutate(ratio = population/count) %>%
    mutate(ratio = round(ratio, digits = 0)) %>%
  ahrf_tidy
}
        
ahrf_tidy <- tidyAHRFData(ahrf, 39, c("165", "041", "103"))

ahrf_tidy %>%
  filter(specialty == "Primary Care Physicians") %>%
  ggplot(aes(x=year, y = ratio, group=county_fips_code, color = county_fips_code)) +
  geom_line()

