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

#Import & Tidy----

load(file = "data/ahrf/ahrf.rda")

tidyAHRF <- function(data, state_fips, county_fips, state, year) {
  end_year <- year
  
  #tidy the data
  
  ##County Level Data
  ahrf_pop_county <- ahrf %>%
    #select relevent data points
    select(
      F00011,
      #state fips
      F00012,
      #county fips
      F1198417,
      # population
      F1198416,
      # population
      F1198415,
      # population
      F1198414,
      # population
      F1198413,
      # population
      F1198412,
      # population
      F1198411,
      # population
      F0453010
    ) %>% # population
    rename(F1198410 = F0453010) %>% #get consistent numbering for population
    rename(state_fips_code = F00011) %>% #rename state fips code
    rename(county_fips_code = F00012) %>% #rename county fips code
    filter(state_fips_code %in% state_fips) %>% #filter state
    filter(county_fips_code %in% county_fips) %>% #filter county
    gather(key = specialty,
           value = population,
           -state_fips_code,
           -county_fips_code) %>%
    separate(col = specialty,
             into = c("code", "year"),
             sep = -2) %>%
    mutate(year = paste0("20", year)) %>%
    filter(year <= end_year) %>%
    select(-code) %>%
    as.tibble
  
  ahrf_sub_county <- ahrf %>%
    #select relevent data points
    select(
      F00011,
      #state fips
      F00012,
      #county fips
      F1467516,
      #Phys,Primary Care, Patient Care Non-Fed;Excl Hsp Res & 75+ Yrs
      F1467515,
      #Phys,Primary Care, Patient Care Non-Fed;Excl Hsp Res & 75+ Yrs
      F1467514,
      #Phys,Primary Care, Patient Care Non-Fed;Excl Hsp Res & 75+ Yrs
      F1467513,
      #Phys,Primary Care, Patient Care Non-Fed;Excl Hsp Res & 75+ Yrs
      F1467512,
      #Phys,Primary Care, Patient Care Non-Fed;Excl Hsp Res & 75+ Yrs
      F1467511,
      #Phys,Primary Care, Patient Care Non-Fed;Excl Hsp Res & 75+ Yrs
      F1467510,
      #Phys,Primary Care, Patient Care Non-Fed;Excl Hsp Res & 75+ Yrs
      F1465017,
      # Dentists w/NPI
      F1465016,
      # Dentists w/NPI
      F1465015,
      # Dentists w/NPI
      F1465014,
      # Dentists w/NPI
      F1465013,
      # Dentists w/NPI
      F1465012,
      # Dentists w/NPI
      F1465011,
      # Dentists w/NPI
      F1465010,
      # Dentists w/NPI
      F0477316,
      # psychiatrists
      F0477315,
      # psychiatrists
      F0477310,
      # psychiatrists
      F1170416,
      # pediatricians
      F1170415,
      # pediatricians
      F1170410
    ) %>% #pediatricians
    rename(state_fips_code = F00011) %>% #rename state fips code
    rename(county_fips_code = F00012) %>% #rename county fips code
    filter(state_fips_code %in% state_fips) %>% #filter state
    filter(county_fips_code %in% county_fips) %>% #filter county
    gather(key = specialty,
           value = count,
           -state_fips_code,
           -county_fips_code) %>%
    separate(col = specialty,
             into = c("specialty", "year"),
             sep = -2) %>%
    mutate(year = paste0("20", year)) %>%
    as.tibble %>%
    filter(year <= end_year)
  
  ahrf_tidy_county <-
    left_join(
      ahrf_sub_county,
      ahrf_pop_county,
      by = c("state_fips_code", "county_fips_code", "year")
    ) %>%
    mutate(ratio = population / count) %>%
    mutate(ratio = round(ratio, digits = 0)) %>%
    mutate(specialty =
             str_replace_all(
               specialty,
               c(
                 "F14675" = "Primary Care Physicians",
                 "F14650" = "Dentists",
                 "F04773" = "Psychiatrists",
                 "F11704" = "Pediatricians"
               )
             ))
  
  end_year <- year
  #tidy the data
  
  ##State level data
  
  ahrf_pop_state <- ahrf %>%
    #select relevent data points
    select(
      F00011,
      #state fips
      F1198417,
      # population
      F1198416,
      # population
      F1198415,
      # population
      F1198414,
      # population
      F1198413,
      # population
      F1198412,
      # population
      F1198411,
      # population
      F0453010
    ) %>% # population
    rename(F1198410 = F0453010) %>% #get consistent numbering for population
    rename(state_fips_code = F00011) %>% #rename state fips code
    filter(state_fips_code %in% state_fips) %>% #filter state
    gather(key = specialty, value = population,-state_fips_code) %>%
    separate(col = specialty,
             into = c("code", "year"),
             sep = -2) %>%
    mutate(year = paste0("20", year)) %>%
    group_by(year, state_fips_code, code) %>%
    summarise(population = sum(population)) %>%
    select(-code) %>%
    filter(year <= end_year)
  
  
  
  ahrf_sub_state <- ahrf %>%
    #select relevent data points
    select(
      F00011,
      #state fips
      F1467516,
      #Phys,Primary Care, Patient Care Non-Fed;Excl Hsp Res & 75+ Yrs
      F1467515,
      #Phys,Primary Care, Patient Care Non-Fed;Excl Hsp Res & 75+ Yrs
      F1467514,
      #Phys,Primary Care, Patient Care Non-Fed;Excl Hsp Res & 75+ Yrs
      F1467513,
      #Phys,Primary Care, Patient Care Non-Fed;Excl Hsp Res & 75+ Yrs
      F1467512,
      #Phys,Primary Care, Patient Care Non-Fed;Excl Hsp Res & 75+ Yrs
      F1467511,
      #Phys,Primary Care, Patient Care Non-Fed;Excl Hsp Res & 75+ Yrs
      F1467510,
      #Phys,Primary Care, Patient Care Non-Fed;Excl Hsp Res & 75+ Yrs
      F1465017,
      # Dentists w/NPI
      F1465016,
      # Dentists w/NPI
      F1465015,
      # Dentists w/NPI
      F1465014,
      # Dentists w/NPI
      F1465013,
      # Dentists w/NPI
      F1465012,
      # Dentists w/NPI
      F1465011,
      # Dentists w/NPI
      F1465010,
      # Dentists w/NPI
      F0477316,
      # psychiatrists
      F0477315,
      # psychiatrists
      F0477310,
      # psychiatrists
      F1170416,
      # pediatricians
      F1170415,
      # pediatricians
      F1170410
    ) %>% #pediatricians
    rename(state_fips_code = F00011) %>% #rename state fips code
    filter(state_fips_code %in% state_fips) %>% #filter state
    gather(key = specialty, value = count,-state_fips_code) %>%
    separate(col = specialty,
             into = c("specialty", "year"),
             sep = -2) %>%
    mutate(year = paste0("20", year)) %>%
    group_by(year, state_fips_code, specialty) %>%
    summarise(count = sum(count)) %>%
    filter(year <= end_year)
  
  
  ahrf_tidy_state <-
    left_join(ahrf_sub_state,
              ahrf_pop_state,
              by = c("state_fips_code", "year")) %>%
    
    mutate(specialty =
             str_replace_all(
               specialty,
               c(
                 "F14675" = "Primary Care Physicians",
                 "F14650" = "Dentists",
                 "F04773" = "Psychiatrists",
                 "F11704" = "Pediatricians"
               )
             )) %>%
    group_by(specialty, year) %>%
    #filter(year == 2011) %>%
    mutate(ratio = population / count) %>%
    mutate(ratio = round(ratio, digits = 0)) %>%
    mutate(county_fips_code = state)
  
  ahrf_tidy <- union(ahrf_tidy_county, ahrf_tidy_state)
  
  ahrf_tidy
}

ahrf_tidy <-
  tidyAHRF(ahrf, 39, c("165", "041", "103"), "Ohio", year = 2016)

#visualize----

primary_care_graph <- ahrf_tidy %>%
  filter(specialty == "Primary Care Physicians") %>%
  mutate(county_fips_code =
           str_replace_all(
             county_fips_code,
             c(
               "165" = "Warren County",
               "041" = "Delaware County",
               "103" = "Medina County",
               "39" = "Ohio"
             )
           )) %>%
  ggplot(aes(
    x = year,
    y = ratio,
    group = county_fips_code,
    color = county_fips_code
  )) +
  geom_line(size = 1) +
  labs(
    title = 'Ratio of Primary Care Physicians to Population',
    subtitle = paste((year - 5), "to", year),
    caption = "Source: 2018 Area Health Resource File \n U.S. Health Resources & Services Administration",
    y = "Population:Physician Ratio",
    x = "Year",
    color = "Geography",
    fill = "Geography"
  )


ggsave(
  plot = primary_care_graph,
  file = paste(
    "charts/",
    paste(year, county, state, "primary_care_graph.png", sep = "_"),
    sep = ""
  ),
  width = 6,
  height = 4.5,
  type = "cairo-png"
)

dentist_graph <- ahrf_tidy %>%
  filter(specialty == "Dentists") %>%
  mutate(county_fips_code =
           str_replace_all(
             county_fips_code,
             c(
               "165" = "Warren County",
               "041" = "Delaware County",
               "103" = "Medina County",
               "39" = "Ohio"
             )
           )) %>%
  ggplot(aes(
    x = year,
    y = ratio,
    group = county_fips_code,
    color = county_fips_code
  )) +
  geom_line(size = 1) +
  labs(
    title = 'Ratio of Dentists to Population',
    subtitle = paste((year - 5), "to", year),
    caption = "Source: 2018 Area Health Resource File \n U.S. Health Resources & Services Administration",
    y = "Population:Dentist Ratio",
    x = "Year",
    color = "Geography",
    fill = "Geography"
  )

ggsave(
  plot = dentist_graph,
  file = paste(
    "charts/",
    paste(year, county, state, "dentist_graph.png", sep = "_"),
    sep = ""
  ),
  width = 6,
  height = 4.5,
  type = "cairo-png"
)
