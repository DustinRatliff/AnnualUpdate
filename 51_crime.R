#INTRODUCTION
#----
#This is a collection of Offenses Known and Clearances By Arrest data from 1960
#to 2016. Each zip file contains monthly and yearly data files. The monthly
#files contain one data file per year (57 total, 1960-2016) as well as a
#codebook for each year. These files have been read into R using the ASCII and
#setup files from ICPSR (or from the FBI for 2016 data) using the package
#asciiSetupReader. The end of the zip folder's name says what data type (R,
#SPSS, SAS, Microsoft Excel CSV, Stata) the data is in.

#The files are lightly cleaned. What this means specifically is that column
#names and value labels are standardized. In the original data column names were
#different between years (e.g. the December burglaries cleared column is
#"DEC_TOT_CLR_BRGLRY_TOT" in 1975 and "DEC_TOT_CLR_BURG_TOTAL" in 1977). The
#data here have standardized columns so you can compare between years and
#combine years together. The same thing is done for values inside of columns.
#For example, the state column gave state names in some years, abbreviations in
#others. For the code uses to clean and read the data, please see my GitHub file
#here.
#https://github.com/jacobkap/crime_data/blob/master/R_code/offenses_known.R

#The yearly data set is a single file with all years stacked together. It also
#contain far fewer descriptive columns about the agencies in an attempt to
#decrease file size. The data file is aggregated yearly and has already combined
#every year 1960-2016. For the code I used to do this, see here
#https://github.com/jacobkap/crime_data/blob/master/R_code/yearly_offenses_known.R.

#If you find any mistakes in the data or have any suggestions, please email me
#at jkkaplan6@gmail.com

#As a description of what UCR Offenses Known and Clearances By Arrest data
#contains, the following is copied from ICPSR's 2015 page for the data.

#The Uniform Crime Reporting Program Data: Offenses Known and Clearances By
#Arrest dataset is a compilation of offenses reported to law enforcement
#agencies in the United States. Due to the vast number of categories of crime
#committed in the United States, the FBI has limited the type of crimes included
#in this compilation to those crimes which people are most likely to report to
#police and those crimes which occur frequently enough to be analyzed across
#time. Crimes included are criminal homicide, forcible rape, robbery, aggravated
#assault, burglary, larceny-theft, and motor vehicle theft. Much information
#about these crimes is provided in this dataset. The number of times an offense
#has been reported, the number of reported offenses that have been cleared by
#arrests, and the number of cleared offenses which involved offenders under the
#age of 18 are the major items of information collected.

# Import----

crime <-
  read_csv("data/crime/ucr_offenses_known_yearly_1960_2016.csv") %>%
  
  select(
    fips_state_code,
    fips_county_code,
    year,
    mailing_address_line_2,
    population_1,
    mailing_address_line_4,
    act_murder,
    act_manslaughter,
    act_rape_total,
    act_force_rape,
    act_attempted_rape,
    act_robbery_total,
    act_gun_robbery,
    act_knife_robbery,
    act_other_weapon_robbery,
    act_strong_arm_robbery,
    act_assault_total,
    act_gun_assault,
    act_knife_assault,
    act_other_weapon_assault,
    act_hand_feet_assault,
    act_simple_assault,
    act_burglary_total,
    act_burg_force_entry,
    act_burg_no_force_entry,
    act_attempted_burglary,
    act_larceny_total,
    act_mtr_vhc_theft_total,
    act_auto_theft,
    act_truck_bus_theft,
    act_other_vhc_theft,
    act_aggravated_assault
  ) %>%
  rename(yr = year)

# rename("Murder" = act_murder) %>%
#   rename("Manslaughter" = act_manslaughter) %>%
#   rename("Rape - All" = act_rape_total) %>%
#   rename("Rape - Forcible" = act_force_rape) %>%
#   rename("Rape - Attempted" = act_attempted_rape) %>%
#   rename("Robbery - All" = act_robbery_total) %>%
#   rename("Robbery - Gun" = act_gun_robbery) %>%
#   rename("Robbery - Knife" = act_knife_robbery) %>%
#   rename("Robbery - Other Weapon" = act_other_weapon_robbery) %>%
#   rename("Robbery - Strong Arm" = act_strong_arm_robbery) %>%
#   rename("Assault - All" = act_assault_total) %>%
#   rename("Assault - Gun" = act_gun_assault) %>%
#   rename("Assault - Knife" = act_knife_assault) %>%
#   rename("Assault - Other Weapon" = act_other_weapon_assault) %>%
#   rename("Assault - Hand or Feet" = act_hand_feet_assault) %>%
#   rename("Assault - Simple" = act_simple_assault) %>%
#   rename("Burglary - All" = act_burglary_total) %>%
#   rename("Burglary - Forced Entry" = act_burg_force_entry) %>%
#   rename("Burglary - No Forced Entry" = act_burg_no_force_entry) %>%
#   rename("Burglary - Attempted" = act_attempted_burglary) %>%
#   rename("Larceny" = act_larceny_total) %>%
#   rename("Motor Vehicle Theft - All" = act_mtr_vhc_theft_total) %>%
#   rename("Motor Vehicle Theft - Auto" = act_auto_theft) %>%
#   rename("Motor Vehicle Theft - Truck or Bus" = act_truck_bus_theft) %>%
#   rename("Motor Vehicle Theft - Other" = act_other_vhc_theft) %>%
#   rename("Aggravated Assault" = act_aggravated_assault)

# Analyze----



crime_rate <-
  crime %>%
  filter(fips_state_code == 39) %>%
  filter(yr >= year-5 & yr < year+1) %>%
  mutate(property_crime = act_burglary_total +
           act_larceny_total + act_mtr_vhc_theft_total) %>%
  mutate(violent_crime =  act_murder + act_manslaughter +
           act_rape_total + act_assault_total +
           act_aggravated_assault) %>%
  select(yr,
         act_rape_total,
         population_1,
         property_crime,
         violent_crime) %>%
  group_by(yr) %>%
  summarise_all(sum) %>%
  mutate(violent_crime_rate = violent_crime / population_1 * 100000) %>%
  mutate(property_crime_rate = property_crime / population_1 * 100000) %>%
  select(
    yr,
    population_1,
    property_crime_rate,
    violent_crime_rate,
    property_crime,
    violent_crime
  ) %>%
  mutate(fips_county_code = "Ohio")


# Visualize----

crime_rate <- crime %>%
  filter(fips_state_code == 39) %>%
  filter(fips_county_code %in% c("165", "041", "103")) %>%
  filter(yr >= year-5 & yr < year+1) %>%
  mutate(property_crime = act_burglary_total +
           act_larceny_total + act_mtr_vhc_theft_total) %>%
  mutate(violent_crime =  act_murder + act_manslaughter +
           act_rape_total + act_assault_total +
           act_aggravated_assault) %>%
  select(yr,
         fips_county_code,
         act_rape_total,
         population_1,
         property_crime,
         violent_crime) %>%
  mutate(fips_county_code = 
           str_replace(fips_county_code, "041", "Delaware County")) %>%
  mutate(fips_county_code = 
           str_replace(fips_county_code, "165", "Warren County")) %>%
  mutate(fips_county_code = 
           str_replace(fips_county_code, "103", "Medina County")) %>%
  group_by(fips_county_code, yr) %>%
  summarise_all(sum) %>%
  #filter(year == 2016) %>%
  mutate(violent_crime_rate = violent_crime / population_1 * 100000) %>%
  mutate(property_crime_rate = property_crime / population_1 * 100000) %>%
  select(
    yr,
    population_1,
    property_crime_rate,
    violent_crime_rate,
    property_crime,
    violent_crime,
    fips_county_code
  ) %>%
  union(crime_rate) %>%
  rename(Geography = fips_county_code)



#Table Crimes----

# crime %>%
#   filter(fips_state_code == 39) %>%
#   filter(fips_county_code == 165) %>%
#   mutate(property_crime = act_burglary_total +
#            act_larceny_total + act_mtr_vhc_theft_total) %>%
#   mutate(violent_crime =  act_murder + act_manslaughter +
#            act_rape_total + act_assault_total +
#            act_aggravated_assault) %>%
#   mutate(violent_crime_rate = violent_crime / population_1 * 100000) %>%
#   mutate(property_crime_rate = property_crime / population_1 * 100000) %>%
#   select(
#     yr,
#       property_crime_rate,
#     violent_crime_rate,
#     property_crime,
#     violent_crime
#   ) %>%
#   filter(yr >= year-5 & yr < year+1) %>%
#   group_by(yr) %>%
#   summarise_all(sum) %>%
#   ungroup() %>%
#   gather(key = Crime, value = Count, -yr) %>%
# spread(key = yr, value = Count, convert=TRUE)
#   group_by(Crime) %>%
# mutate(diff = (Count - lag(Count))/lag(Count)*100)

crime_table <- crime_rate %>%
  filter(yr == year) %>%
  select(
    Geography,
    population_1,
    property_crime,
    property_crime_rate,
    violent_crime,
    violent_crime_rate) %>% 
  # rename("Population" = population_1) %>%
  # rename("# of Property Crimes" = property_crime) %>%
  # rename("Property Crime Rate" = property_crime_rate) %>%
  # rename("# of Violent Crimes" = violent_crime) %>%
  # rename("Violent Crime Rate" = violent_crime_rate) %>%
arrange(desc(population_1))


# Graph Crimes----

violent_crime_graph <- crime_rate %>%
  ggplot(aes(x = yr, y = violent_crime_rate, group = Geography)) +
  geom_line(aes(color = Geography), size=1) +
  
  #theme_minimal() +
  #theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  #theme(panel.background = element_blank()) +
  
      labs(title = 'Violent Crime Rates(per 100,000 people)',
           subtitle = paste((year-5), "to", year),
           caption = "Source: FBI Uniform Crime Reporting Program",
           y ="Crime Rate",
           x = "Year")

ggsave(
  plot = violent_crime_graph,
  file = paste(
    "charts/",
    paste(year, county, state, "violent_crime_graph.png", sep = "_"),
    sep = ""
  ),
  width = 6,
  height = 4.5,
  type = "cairo-png"
)

property_crime_graph <- crime_rate %>%
  ggplot(aes(x = yr, y = property_crime_rate, group = Geography)) +
  geom_line(aes(color = Geography), size=1) +
  
  #theme_minimal() +
  #theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  #theme(panel.background = element_blank()) +
  
  labs(title = 'Property Crime Rates(per 100,000 people)',
       subtitle = paste((year-5), "to", year),
       caption = "Source: FBI Uniform Crime Reporting Program",
       y ="Crime Rate",
       x = "Year")

ggsave(
  plot = property_crime_graph,
  file = paste(
    "charts/",
    paste(year, county, state, "property_crime_graph.png", sep = "_"),
    sep = ""
  ),
  width = 6,
  height = 4.5,
  type = "cairo-png"
)



# rename("Murder" = act_murder) %>%
#   rename("Manslaughter" = act_manslaughter) %>%
#   rename("Rape - All" = act_rape_total) %>%
#   rename("Rape - Forcible" = act_force_rape) %>%
#   rename("Rape - Attempted" = act_attempted_rape) %>%
#   rename("Robbery - All" = act_robbery_total) %>%
#   rename("Robbery - Gun" = act_gun_robbery) %>%
#   rename("Robbery - Knife" = act_knife_robbery) %>%
#   rename("Robbery - Other Weapon" = act_other_weapon_robbery) %>%
#   rename("Robbery - Strong Arm" = act_strong_arm_robbery) %>%
#   rename("Assault - All" = act_assault_total) %>%
#   rename("Assault - Gun" = act_gun_assault) %>%
#   rename("Assault - Knife" = act_knife_assault) %>%
#   rename("Assault - Other Weapon" = act_other_weapon_assault) %>%
#   rename("Assault - Hand or Feet" = act_hand_feet_assault) %>%
#   rename("Assault - Simple" = act_simple_assault) %>%
#   rename("Burglary - All" = act_burglary_total) %>%
#   rename("Burglary - Forced Entry" = act_burg_force_entry) %>%
#   rename("Burglary - No Forced Entry" = act_burg_no_force_entry) %>%
#   rename("Burglary - Attempted" = act_attempted_burglary) %>%
#   rename("Larceny" = act_larceny_total) %>%
#   rename("Motor Vehicle Theft - All" = act_mtr_vhc_theft_total) %>%
#   rename("Motor Vehicle Theft - Auto" = act_auto_theft) %>%
#   rename("Motor Vehicle Theft - Truck or Bus" = act_truck_bus_theft) %>%
#   rename("Motor Vehicle Theft - Other" = act_other_vhc_theft) %>%
#   rename("Aggravated Assault" = act_aggravated_assault) %>%