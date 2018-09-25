# Map County Areas----

county_areas_map <- target_county %>%
  
  mutate(Population = B01003_001E) %>%
  select(area, Population) %>%
  group_by(area) %>%
  rename("Area" = area) %>%
  
  summarise(Population = sum(Population, na.rm=T)) %>%
  select(Area) %>%
  mutate(
    lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
    lat = map_dbl(geometry, ~st_centroid(.x)[[2]])) %>%
  ggplot(aes(fill=Area, color = Area)) +
  
  geom_sf() +
  coord_sf(crs = 26915) +
  
  scale_fill_tableau() +
  scale_color_tableau() +
  labs(title = "Geographical Areas in Warren County,OH"
  )+
  map_theme() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "right", legend.box = "vertical", legend.title=element_blank())

ggsave(
  plot = county_areas_map,
  file = "maps/county_areas_map.png",
  width = 6,
  height = 4.5,
  type = "cairo-png"
)


# Social and Community Context----
##Population
###Map

over65_map<- target_county %>%
  mutate(Percentage = Older64 / B02001_001E * 100) %>%
  select(area, Population, Percentage) %>%
  map_theme_percentage(Percentage = Percentage) +
  labs(title = "Percentage of Population 65+ by Census Tract",
       subtitle = bquote(.(county) ~ "County," ~ .(state) ~ .(year)),
       caption = bquote("Source:" ~ .(year) ~ "American Community Survey, U.S. Census Bureau"))

ggsave(
  plot = over65_map,
  file = paste(
    "maps/",
    paste(year, county, state, "over65_map.png", sep = "_"),
    sep = ""
  ),
  width = 6,
  height = 4.5,
  type = "cairo-png"
)

population_map  <- target_county %>%
  mutate(Percentage = Population / sum(Population) * 100) %>%
  select(area, Population, Percentage) %>%
  map_theme_percentage(Percentage = Percentage) +
  labs(title = "Percentage of Population by Census Tract",
       subtitle = bquote(.(county) ~ "County," ~ .(state) ~ .(year)),
       caption = bquote("Source:" ~ .(year) ~ "American Community Survey, U.S. Census Bureau"))


ggsave(
  plot = population_map,
  file = paste(
    "maps/",
    paste(year, county, state, "population_map.png", sep = "_"),
    sep = ""
  ),
  width = 6,
  height = 4.5,
  type = "cairo-png"
)

###Table
population_table <- target_county %>%
  as.tibble() %>%
  mutate(Percentage = Population / sum(Population) * 100) %>%
  select(area, Population, Percentage) %>%
  group_by(area) %>%
  rename(Area = area) %>%
  summarise_all(funs(sum))

### Chart
population_chart <- target_county %>%
  as.tibble() %>%
  mutate(Percentage = Population / sum(Population) * 100) %>%
  select(area, Population, Percentage) %>%
  group_by(area) %>%
  rename(Area = area) %>%
  summarise_all(funs(sum)) %>%
  ggplot(aes(x=Area, y=Population)) +
  geom_bar(stat="identity", fill="steelblue") +
  geom_text(aes(label=Population), vjust=1.6, color="white", size=3.5) +
  #theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  chart_theme()

##Race and Ethnicity
###Map
race_map <- target_county %>%
  mutate(OtherRace = (B03002_010E +B03002_008E + B03002_005E + B03002_007E)/B03002_001E) %>%
  select(GEOID,
         NAME,
         White,
         Black,
         Asian,
         OtherRace,
         Hispanic,
         NonHispanic,
         geometry) %>%
  rename("Non-Hispanic, White" = White) %>%
  rename("Non-Hispanic, Black or African American" = Black) %>%
  rename("Non-Hispanic, Asian" = Asian) %>%
  rename("Non-Hispanic, Multiple or Other Race" =OtherRace) %>%
  rename("Hispanic, Any Race" = Hispanic) %>%
  rename("Non-Hispanic, Any Race" = NonHispanic) %>%
  gather(key = race, value = percentage, -GEOID, -NAME, -geometry) %>%
  mutate(race = factor(race,levels = c(
    "Non-Hispanic, Asian",
    "Non-Hispanic, Black or African American",
    "Non-Hispanic, Multiple or Other Race",
    "Non-Hispanic, White",
    "Hispanic, Any Race",
    "Non-Hispanic, Any Race"))) %>%
  
  mutate(percentage = percentage * 100) %>%
  ggplot(aes(fill = percentage, color = percentage)) +
  facet_wrap(~ race) +
  geom_sf() +
  coord_sf(crs = 26915) +
  scale_fill_viridis(name = "Percentage") +
  scale_color_viridis(name = "Percentage") +
  labs(title = "Population Density by Race by Census Tract",
       subtitle = bquote(.(county) ~ "County," ~ .(state) ~ .(year)),
       caption = bquote("Source:" ~ .(year) ~ "American Community Survey, U.S. Census Bureau"))+
  map_theme()

ggsave(
  plot = race_map,
  file = paste(
    "maps/",
    paste(year, county, state, "race_map.png", sep = "_"),
    sep = ""
  ),
  width = 9,
  height = 6,
  type = "cairo-png"
)

###Table
race_table <- target_county %>%
  as.tibble() %>%
  mutate(
    White = B03002_003E,
    Black = B03002_004E,
    AIAN = B03002_005E,
    Asian = B03002_006E,
    NHPI = B03002_007E,
    
    MultipleOrOtherRace = (B03002_010E + B03002_008E),
    Hispanic = B03002_012E
  ) %>%
  select(White, Black, AIAN, Asian, NHPI, MultipleOrOtherRace, Hispanic) %>%
  rename("Non-Hispanic, White" = White) %>%
  rename("Non-Hispanic, Black or African American" = Black) %>%
  rename("Non-Hispanic, Asian" = Asian) %>%
  rename("Non-Hispanic, Multiple or Other Race" = MultipleOrOtherRace) %>%
  rename("Non-Hispanic, Native Hawaiian or Pacific Islander" = NHPI) %>%
  rename("Non-Hispanic, American Indian & Alaskan Native" = AIAN) %>%
  rename("Hispanic, Any Race" = Hispanic) %>%
  gather(key = Race, value = number) %>%
  group_by(Race) %>%
  summarise(Count = sum(number)) %>%
  mutate(Percentage = Count / sum(Count) * 100) %>%
  mutate(Percentage = round(Percentage, digits = 2)) %>%
  mutate(Race = factor(
    Race,
    levels = c(
      "Non-Hispanic, American Indian & Alaskan Native",
      "Non-Hispanic, Asian",
      "Non-Hispanic, Black or African American",
      "Non-Hispanic, Native Hawaiian or Pacific Islander",
      "Non-Hispanic, Multiple or Other Race",
      "Non-Hispanic, White",
      "Hispanic, Any Race"
    )
  ))

##Inequality
###Map
gini_map <- target_county %>%
  select(GINI) %>%
  ggplot(aes(fill = GINI, color = GINI)) +
  geom_sf() +
  coord_sf(crs = 26915) +
  scale_fill_viridis(name = "Gini Coefficient") +
  scale_color_viridis(name = "Gini Coefficient") +
  map_theme() +
  labs(title = "Gini Coefficient of Income Inequality by Census Tract",
       subtitle = bquote(.(county) ~ "County," ~ .(state) ~ .(year)),
       caption = bquote("Source:" ~ .(year) ~ "American Community Survey, U.S. Census Bureau"))

ggsave(
  plot = gini_map,
  file = paste(
    "maps/",
    paste(year, county, state, "gini_map.png", sep = "_"),
    sep = ""
  ),
  width = 6,
  height = 4.5,
  type = "cairo-png"
)

###Table
gini_table <- target_county %>%
  as.tibble() %>%
  select(area, GINI) %>%
  group_by(area) %>%
  rename(Area = area) %>%
  summarise(Coefficient = mean(GINI, na.rm=T)) %>%
  mutate(Coefficient = round(Coefficient, digits = 2))

###Chart
gini_chart <-gini_table %>%
  ggplot(aes(x=Area, y=Coefficient)) +
  geom_bar(stat="identity", fill="steelblue") +
  geom_text(aes(label=Coefficient), vjust=1.6, color="white", size=3.5) +
  #theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  chart_theme() +
  
  labs(title = "Gini Coefficient of Income Inequality by County Area",
       subtitle = bquote(.(county) ~ "County," ~ .(state) ~ .(year)))

##Median Age
### Map

median_age_map <- target_county %>%
  select(B01002_001E) %>%
  ggplot(aes(fill = B01002_001E, color = B01002_001E)) +
  geom_sf() +
  coord_sf(crs = 26915) +
  scale_fill_viridis(name = "Median Age") +
  scale_color_viridis(name = "Median Age") +
  map_theme() +
  labs(title = "Median Age by Census Tract",
       subtitle = bquote(.(county) ~ "County," ~ .(state) ~ .(year)),
       caption = bquote("Source:" ~ .(year) ~ "American Community Survey, U.S. Census Bureau"))

ggsave(
  plot = median_age_map,
  file = paste(
    "maps/",
    paste(year, county, state, "median_age_map.png", sep = "_"),
    sep = ""
  ),
  width = 6,
  height = 4.5,
  type = "cairo-png"
)

###Table
median_age_table <- target_county %>%
  as.tibble() %>%
  select(area, B01002_001E, Population) %>%
    group_by(area) %>%
  rename(Area = area) %>%
  summarise("Median Age" = weighted.mean(B01002_001E, Population))

###Chart
median_age_table <- median_age_table %>%
  mutate(`Median Age` = round(`Median Age`, digits =1)) %>%
  ggplot(aes(x=Area, y=`Median Age`)) +
  geom_bar(stat="identity", fill="steelblue") +
  geom_text(aes(label=`Median Age`), vjust=1.6, color="white", size=3.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  chart_theme() +
  
  labs(title = "Median Age by County Area",
       subtitle = bquote(.(county) ~ "County," ~ .(state) ~ .(year)))


# Economic Stability----
##Income
###Map
income_map <- target_county %>%
  
  ggplot(aes(fill = MedianIncome, color = MedianIncome)) +
  geom_sf() +
  coord_sf(crs = 26915) +
  scale_fill_viridis(name = "Median Income") +
  scale_color_viridis(name = "Median Income") +
  labs(title = "Median Income in USD by Census Tract",
       subtitle = bquote(.(county) ~ "County," ~ .(state) ~ .(year)),
       caption = bquote("Source:" ~ .(year) ~ "American Community Survey, U.S. Census Bureau"))+
  map_theme()

ggsave(
  plot = income_map,
  file = paste(
    "maps/",
    paste(year, county, state, "income_map.png", sep = "_"),
    sep = ""
  ),
  width = 6,
  height = 4.5,
  type = "cairo-png"
)

###Table
income_table <- target_county %>%
  as.tibble() %>% 
  select(area, MedianIncome) %>%
  group_by(area) %>%
  summarise(Income = mean(MedianIncome, na.rm=T)) %>%
  mutate(Income = round(Income, digits=0)) %>%
  rename(Area = area)

###Chart
income_chart <- income_table %>%
  ggplot(aes(x=Area, y=Income)) +
  geom_bar(stat="identity", fill="steelblue") +
  geom_text(aes(label=Income), vjust=1.6, color="white", size=3.5) +
  #theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  chart_theme() +
  
  labs(title = "Average Median Income by County Area",
       subtitle = bquote(.(county) ~ "County," ~ .(state) ~ .(year)))

##Poverty
###Map
poverty_map <- target_county %>%
  mutate(Percentage = Poverty/B17001_001E*100) %>%
  map_theme_percentage(Percentage=Percentage) +
  
  labs(title = "Percentage of Population Living in Poverty by Census Tract",
       subtitle = bquote(.(county) ~ "County," ~ .(state) ~ .(year)),
       caption = bquote("Source:" ~ .(year) ~ "American Community Survey, U.S. Census Bureau"))

ggsave(
  plot = poverty_map,
  file = paste(
    "maps/",
    paste(year, county, state, "poverty_map.png", sep = "_"),
    sep = ""
  ),
  width = 6,
  height = 4.5,
  type = "cairo-png"
)

###Table
poverty_table <- target_county %>%
  as.tibble() %>%
  select(area, Poverty, B27001_001E) %>%
  group_by(area) %>%
  rename(Area = area) %>%
  summarise_all(funs(sum)) %>%
  mutate(Percentage = Poverty/B27001_001E*100) %>%
  mutate(Percentage = round(Percentage, digits = 2)) %>%
  rename(Count = Poverty) %>%
  rename(Population = B27001_001E) %>%
  select(Area, Count, Population, Percentage)

###Chart
poverty_chart <- poverty_table %>%
  ggplot(aes(x=Area, y=Percentage)) +
  geom_bar(stat="identity", fill="steelblue") +
  geom_text(aes(label=Percentage), vjust=1.6, color="white", size=3.5) +
  #theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  chart_theme() +
  
  labs(title = "Percentage of Population Living in Poverty by County Area",
       subtitle = bquote(.(county) ~ "County," ~ .(state) ~ .(year)))

  

# Health and Healthcare----
##No Insurance
###Map
no_health_insurance_map <- target_county %>%
  select(
    GEOID,
    NAME,
    NoInsurance,
    B27001_001E
  ) %>%
  mutate(Percentage = (NoInsurance/B27001_001E)*100) %>%
  map_theme_percentage(Percentage = Percentage)+
  labs(title = "Percentage of Individuals with No Insurance by Census Tract",
       subtitle = bquote(.(county) ~ "County," ~ .(state) ~ .(year)),
       caption = bquote("Source:" ~ .(year) ~ "American Community Survey, U.S. Census Bureau"))

ggsave(
  plot = no_health_insurance_map,
  file = paste(
    "maps/",
    paste(year, county, state, "no_health_insurance_map.png", sep = "_"),
    sep = ""
  ),
  width = 6,
  height = 4.5,
  type = "cairo-png"
)

###Table
no_health_insurance_table <- target_county %>%
  as.tibble() %>%
  select(
    area,
    NoInsurance,
    B27001_001E
  ) %>%
  group_by(area) %>%
  rename(Area = area) %>%
  summarise_all(funs(sum)) %>%
  mutate(Percentage = NoInsurance/B27001_001E*100) %>%
  mutate(Percentage = round(Percentage, digits = 2)) %>%
  rename(Count = NoInsurance) %>%
  rename(Population = B27001_001E) %>%
  select(Area, Count, Population, Percentage)

###Chart
no_health_insurance_chart <- no_health_insurance_table %>%
  ggplot(aes(x=Area, y=Percentage)) +
  geom_bar(stat="identity", fill="steelblue") +
  geom_text(aes(label=Percentage), vjust=1.6, color="white", size=3.5) +
  #theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  chart_theme() +
  
  labs(title = "Percentage of Population with No Health Insurance by County Area",
       subtitle = bquote(.(county) ~ "County," ~ .(state) ~ .(year)))


# Education----
##Educational Attainment
###Map
educational_attainment_map <- target_county %>%
  select(
    GEOID,
    NAME,
    LessThanHighSchool,
    HighSchool,
    SomeCollegeNoDegree,
    AssociatesDegree,
    BachelorsDegree,
    GraduateOrProfessional,
    B15003_001E
  ) %>%
  rename("Less Than High School" = LessThanHighSchool) %>%
  rename("Some College, No Degree" = SomeCollegeNoDegree) %>%
  rename("High School or Equivalent" = HighSchool) %>%
  rename("Associate's Degree" = AssociatesDegree) %>%
  rename("Bachelor's Degree" = BachelorsDegree) %>%
  rename("Graduate or Professional Degree" = GraduateOrProfessional) %>%
  gather(key = EducationalAttainment, value = percentage, -GEOID, -NAME, -geometry, -B15003_001E) %>%
  mutate(percentage = percentage/B15003_001E) %>%
  mutate(EducationalAttainment =
           factor(
             EducationalAttainment,
             levels = c(
               "Less Than High School",
               "High School or Equivalent",
               "Some College, No Degree",
               "Associate's Degree",
               "Bachelor's Degree",
               "Graduate or Professional Degree"
             )
           )) %>%
  mutate(percentage = percentage * 100) %>%
  ggplot(aes(fill = percentage, color = percentage)) +
  facet_wrap(~ EducationalAttainment) +
  geom_sf() +
  coord_sf(crs = 26915) +
  scale_fill_viridis(name = "Percentage") +
  scale_color_viridis(name = "Percentage") +
  labs(title = "Highest Level of Educational Attainment by Census Tract",
       subtitle = bquote(.(county) ~ "County," ~ .(state) ~ .(year)),
       caption = bquote("Source:" ~ .(year) ~ "American Community Survey, U.S. Census Bureau")) +
  map_theme()

ggsave(
  plot = educational_attainment_map,
  file = paste(
    "maps/",
    paste(year, county, state, "educational_attainment_map.png", sep = "_"),
    sep = ""
  ),
  width = 9,
  height = 6,
  type = "cairo-png"
)

###Table
educational_attainment_table <- target_county %>%
  as.tibble() %>%
  select(
    LessThanNinthGrade,
    NinthToTwelthNoDiploma,
    HighSchool,
    SomeCollegeNoDegree,
    AssociatesDegree,
    BachelorsDegree,
    MastersDegree,
    ProfessionalSchoolDegree,
    DoctorateDegree
  ) %>%
  rename("Less than 9th Grade" = LessThanNinthGrade) %>%
  rename("9th to 12th Grade, No Diploma" = NinthToTwelthNoDiploma) %>%
  rename("High School or Equivalent" = HighSchool) %>%
  rename("Some College, No Degree" = SomeCollegeNoDegree) %>%
  rename("Associate's Degree" = AssociatesDegree) %>%
  rename("Bachelor's Degree" = BachelorsDegree) %>%
  rename("Master's Degree" = MastersDegree) %>%
  rename("Professional School Degree" = ProfessionalSchoolDegree) %>%
  rename("Doctorate Degree" = DoctorateDegree) %>%
  gather(key = "Educational Attainment", value = number) %>%
  group_by(`Educational Attainment`) %>%
  summarise(Count = sum(number)) %>%
  mutate(Percentage = Count/sum(Count)*100) %>%
  mutate(Percentage = round(Percentage, digits = 2)) %>%
  mutate(`Educational Attainment` =
           factor(
             `Educational Attainment`,
             levels = c(
               "Less than 9th Grade",
               "9th to 12th Grade, No Diploma",
               "High School or Equivalent",
               "Some College, No Degree",
               "Associate's Degree",
               "Bachelor's Degree",
               "Master's Degree",
               "Professional School Degree",
               "Doctorate Degree"
             )
           )) %>%
  arrange(`Educational Attainment`)

###Chart
educational_attainment_chart <- educational_attainment_table %>%
  ggplot(aes(x=`Educational Attainment`, y=Percentage)) +
  geom_bar(stat="identity", fill="steelblue") +
  coord_flip() +
  geom_text(aes(label=Percentage), hjust=-.1, color="black", size=3.5) +
  #theme_minimal() +
  #theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  #chart_theme() +
  theme(panel.background = element_blank()) +
  theme(
    #axis.title.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  theme(
    axis.title.y = element_blank(),
    #axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  labs(title = "Highest Level of Educational Attainment",
       subtitle = bquote(.(county) ~ "County," ~ .(state) ~ .(year)))


# Neighborhood and Built Environment----
##Housing Ownsership
###Map
own_house_map <-target_county %>%
  select(
    GEOID,
    NAME,
    OwnHouse, B25106_001E
  ) %>%
  mutate(Percentage = (OwnHouse/B25106_001E)*100) %>%
  ggplot(aes(fill = Percentage, color = Percentage)) +
  geom_sf() +
  coord_sf(crs = 26915) +
  scale_fill_viridis(name = "Percentage") +
  scale_color_viridis(name = "Percentage") +
  labs(title = "Percentage of Owner-Occupied Housing Units by Census Tract",
       subtitle = bquote(.(county) ~ "County," ~ .(state) ~ .(year)),
       caption = bquote("Source:" ~ .(year) ~ "American Community Survey, U.S. Census Bureau"))+
  map_theme()

ggsave(
  plot = own_house_map,
  file = paste(
    "maps/",
    paste(year, county, state, "own_house_map.png", sep = "_"),
    sep = ""
  ),
  width = 6,
  height = 4.5,
  type = "cairo-png"
)

###Table
own_house_table <- target_county %>%
  as.tibble() %>% 
  select(area, OwnHouse, B25106_001E) %>%
  group_by(area) %>%
  summarise_all(funs(sum)) %>%
  mutate(Percentage = OwnHouse/B25106_001E*100) %>%
  mutate(Percentage = round(Percentage, digits=1)) %>%

  rename(AllHouses = B25106_001E) %>%
  select(area, OwnHouse, AllHouses, Percentage) %>%
  rename(Area = area) 

###Chart
own_house_chart <- own_house_table %>%
  ggplot(aes(x=Area, y=Percentage)) +
  geom_bar(stat="identity", fill="steelblue") +
  geom_text(aes(label=Percentage), vjust=1.6, color="white", size=3.5) +
  #theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  chart_theme() +
  labs(title = "Percentage of Owner-Occupied Housing Units by County Area",
       subtitle = bquote(.(county) ~ "County," ~ .(state) ~ .(year)))

##New Housing
###Map
new_house_map <-target_county %>%
  mutate(Count = (B25034_002E)) %>%
  ggplot(aes(fill = Count, color = Count)) +
  geom_sf() +
  coord_sf(crs = 26915) +
  scale_fill_viridis(name = "Count") +
  scale_color_viridis(name = "Count") +
  labs(title = "Number of New Housing Units by Census Tract",
       subtitle = bquote(.(county) ~ "County," ~ .(state) ~ .(year)),
       caption = bquote("Source:" ~ .(year) ~ "American Community Survey, U.S. Census Bureau"))+
  map_theme()

ggsave(
  plot = new_house_map,
  file = paste(
    "maps/",
    paste(year, county, state, "new_house_map.png", sep = "_"),
    sep = ""
  ),
  width = 6,
  height = 4.5,
  type = "cairo-png"
)

###Table
new_house_table <- target_county %>%
  as.tibble() %>% 
  select(area, B25034_001E, B25034_002E, B25034_003E, B25034_004E) %>%
  mutate(Old = (B25034_001E-(B25034_002E+ B25034_003E+ B25034_004E))) %>%
  select(area, Old, B25034_002E, B25034_003E, B25034_004E) %>%
  rename("Built Before 2000" = Old) %>%
  rename("Built 2000 to 2009" = B25034_004E) %>%
  rename("Built 2010 to 2013" = B25034_003E) %>%
  rename("Built 2014 or Later" = B25034_002E) %>%
  gather(key = Year, value = Count, -area) %>%
  group_by(area, Year) %>%
  summarise(Count = sum(Count)) %>%
  rename(Area = area)


###Chart
new_house_chart <- new_house_table %>%
  filter(Year != "Built Before 2000" & Year != "Built 2000 to 2009") %>%
  
  ggplot(aes(x=Area, y=Count, fill=Year)) +
  geom_bar(stat="identity", position="dodge") +
  #geom_text(aes(label=Count)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  chart_theme() +
  labs(title = "Count of New Housing Units by County Area",
       subtitle = bquote(.(county) ~ "County," ~ .(state) ~ .(year)))