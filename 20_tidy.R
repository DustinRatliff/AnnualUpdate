# Add county areas to sf
# 
# target_county is the imported tidycensus object; y ="areas.csv" is a csv with rows for NAME, GEOID, and area
# area" is a pre-determined combination of multiple census tracts

makeAreaCSVTemplate <- function(target_county) {
  target_county %>%
    
    mutate(area = NA) %>%
    
    as.tibble() %>%
    select(NAME, GEOID, area) %>%
    write_csv(path = "target_county_area.csv")
  cat(
    "Open up target_county_area.csv in the project directory and the area column to be pre-determined areas based on groupings of census tracts"
  )
}

addAreas <- function(target_county, area) {
  if (is.na(area) == TRUE) {
    target_county %>%
      mutate(area = NA)
  } else{
    left_join(target_county, as.tibble(read_csv(area, col_types = "ccc")), by =
                "GEOID")
  }
}

target_county <- addAreas(target_county, "maps/warren_area.csv")

map_theme <- function() {
  theme(panel.background = element_blank()) +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    ) +
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
}

map_theme_percentage <- function(Percentage, ...) {
  ggplot(data = ..., aes(fill = Percentage, color = Percentage)) +
    geom_sf() +
    coord_sf(crs = 26915) +
    scale_fill_viridis(name = "Percentage") +
    scale_color_viridis(name = "Percentage") +
    theme(panel.background = element_blank()) +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    ) +
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
}

chart_theme <- function() {
  theme(panel.background = element_blank()) +
    theme(axis.title.x = element_blank(),
          axis.ticks.x = element_blank()) +
    theme(#axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank())
}