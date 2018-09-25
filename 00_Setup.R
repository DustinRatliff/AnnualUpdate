library(tidyverse)
library(haven)
library(knitr)
library(hrbrthemes)
library(gridExtra)
library(ggExtra)
library(ggthemes)
library(srvyr)
library(flexdashboard)
library(tidycensus)
library(viridis)
library(Cairo)
library(kableExtra)
library(ggrepel)
library(sf)
library(flextable)
library(huxtable)
library(officer)
library(knitr)
library(readxl)
library(tidyxl)

census_api_key("d7f13444facd6127f91279f4004d6bc413ca7218")
options(tigris_use_cache = TRUE)

# population_table %>%
#   regulartable() %>%
#   # set_formatter(Percentage = percent_format) %>%
#   theme_booktabs() %>%
#   autofit()
