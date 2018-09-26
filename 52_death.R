# Import----

death_2017 <- read_csv("data/death/deaths2017prelim.csv")
death_2016 <- read_csv("data/death/deaths2016prelim.csv")
# oarrs_data <- read_csv("H:/R/opiods/Data/OARRSData/countyreport_tidy.csv")


# Tidy death data----
death <- union((death_2017 %>% 
                     select(dod_yr, sex, age, countyc, marital, dod_mo, 
                                         dplace, deduc, manner, icdcode113, leadingcause113)), 
                    (death_2016 %>%
                     select(dod_yr, sex, age, countyc, marital, dod_mo, 
                                     dplace, deduc, manner, icdcode113, leadingcause113)))
rm(death_2016, death_2017)

# Explore Data----
#filter by ICD Code 122: Accidental poisoning and 
#exposure to noxious substances

death %>%
  filter(countyc == "165") %>%
  filter(icdcode113 == "122") %>%
  group_by(sex, dod_yr) %>%
  summarise(
    n = n(),
    avg = mean(age),
    st_dev = sd(age),
    min = min(age),
    median = median(age),
    max = max(age)
  )

death %>%
  filter(countyc == "165") %>%
  #filter(icdcode113 == "122") %>%
  ggplot(aes(sex, age), fill=sex, group=sex, color=sex) +
  geom_violin(aes(fill=sex, color=sex),scale="area") +
  
  labs(title = "Demographics of Deaths in Warren County, Ohio",
       y = "Age of Decedent", x = "Sex") +
  theme_hc()

death %>%
  filter(countyc == "165") %>%
  filter(icdcode113 == "122") %>%
  ggplot(aes(sex, age)) +
  geom_violin(aes(fill=sex, color=sex), scale="area") +
    labs(title = "Demographics of Overdose Deaths Impacting Warren County, Ohio",
       y = "Age of Decedent", x = "Sex") +
  theme_hc()

death %>%
  filter(countyc == "165") %>%
  filter(icdcode113 == "122") %>%
  ggplot(aes(age)) +
  geom_dotplot(binwidth=1) +

  labs(title = "Demographics of Overdose Deaths Impacting Warren County, Ohio",
       x = "Age of Decedent") +
  theme_hc()

# YPLL----

codebook113 <- read_excel(
  "H:/R/Opiods/Data/DeathData/May/codebook113.xls", col_types='text')
codebook113

death_join <-
  inner_join(death, codebook113, by=c( "leadingcause113", "icdcode113"))

death_join %>%
  filter(countyc == 165) %>% 
  filter(dod_yr == 2016) %>%
  group_by(cause_leading, icdcode113) %>%
  mutate(ypll = ifelse(age>75, 0,  75-age))%>% 
  summarise(count = n(), ypll = sum(ypll)) %>%
  top_n(10, count) %>%
  arrange(desc(count)) %>%
  group_by(cause_leading) %>%
  summarise(count = sum(count), ypll = sum(ypll)) %>%
  top_n(10, count) %>%
  arrange(desc(count))


death_join %>%
  #filter(countyc == 165) %>% 
  filter(dod_yr == 2016) %>%
  group_by(cause_leading, icdcode113) %>%
  mutate(ypll = ifelse(age>75, 0,  75-age))%>% 
  summarise(count = n(), ypll = sum(ypll)) %>%
  top_n(10, count) %>%
  arrange(desc(count)) %>%
  group_by(cause_leading) %>%
  summarise(count = sum(count), ypll = sum(ypll)) %>%
  top_n(10, count) %>%
  arrange(desc(count))
 
# oarrs <- read_excel("H:/R/opiods/Data/OARRSData/countyreport.xlsx") %>% 
#   select(-"X__28", -"X__29")
# 
# year <- colnames(oarrs) %>%
#   as.tibble() %>%
#   slice(seq(from = 2,to = length(colnames(oarrs)), by=5))
# year <-variables[rep(seq_len(ncol(year)), each=5),]
# 
# county <- oarrs %>% 
#   select(X__1) %>% 
#   slice(seq(2,442, by=5)) %>% 
#   separate(X__1, into = c("County"))
# county <- county[rep(seq_len(nrow(county)), each=3),]
# 
# colnames(oarrs) <- slice(oarrs, 1L) %>% unlist(., use.names = F) %>% as.tibble()
# colnames(oarrs)[1] <- "Variable"
# 
# oarrs %>% select_if(~sum(!is.na(.)) > 0)
