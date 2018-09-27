# Import----

death_2017 <- read_csv("data/death/deaths2017prelim.csv") %>%
  mutate(leadingcause113 = as.integer(leadingcause113)) %>%
  mutate(icdcode113 = as.integer(icdcode113))
death_2016 <- read_csv("data/death/deaths2016.csv") %>%
  mutate(countyc = ifelse(
    nchar(as.character(countyc)) ==2, paste0("0", countyc), 
    as.character(countyc)))

# Tidy death data----
death <- union((death_2017 %>% 
                     select(dod_yr, sex, age, countyc, marital, dod_mo, 
                                         dplace, deduc, manner, icdcode113, leadingcause113)), 
                    (death_2016 %>%
                     select(dod_yr, sex, age, countyc, marital, dod_mo, 
                                     dplace, deduc, manner, icdcode113, leadingcause113)))
rm(death_2016, death_2017)

# Add data on leading cause of death and specific cause of death

codebook113 <- read_excel("data/death/codebook113.xls")
codebook113

death_join <-
  left_join(death, codebook113, by=c( "leadingcause113", "icdcode113"))

# Explore Data----
#filter by ICD Code 122: Accidental poisoning and 
#exposure to noxious substances

death %>%
  filter(dod_yr == year) %>%
  filter(countyc == "165") %>%
  #filter(icdcode113 == "122") %>%
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
  filter(dod_yr == year) %>%
  #filter(icdcode113 == "122") %>%
  ggplot(aes(sex, age), fill=sex, group=sex, color=sex) +
  geom_violin(aes(fill=sex, color=sex),scale="area") +
  
  labs(title = "Demographics of Deaths in Warren County, Ohio",
       y = "Age of Decedent", x = "Sex") +
  theme_hc()

death %>%
  filter(countyc == "165") %>%
  filter(dod_yr == year) %>%
  filter(icdcode113 == "122") %>%
  ggplot(aes(sex, age)) +
  geom_violin(aes(fill=sex, color=sex), scale="area") +
    labs(title = "Demographics of Overdose Deaths Impacting Warren County, Ohio",
       y = "Age of Decedent", x = "Sex") +
  theme_hc()

death %>%
  filter(countyc == "165") %>%
  filter(dod_yr == year) %>%
  filter(icdcode113 == "122") %>%
  ggplot(aes(age)) +
  geom_dotplot(binwidth=1) +

  labs(title = "Demographics of Overdose Deaths Impacting Warren County, Ohio",
       x = "Age of Decedent") +
  theme_hc()

# YPLL----

death_join %>%
  filter(countyc == 165) %>% 
  filter(dod_yr == 2016) %>%
  group_by(cause_leading) %>%
  mutate(ypll = ifelse(age>75, 0,  75-age))%>% 
  summarise(count = n(), ypll = sum(ypll)) %>%
  filter(cause_leading != "Not a Leading Cause") %>%
  mutate(count_rank = dense_rank(desc(count))) %>%
  mutate(ypll_rank = dense_rank(desc(ypll))) %>%
  arrange(desc(count)) %>%
  top_n(10, count)






