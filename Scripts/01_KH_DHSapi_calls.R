# Fetch selected DHS indicators for Cambodia
# Author: Tim Essam
# Date: 2019_09_05
# Notes: For Cambodia TDY prep

library(fetchdhs)

# What surveys are available
dhs_list <-  fetch_surveys() %>% 
  filter(country_name %in% c("Cambodia"))


# Select the tags in which you are interested
fetch_tags() %>% print(n = Inf)

# Country list for DHS API: https://api.dhsprogram.com/rest/dhs/countries?f=html
kh <- fetch_data(countries = "KH", years = 2000:2014)

# Better yet, return all indicators with stunting in the definition
fetch_indicators() %>% 
  #filter(str_detect(definition, "stunt")) %>% 
  select(tag_ids, indicator_id, label)


# Returns a list of values
dhs_api <- 
  fetch_data(
    countries = c("KH"),
    # Stunting, Improved toilet, literacy rate for women, and 
    indicators = c("CN_NUTS_C_HA2", "WS_TLET_H_IMP", "ED_LITR_W_LIT"),
    breakdown_level = "subnational"
  )

kh_dhs <- 
  map_dfr(dhs_api, ~as.data.frame(.)) %>% 
  filter(!is.na(data_id)) %>% 
  mutate(REG_ID = region_id)
summary(kh_dhs)

# Plot results to see what is consistent across time / space
kh_dhs %>% filter(survey_year > 2000) %>% 
  ggplot(aes(x = survey_year, y = value, group = characteristic_label)) +
  geom_line() + geom_point() +
  facet_wrap(~characteristic_label) +
  #scale_y_continuous(scales::percent_format()) + 
  theme_minimal()


# For 2000, there are only 17 regions


kh_dhs %>% 
  group_by(characteristic_label, survey_year_label) %>% 
  count() %>% 
  print(n = Inf)

