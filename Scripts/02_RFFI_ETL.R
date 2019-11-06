# Read Cambodia data from Access database (World Fish)
# Author: Tim Essam, GeoCenter
# Date: 2019_10_30
# Audience: USAID Cambodia Mission and Staff

library(tidytext)

# Files you are pulling from
dir(file.path(datapath, "RFFI_Data"))

access_files <- list.files(file.path(datapath, "RFFI_Data"), pattern = ".xlsx")
access_path <- "Data/RFFI_Data"


fish <- map(as.list(access_files), ~read_excel(file.path(access_path, .)))

names(fish) <- as.list(access_files) %>% set_names()

# Pluck out fish pond category
pond_cat <- fish$`Biological Monitoring_BasicInfo.xlsx` %>% 
  select(CFRName, Category)

# Add in category to the plots and do double facet wrap
gov <- fish$`Governance Scores 2012 & 2015.xlsx` %>% 
  mutate(Overall = rowMeans(.[,3:7])) %>%
  left_join(., pond_cat, by = c(`CFR name` = "CFRName")) %>% 
  gather("gov_type", "value", Structure:Overall) %>% 
  group_by(Year, `CFR name`) %>% 
  mutate(ave_value = mean(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(`CFR name`, gov_type) %>% 
  mutate(lag_value = lag(value, n = 1),
         lead_value = lead(value, n = 1), 
         diff_value_type = value - lag_value, 
         max_value = max(value, na.rm = TRUE)) %>% 
        fill(diff_value_type, .direction = c("up")) %>% 
  fill(lag_value, .direction = c("up")) %>% 
  fill(lead_value) %>% 
  ungroup() %>% 
  group_by(Year, `gov_type`) %>% 
  mutate(ave_type = mean(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(gov_type_sort = fct_reorder(gov_type, value, .desc = TRUE),
         
         # Move overall the the end for the plots
         gov_type_sort = fct_relevel(gov_type_sort, "Overall", after = 5),
         year_color = ifelse(Year == 2012, '#b5b991', '#edbb8a'))

# Plot the gains across the governance types for each CFR
# Need to sort twice
# Function to plot results by diff sort orders
gov_plot <- function(df, sortvar = max_value) {
  df %>%
    mutate(order_CFR = reorder_within(`CFR name`, {{sortvar}}, gov_type)) %>% 
    ggplot(aes(x = value, y = order_CFR, group = Year, fill = year_color)) +
    geom_segment(aes(x = lead_value, xend = lag_value, y = order_CFR, yend = order_CFR),
                 colour = grey30K, size = 0.5) +
    geom_point(aes(fill = year_color), size = 3, shape = 21, colour = "white") + 
    scale_y_reordered() +
    facet_wrap(~gov_type_sort, scales = "free_y") +
    theme_minimal() +
    theme(axis.text = element_text(size = 8),
          panel.grid.major.y = element_blank(),
          strip.text = element_text(hjust = 0, size = 12)) +
    scale_fill_identity() 
}


# Loop over plots by category, saving resulting plots in a grouped / nested dataframe
# extract the nested plots by calling the appropriate position of the nested plot
plots <- 
  gov %>% 
  group_by(Category) %>% 
  nest() %>% 
  mutate(plots = map2(data, Category, 
                      ~gov_plot(.) + labs(x = "", y = "",
                      title = str_c("Category ", Category, ": Governance scores for community fish refuges"),                      
        caption = "Source: 2016 Rice Field Fishery Enhancement Project Database: Governance Scores Module")))
         
plots$plots[2]

map2(file.path(imagepath, paste0("Category ", plots$Category,  
                                 ": Governance scores for community fish refuges.pdf")), 
     plots$plots,
     height = 8.5, 
     width = 11,
     dpi = 300, 
     ggsave)


gov_summary <- gov_plot(gov) + labs(x = "", y = "", 
                     caption = "Source: 2016 Rice Field Fishery Enhancement Project Database: Governance Scores Module")
 ggsave(file.path(imagepath, "Summary governance scores for community fish refuges.pdf"),
         plot = gov_summary,
         height = 8.5,
         width = 11,
         dpi = "retina",
         useDingbats = FALSE,
         scale = 1.2)
  


  
