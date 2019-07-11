# Process subnational Human Development Indicators for Ghana
# By: Tim Essam
# Date: July 8, 2019
# 


# Load data  --------------------------------------------------------------

hdi_wide <- read_csv(file.path(datapath, "GDL-Sub-national-HDI-data.csv"))
hdi <- 
  hdi_wide %>% 
  gather(key = Year, value = hdi, `1990`:`2017`)

hdi_sf <- 
  gha_sf1 %>% 
  left_join(., hdi, by = c("Region"))


hdi_sf %>% 
  ggplot() +
  geom_sf(aes(fill = hdi)) +
  facet_wrap(~Year) +
    scale_fill_viridis_c(alpha = 0.85, direction = -1, option = "D",
                         labels = scales::percent_format(accuracy = 1))
  

hdi_sf %>% 
  group_by(Region, Year) %>% 
  mutate(hdi_ave = mean(hdi, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(Region = fct_reorder(Region, hdi_ave)) %>% 
  ggplot(aes(x = Year, y = hdi, group = Region)) +
  geom_line() +
  facet_wrap(~Region)
  