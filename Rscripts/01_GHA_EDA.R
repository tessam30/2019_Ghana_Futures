# Purpose: Investigate trends and make some plots
# Author: Tim Essam, Ph.D
# Date: 2019/06/14
# 

library(ggrepel)
library(RColorBrewer)
library(tidytext)
library(rmarkdown)

# Load data and investigate -----------------------------------------------

theme_line <- theme_xygrid(projector = TRUE) +
  theme(legend.position = "none",
        strip.text = element_text(hjust = 0, size = 12)) 
  
subtitle <- c("Points above grey area indicate levels above the national average")
y_axix_pct <- list( scale_y_continuous(
  labels = scales::percent_format(accuracy = 1)
  # breaks = seq(0, 1, by = 0.25)
))

# Change names to read as character vector not factor for mergabliity later on
gha_sf1_new <- st_read(file.path(datapath, "Admin1", "Admin1.shp"), stringsAsFactors = FALSE)

# Create old Regions to make maps with; Plot below to check how they look
gha_sf1 <- 
  st_read(file.path(datapath, "Admin2", "Admin2.shp"), stringsAsFactors = FALSE) %>% 
  group_by(RGN_NM2012) %>% 
  summarise() %>% 
  st_simplify(dTolerance = 20) %>% 
  rename(Region = RGN_NM2012)
 
gha_sf1 %>% 
  ggplot() + 
  geom_sf(aes(fill = Region, colour = Region))

excel_sheets(file.path(datapath, "2019_Ghana_Indicators.xlsx"))

ghana_data_path <- file.path(datapath, "2019_Ghana_Indicators.xlsx")

gha_df <- 
  excel_sheets(ghana_data_path) %>% 
  set_names() %>% 
  map(read_excel, path = ghana_data_path)
glimpse(gha_df)

gha_df$Stunting <- 
  gha_df$Stunting %>% 
  mutate(Region = str_squish(Region),
         map_label = ifelse(Year == 1993, Region, NA))

# Checking for differences in the names as this will be the merge feature
setdiff(gha_sf1$Region, gha_df$Stunting$Region)

stunting_ntl_ave <- 
  gha_df$Stunting %>% 
  filter(Region == "National")

# Stunting ----------------------------------------------------------------
## @knitr stunting
stunting_plot <- 
  gha_df$Stunting %>% 
  mutate(Region_sort = fct_reorder(Region, Value, .desc = TRUE),
         text = ifelse(Region == "Northern" & Year == 1998, "national average", "")) %>% 
  filter(Region != "National") %>% 
  ggplot(aes(x = Year, y = Value)) +
  geom_area(data = stunting_ntl_ave, aes(x = Year, y = Value), 
            fill = grey10K, size = 1, alpha = 0.85) +
  geom_line(colour = grey40K, size = 0.5) + 
  geom_point(aes(fill = Value), size = 4, colour = grey80K, shape = 21) + 
  #geom_text(aes(y = .25, label = text), vjust = 4, size = 4, colour = grey60K) +
  theme_line +
  facet_wrap(~Region_sort) + 
  scale_x_continuous(breaks = seq(1990, 2020, by = 5)) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1)
    # breaks = seq(0, 1, by = 0.25)
  ) +
  scale_fill_viridis_c(option = "A", direction = -1) +
labs(title = "Northern Region has the highest levels of stunting",
       subtitle = subtitle,
       x = "", y = "",
       caption = "Source: DHS & MICS surveys")
  
stunting_map <- 
  gha_sf1 %>% 
  left_join(gha_df$Stunting, by = c("Region")) %>% 
  filter(Region != "National") %>% 
  mutate(lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
         lat = map_dbl(geometry, ~st_centroid(.x)[[2]])) %>% 
  ggplot() + 
  geom_sf(aes(fill = Value), colour = "white", size = 0.0) + 
  geom_text(aes(
    label = map_label,
    x = lon,
    y = lat
  ),
  color = "#ffffff",
  size = 4) +
  facet_wrap(~Year, nrow = 2) +
  scale_fill_viridis_c(option = "A", direction = -1,
                       labels = scales::percent_format(accuracy = 1)) +
  theme_minimal() +
  theme(legend.position = "top",
        strip.text.x = element_text(hjust = 0),
        panel.background = element_rect(fill = grey20K,
        colour = "#ffffff",
        size = 0, linetype = "solid")) +
  labs(title = "Northern Region has the highest levels of stunting",
       subtitle = "Regions based on pre-2018 polygons",
       caption = "Source: DHS & MICS surveys",
       labels = "Stunting rate")
  
  

# Poverty Region  ------------------------------------------------------------
gha_df$Population %>% print(n = Inf)
gha_df$Population %>% 
  filter(Regions != "Totals country") %>% 
  ggplot(aes(x = year, colour = Regions)) +
  geom_line(aes(y = Male)) +
  geom_line(aes(y = Female)) 



# Poverty by region -------------------------------------------------------
## @knitr poverty

pov_plot <- gha_df$Poverty_region %>% 
  filter(Indicator == "Poverty incidence") %>% 
  mutate(Region_sort = fct_reorder(Region, Value, .desc = TRUE)) %>%
  ggplot(aes(x = Year, group = Region)) +
  geom_area(aes(y = National_ave), fill = grey10K, size = 1, alpha = 0.85) +
  geom_line(aes(y = Value), colour = grey40K, size = 0.5) + 
  facet_wrap(~Region_sort) +
  geom_point(aes(y = Value, fill = Value), size = 4, colour = grey80K, shape = 21) +
  scale_x_continuous(limits = c(2005, 2020)) +
  scale_fill_viridis_c(option = "C", direction = -1) +
  theme_line +
  labs(title = "Upper West region has the highest poverty levels",
       subtitle = subtitle,
       x = "", y = "")

pov_map <- 
  gha_sf1 %>%
  left_join(gha_df$Poverty_region, by = c("Region")) %>% 
  filter(Indicator == "Poverty incidence") %>% 
  ggplot() +
  geom_sf(aes(fill = Value), colour = "white", size = 0.1) + facet_wrap(~Year) +
  scale_fill_viridis_c(option = "C", direction = -1) +
  labs(title = "Upper West region has the highest poverty levels",
       subtitle = subtitle,
       x = "", y = "",
       fill = "Poverty rate") + 
  theme_minimal()



gini_plot <- 
  gha_df$Poverty_region %>% 
  filter(Indicator == "Gini coefficient") %>% 
  mutate(Region_sort = fct_reorder(Region, Value, .desc = TRUE)) %>%
  ggplot(aes(x = Year, group = Region)) +
  geom_area(aes(y = National_ave), fill = grey10K, size = 1, alpha = 0.85) +
  geom_line(aes(y = Value), colour = grey40K, size = 0.5) + 
  facet_wrap(~Region_sort) +
  geom_point(aes(y = Value, fill = Value), size = 4, colour = grey80K, shape = 21) +
  scale_x_continuous(limits = c(2005, 2020)) +
  scale_fill_viridis_c(option = "C", direction = -1) +
  theme_line +
  labs(title = "Upper West region has the highest gini coefficient levels",
       subtitle = str_c(subtitle, "\n", "A higher coefficient indicates more inequality"),
                        x = "", 
                        y = "",
                        caption = "Source: 2017 Ghana Living Standards Survey")


# Family Planning ---------------------------------------------------------

## @knitr syntax

fertility_plot <- gha_df$Fertility_Region %>% 
  mutate(Region_sort = fct_reorder(Region, `adolescent birth rate`),
         reg_color = ifelse(Region == "National", '#80cdc1', grey30K)) %>% 
  gather("indicator", "value", `adolescent birth rate`:`demand for family planning`) %>% 
  mutate(indicator_sort = fct_reorder(indicator, value, .desc = TRUE),
         region_sort2 = reorder_within(Region, value, indicator)) %>% 
  ggplot(aes(y = value, x = region_sort2, fill = reg_color)) +
  coord_flip() + geom_col() +
  scale_x_reordered() +
  facet_wrap(~indicator_sort, scales = "free") +
  scale_fill_identity() +
  theme_line +
  y_axix_pct + 
  labs(title = "Family planning and birth rates by region",
       subtitle = "Note free scales to accomodate indicator ranges",
       x = "", y = "",
       caption = "2018 Multiple Indicator Cluster Survey (MICS)")


# GDP Sectoral Composition  --------------------------------------------------------------------
## @knitr gdp_share
gdp_shares <- gha_df$`GDP Sectoral_Share` %>% 
  mutate(label = if_else(Year == max(Year), as.character(Sector), NA_character_)) %>% 
  ggplot(aes(x = Year, y = Value, group = Sector, colour = Sector)) +
  geom_line(size = 1.25) + geom_point(size = 2) +
  scale_color_manual(values = c("#a6d854", "#e5c494", "#e78ac3")) +
  theme_line +
  scale_x_continuous(limits = c(2004, 2020)) +
  scale_y_continuous(limits = c(0, 0.75),
                     labels = scales::percent_format(accuracy = 1)) +
  geom_label_repel(aes(label = label),
                     nudge_x = 1,
                     na.rm = TRUE, segment.size = 0) +
  labs(x = "", y = "",
       title = "Services constitute the largest share of overall gross domestic product",
       subtitle = "While the overall share is shrining, agriculture has surged in recent years",
       caption = "Source: Ghana Statistical Service")

gdp_growth <- 
  gha_df$`Gdp Growth` %>% 
    ggplot(aes(x = Year, y = Value, group = Indicator)) +
    geom_line(colour = grey40K, size = 1) + 
    geom_point(aes(y = Value, fill = Value), size = 4, colour = grey80K, shape = 21) +
    facet_wrap(~Indicator) +
    theme_line + 
    scale_x_continuous(limits = c(2004, 2020)) +
    labs(x = "", y = "", 
         title = "GDP growth has been strong in recent years, while inflation is near average",
         subtitle = "High growth is associated with periods of low inflation") +
    scale_fill_viridis_c(option = "D", direction = -1) +
    scale_y_continuous(limits = c(0, 0.25),
                       labels = scales::percent_format(accuracy = 1)) +
    theme(strip.text = element_text(size = 20))
    

# Access to services ------------------------------------------------------

access_plot <- 
  gha_df$Service_Access %>% 
    mutate(Region_sort = fct_reorder(Region, Value)) %>%
    mutate(label = if_else(Year == max(Year) & Region == "Upper East", as.character(Indicator), NA_character_)) %>% 
    ggplot(aes(x = Year, y = Value, group = Indicator, colour = Indicator)) +
    geom_line(size = 1) + 
    geom_point() +
    facet_wrap(~Region_sort) +
    theme_line +
    scale_color_manual(values = c("#7570b3", "#1b9e77")) +
    scale_x_continuous(limits = c(2004, 2020)) +
    scale_y_continuous(limits = c(0, 1),
                       labels = scales::percent_format(accuracy = 1)) +
    geom_label_repel(aes(label = label),
                     na.rm = TRUE, 
                     segment.size = 0,
                     label.size = NA,
                     nudge_y = 0.1,
                     nudge_x = 10,
                     box.padding = 0.80, point.padding = 0.5) +
    labs(x = "", y = "",
         title = "Regions in Northern Ghana continue to lag behind in terms of access to electricty and improved toilets",
         subtitle = "Electricity access has grown faster than improved toilet access",
         caption = "Source: 2017 Ghana Living Standards Survey")
    
    

# ICT Use -----------------------------------------------------------------
gha_df$`ICT USe` %>% 
  mutate(Region_sort = fct_reorder(Region, `Mobile use`)) %>% 
    gather(indicator, value, `Computer use`:`One ICT activity`) %>%  
    mutate(indicator_sort = fct_reorder(indicator, value, .desc = TRUE)) %>% 
    ggplot(aes(x = Region_sort, y = indicator_sort)) +
    geom_tile(aes(fill = value), color = grey10K) +
    geom_text(aes(label = percent(value)), colour = "black") +
    scale_fill_viridis_c(
      direction = -1, alpha = 1,
      option = "D", label = percent_format(accuracy = 2), 
    ) + # format labels in legend
    theme_minimal() +
    #coord_fixed(ratio = 1.5) + # Fix the size of the squares
    facet_wrap(~Sex, ncol = 2) + coord_flip() +
    theme_line

# Dumbell plot may be better here
  library(ggalt)  

  mobile_plot <- 
    gha_df$`ICT USe` %>% 
    gather(indicator, value, `Computer use`:`One ICT activity`) %>%  
    mutate(indicator_sort = fct_reorder(indicator, value, .desc = TRUE)) %>% 
    spread(Sex, value)  %>% 
    filter(indicator == "Mobile use") %>% 
    mutate(Region_sort = fct_reorder(Region, male),
           diff = male - female) %>% 
    ggplot(aes(x = female, xend = male, y = Region_sort)) +
    geom_dumbbell( colour_x="#fc8d62", 
                   colour_xend = "#a6d854", 
                   color = grey40K,
                   size = 2,
                   size_x = 6,
                   size_xend = 6) + facet_wrap(~indicator_sort) +
    theme_line + theme_xygrid(projector = TRUE) +
    scale_x_continuous(limits = c(0, 1),
                       labels = scales::percent_format(accuracy = 1)) +
    labs(x = "", 
         y = "", 
         title = "Men (green) use mobile phones more than women",
         subtitle = "Upper West Region lags behind in terms of women's use (orange = women, green = men)",
         caption = "Source: 2017 Multiple Indicator Cluster Survey (MICS)")
  
    
    
    ict_plot <- 
      gha_df$`ICT USe` %>% 
    gather(indicator, value, `Computer use`:`One ICT activity`) %>%  
    mutate(indicator_sort = fct_reorder(indicator, value, .desc = TRUE)) %>% 
    spread(Sex, value) %>% 
    filter(indicator != "Mobile use") %>% 
    mutate(Region_sort = fct_reorder(Region, male),
           diff = male - female) %>% 
    ggplot(aes(x = female, xend = male, y = Region_sort)) +
    geom_dumbbell(colour_x="#fc8d62", size = 2,
                   colour_xend = "#a6d854", 
                   color = grey40K,
                   size_x = 6,
                   size_xend = 6) + facet_wrap(~indicator_sort) +
    theme_line + theme_xygrid(projector = TRUE) + 
    scale_x_continuous(limits = c(0, 1),
                       labels = scales::percent_format(accuracy = 1)) +
    labs(x = "", 
         y = "", 
         title = "Men (green) use information communication technology more than women",
         subtitle = "The gap is greatest in areas with the heaviest use (orange = women, green = men)",
         caption = "Source: 2017 Multiple Indicator Cluster Survey (MICS)")


# Maps -------------------------------------------------------------

# Compare boundaries to show differences and introduce the repurcussions for new statistics
    colour_count <- length(unique(gha_sf1$Region))
    mycolours <- colorRampPalette(RColorBrewer::brewer.pal(9, "Spectral"))(colour_count)    
    
    gha_map_old <- 
    gha_sf1 %>% 
      mutate(lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
             lat = map_dbl(geometry, ~st_centroid(.x)[[2]])) %>% 
      ggplot() + 
      geom_sf(aes(fill = Region), colour = grey60K, size = 0.1) + 
      geom_text(aes(
        label = Region,
        x = lon,
        y = lat
      )) +
      scale_fill_manual(values = mycolours) +
      theme(legend.position = "none") +
      labs(title = "Ghana Regional Boundaries pre-2018")
    
    colour_count_new <- length(unique(gha_sf1_new$REGION)) 
    mycolours_new <- colorRampPalette(RColorBrewer::brewer.pal(9, "Spectral"))(colour_count_new) 
    
    gha_map_new <- 
    gha_sf1_new %>% 
      mutate(lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
             lat = map_dbl(geometry, ~st_centroid(.x)[[2]]),
             REGION = str_to_title(REGION)) %>% 
      ggplot() + 
      geom_sf(aes(fill = REGION), colour = grey60K, size = 0.1) + 
      geom_text(aes(
        label = REGION,
        x = lon,
        y = lat
      )) +
      scale_fill_manual(values = mycolours_new) +
      theme(legend.position = "none") +
      labs(title = "Ghana Regional Boundaries as of 2018")
    
    
gha_compare <- ggpubr::ggarrange(gha_map_old, gha_map_new, 
                                nrow = 1, align = "h") %>% 
  annotate_figure(., top = text_grob("Ghana gained six new geographic regions in 2018", 
                                     size = 16, hjust = 1))  
  

                             