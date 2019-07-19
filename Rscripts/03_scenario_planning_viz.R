# Scenario Planning Results Visualizations
# Tim Essam, GeoCenter
# 2019_07_14
# Notes:


# load it all up ----------------------------------------------------------

sp_df <- read_excel(file.path(datapath, "GHA_scenario_planning_results.xlsx"))




sp_df <- 
  sp_df %>% 
  group_by(Workshop) %>% 
  mutate(total_score = sum(score),
         share = score / total_score) %>% 
  ungroup() %>% 
  mutate(Workshop = fct_reorder(Workshop, total_score, .desc = TRUE),
         driver = reorder_within(Driver_group, score, Workshop),
         scenario_color = ifelse(Scenario == 1, "#80cdc1", grey30K),
         flag = ifelse(Workshop %in% c("Kumasi", "Accra Civil Society", "Tamale", "Cape Coast", "Accra USAID"), 1, 0)) 


sp_df %>% 
  count(Driver_group, sort = TRUE, wt = score) %>% 
  print(n = 25)

plot_text <- sp_df %>% 
  filter(flag == 1) %>% 
  ggplot(aes(y = score, x = driver)) + #fill = scenario_color)) +
  coord_flip() + geom_col() +
  scale_x_reordered() +
  facet_wrap(~Workshop, scales = "free_y", ncol = 2) +
  theme_minimal() +
  scale_fill_identity() +
  theme(strip.text = element_text(colour = grey90K, hjust = 0, size = 12), 
                                  legend.position = "top") +
  # theme(strip.text = element_text(hjust = 0),
  #       legend.position = "top") +
  labs(title = "Education and Governance were the most popular clusters",
       x = "",
       y = "",
       caption = "Source: Voting results from scenario planning workshops.",
       fill = "Scenario driver")
plot_text
plot_notext
  


ggsave(file.path(imagepath, "GHA_cluster_results_text.png"),
       plot = plot_text,
       device = "png",
       height = 9, 
       width = 16, 
       units = "in",
       scale = 0.5)

 

sp_df %>% 
  ggplot(aes(y = share, x = driver, group = Workshop)) +
  coord_flip() + geom_col() +
  scale_x_reordered() +
  scale_y_continuous(limits = c(0, 0.75),
                     labels = scales::percent_format(accuracy = 1)) +
  facet_wrap(~Workshop, scales = "free_y") +
  theme_minimal() +
  theme(strip.text = element_text(hjust = 0)) +
  labs(title = "Education and Governance were the most popular drivers",
       x = "",
       y = "total votes",
       caption = "Source: Voting results from scenario planning workshops.")



