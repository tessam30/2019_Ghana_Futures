pacman::p_load("tidyverse", "purrr", "llamar", "ggrepel", "extrafont", "extrafontdb")


# Set ggplot themes up
theme_line <- theme_xygrid(projector = FALSE) +
  theme(legend.position = "none",
        strip.text = element_text(hjust = 0, size = 12)) 


olympics <- url("https://raw.githubusercontent.com/UBC-MDS/DSCI_522_OlympicMedalPrediction/master/data/athlete_events.csv")

df <- read_csv(olympics) 

df_sub <- 
  df %>% 
  filter(Year >= 1900 & Season == "Summer") %>% 
  filter(NOC %in% c("GBR", "USA", "URS", "CHN", "RUS")) %>% 
  filter(!is.na(Medal)) %>% 
  mutate(country = case_when(
    NOC %in% c("URS", "RUS") ~ "USSR/Russia",
    NOC == "USA" ~ "USA",
    NOC == "CHN" ~ "China",
    NOC == "GBR" ~ "Great Britain")
  ) %>% 
  group_by(country, Year) %>% 
  summarise(medals = n()) %>% 
  mutate(russia_flag = ifelse(Year < 1950 & country == "USSR/Russia", 0, 1)) %>% 
  filter(russia_flag == 1) %>% 
  arrange(country, Year) %>% 
  mutate(label = if_else(Year == max(Year), as.character(country), NA_character_)) %>% 
  ungroup()


medal_count <- 
  df_sub %>% 
  ggplot(aes(x = Year, y = medals, group = country, colour = country)) + 
  geom_line() +
  geom_point(aes(fill = country), size = 4, colour = "white", shape = 21) +
  theme_line +
  labs(title = "Summer Olympic  Medal Totals", 
       x = "", y = "") +
  geom_text_repel(aes(label = label),
                  nudge_x = 4,
                  na.rm = TRUE, segment.size = 0) +
  scale_x_continuous(limits = c(1900, 2030), breaks = seq(1900, 2030, 10)) +
  scale_color_manual(values = c("#fdb462", "#bebada", "#80b1d3", "#fb8072")) +
  scale_fill_manual(values = c("#fdb462", "#bebada", "#80b1d3", "#fb8072"))


ggsave("~/Documents/USAID/SP_olympic_medal_count.pdf",
       plot = medal_count,
       device = "pdf",
       dpi = "retina",
       height = 2.5, width = 9.5, units = c("in"),
       useDingbats=FALSE)

# Repeat the process for the same four countries using life expectancy at age 10
life_exp_url <- "https://raw.githubusercontent.com/tessam30/2019_Ghana_Futures/master/life_expectancy_age_10.tsv"
life_exp <- read_tsv(life_exp_url)

life_exp_sub <- 
  life_exp %>% 
  filter(Code %in% c("GBR", "USA", "URS", "CHN", "RUS")) %>% 
  mutate(label = if_else(Year == max(Year), as.character(Entity), NA_character_))

life_exp_plot <- 
  life_exp_sub %>% 
  ggplot(aes(x = Year, y = e10, group = Entity, colour = Entity)) + 
  geom_vline(xintercept = 2015, colour = grey20K, size = 1) +
  geom_line() +
  geom_point(aes(fill = Entity), size = 4, colour = "white", shape = 21) +
  theme_line +
  labs(title = "Life Expectancy at Age 10, 1950 - 2095", 
       x = "", y = "") +
  geom_text_repel(aes(label = label),
                  nudge_x = 3,
                  na.rm = TRUE, segment.size = 0) +
  scale_x_continuous(limits = c(1950, 2130), breaks = seq(1950, 2130, 50)) +
  scale_y_continuous(limits = c(45, 90), breaks = seq(0, 90, 15)) + 
  scale_color_manual(values = c("#fdb462", "#fb8072", "#bebada", "#80b1d3")) +
  scale_fill_manual(values = c("#fdb462", "#fb8072", "#bebada", "#80b1d3"))


ggsave("~/Documents/USAID/SP_life_exp_plot.pdf",
       plot = life_exp_plot,
       device = "pdf",
       dpi = "retina",
       height = 2.5, width = 9.5, units = c("in"),
       useDingbats = FALSE)

