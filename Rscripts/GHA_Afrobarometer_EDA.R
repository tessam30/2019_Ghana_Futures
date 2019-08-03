# Read in and explore afrobarometer data


pacman::p_load("haven", "janitor", "sjlabelled", "expss", "ipumsr", "vtable")

df <- read_spss(file.path(datapath, "Afrobarometer_2016.sav"))

# Issue, when subsetting the data labels get lost.

gh <- df[which(df$COUNTRY == 11), ]
gh$REGION <- lbl_clean(gh$REGION)

# Save clean copy of variables and label
df_lab <- tibble::enframe(sjlabelled::get_label(gh))

# Look for key terms
df_lab %>% filter(str_detect(df_lab$value, "ag"))

cro_cpct(gh$REGION, gh$Q98A) 
cro(gh$Q21)
Hmisc::describe(gh)




barplot(
  table(as_label(gh$REGION),
        as_label(gh$Q81A)), 
  beside = T, 
  legend.text = T
)
