# To see your language locale 
Sys.getlocale() 

# To change it into English 
Sys.setlocale("LC_ALL", "Italian")  


# Ensures the package "pacman" is installed
if (!require("pacman")) {
  install.packages("pacman") }

pacman::p_load(
  
  rio,             # to import datasets
  skimr,           # for displaying your data
  janitor,         # a package for data cleaning
  gtsummary,       # to create frequency tables
  rstatix,         # to generate summary statistics
  epitools,        # for univariable analysis
  epikit,          # for creating age categories
  epiR,            # for the adjusted risk ratios across strata
  ggsurveillance,  # for epidemic curves
  EpiStats,        #load EpiStats package for univariable and stratified analysis
  tidyverse        # data management and visualization
  
)

# Import the dataset 'tira.csv'  
tira_grezzo <- import("data/tiramisu_unisr.txt") 

tira_pul <- tira_grezzo %>% 
  mutate(dateonset = as_date(dateonset))

# Curva epidemica ---------------------------------------------------------

tira_pul %>% 
  ggplot(aes(x  = dateonset)) + 
  geom_epicurve(date_resolution = "day", fill = "darkgreen", colour = "black") + 
  geom_vline(aes(xintercept = as.Date("2025-06-26")), linetype = 2)+
  scale_x_date(labels = scales::label_date_short(), date_breaks = "day")+
  labs(
    y = "Numero di casi",
    x = ""
  ) +
  theme_bw()
  
ggsave("outputs/curva_epi_sessione5.png", width = 8, height = 6)
