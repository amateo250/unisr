## Programmi Statistici - MEPI
## Autore: Alberto Mateo Urdiales
## Obiettivo: Indagine epidemiologica su focolaio di gastroenterita UNISR
## Contatto: alberto.mateourdiales@iss.it
## Data ultimo aggiornamento: 21 marzo 2025

pacman::p_load(
  
  rio,             # to import datasets
  skimr,           # for displaying your data
  janitor,         # a package for data cleaning
  gtsummary,       # to create frequency tables
  rstatix,         # to generate summary statistics
  epitools,        # for univariable analysis
  epikit,          # for creating age categories
  epiR,            # for the adjusted risk ratios across strata
  EpiStats,        #load EpiStats package for univariable and stratified analysis
  tidyverse        # data management and visualization
)

# 2.1 Leggere i dati ------------------------------------------------------


# Import the dataset 'tira.csv'  
tira_grezzo <- import("data/tiramisu_unisr.txt") 


# 2.2. Esplorare i dati  --------------------------------------------------

# Inspect your dataset and generate basic statistics using the skim function  
skim(tira_grezzo)
# Glimpse is an alternative to skim which gives more accurate information about the nature of the  variables
glimpse(tira_grezzo)
# To see the different categories of the variables
tabyl(tira_grezzo, tomato)


# 2.3. Esplorare 'age' in modo più dettagliato  ---------------------------

## Generate summary statistics 
summary(tira_grezzo$age)

### Creating a boxplot of age by illness status
boxplot_eta <- tira_grezzo %>% 
  
  ggplot(mapping = aes(x = ill,
                       y = age,
                       fill = ill)) + 
  
  geom_boxplot() + 
  
  labs(
    title = "Boxplot: Distribuzione dell'età per stato di salute",
    x = "Stato di salute",
    y = "Età",
    fill = "Malattia (0=No;1=Sì)"
  ) +
  
  theme_bw()

boxplot_eta
