# installa (se necessario) da CRAN e carica i pacchetti da utilizzare
pacman::p_load(
  rio, # importazione dati
  skimr, # ottieni panoramica dei dati
  janitor, # pulizia dati e tabelle
  lubridate, # lavorare con le date
  epikit, # per creare categorie di età
  flextable, # per formattare tabelle
  scales,   # per mostrare percentuale nelle tabelle
  gtsummary, # statistiche riassuntive, test e regressioni
  tidyverse # gestione e visualizzazione dati
)


## 4.3. Leggere i dati -----------------------------------------------------


# leggere i dati di Ebola
casi_ebola_grezzo <- import("data/linelist_raw.xlsx")

demo <- casi_ebola_grezzo %>% select(case_id, `date onset`, gender, age, fever)

demo <- demo[1:10, ]

demo <- demo %>% mutate(age = as.numeric(age))

#demo <- filter(demo, age < 18 & gender == "f")

demo <- demo %>% filter(age < 18 & gender == "f") %>% select(case_id, gender, age, fever)

demo <- demo %>% 
  mutate(gruppo_eta = case_when(
    age <= 1   ~  "neonato",
    age >  1   ~  "bambino"
  ))

demo <- demo %>% 
  mutate(gender = recode(gender,
                         "m" = "maschio",
                         "f" = "femmina"))
