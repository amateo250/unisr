## Programmi Statistici - MEPI
## Autore: Alberto Mateo Urdiales
## Obiettivo: Analisi dei dati di un focolaio di Ebola
## Contatto: alberto.mateourdiales@iss.it
## Data ultimo aggiornamento: 21 gennaio 2025


# SESSIONE 1 --------------------------------------------------------------

## 4.2. Istallare e caricare i pacchetti necessari -------------------------

# installa (se necessario) da CRAN e carica i pacchetti da utilizzare
pacman::p_load(
  rio, # importazione dati
  skimr, # ottieni panoramica dei dati
  janitor, # pulizia dati e tabelle
  lubridate, # lavorare con le date
  epikit, # per creare categorie di età
  gtsummary, # statistiche riassuntive, test e regressioni
  tidyverse # gestione e visualizzazione dati
)


## 4.3. Leggere i dati -----------------------------------------------------


# leggere i dati di Ebola
casi_ebola_grezzo <- import("data/linelist_raw.xlsx")


## 4.4. Esplorare il database ---------------------------------------------------

# esplorare tutto il database con skimr
skim(casi_ebola_grezzo)

# esplorare categorie di una variabile "character" o "factor"
tabyl(casi_ebola_grezzo, gender)
tabyl(casi_ebola_grezzo, hospital)
tabyl(casi_ebola_grezzo, fever)


# SESSIONE 2 --------------------------------------------------------------


## Pulizia dei dati --------------------------------------------------------
#vederei nomi delle colonne
names(casi_ebola_grezzo)

casi_ebola_pul <- casi_ebola_grezzo %>% 
  
  #rinomina automaticamente tutte le colonne
  clean_names() %>% 
  
  #rinomina manualmente
    rename(
      #NUOVO_NOME = VECCHIO_NOME  
      date_hosp   = hosp_date,
      date_infection = infection_date
  ) %>% 
  
  select(-merged_header, -x28) %>% #rimuove queste colonne
  
  #funzioni come starts_with() o everything() o contains() (non utilizzata qua) 
  #sono funzioni che ci rendono la vita molto più facile
  select(case_id, starts_with("date"), everything()) %>% 
  
  #4.1. creazione di una colonna con un valore: Ebola
  mutate(malattia = "Ebola") %>% 
  
  #4.2. modifichiamo age per convertirla in numerica
  mutate(age = as.numeric(age)) %>% 
  
  #4.2. ricodificare gender
  mutate(gender = recode(gender, 
                         "m" = "maschi",
                         "f" = "femmine")) %>% 
  #4.2. ricodificare hospital
  mutate(hospital = recode(hospital,
    "Central Hopital" = "Central Hospital",
    "Military Hopital" = "Military Hospital",
    "Mitylira Hopital" = "Military Hospital",
    "Mitylira Hospital" = "Military Hospital",
    "Port Hopital"     =  "Port Hospital",
    "St. Mark's Maternity Hospital (SMMH)" = "SMMH",
    "St. Marks Maternity Hopital (SMMH)" = "SMMH"
  )) %>% 
  
  #4.3. ricodifica logica per stato_caso
  mutate(stato_caso = case_when(
    ct_blood < 20                      ~   "confermato",
    !is.na(source) & fever == "yes"    ~   "sospetto",
    TRUE                               ~   "sotto_indagine"
  )) %>% 
  
  #4.3. ricodifica logica di età
  mutate(eta_anni = case_when(
    age_unit == "years"  ~  age,
    is.na(age_unit)      ~  age,
    age_unit == "months" ~  age/12
  )) %>% 
  
  filter(!is.na(case_id) & stato_caso == "confermato" & date_infection >= "2014-01-01")
      
#Verifichiamo che age è cambiata (QUESTO NON E' PARTE DEL COMANDO DI PULIZIA)
class(casi_ebola_grezzo$age)
class(casi_ebola_pul$age)

#Verifichiamo che gender e hospital sono ricodificati correttamente (QUESTO NON E' PARTE DEL COMANDO DI PULIZIA)
tabyl(casi_ebola_pul$gender)
tabyl(casi_ebola_pul$hospital)

#Verifichiamo la nuova variabile stato_caso
tabyl(casi_ebola_pul$stato_caso)

