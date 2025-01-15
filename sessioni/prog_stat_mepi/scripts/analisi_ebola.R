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
  flextable, # per formattare tabelle
  scales,   # per mostrare percentuale nelle tabelle
  gtsummary, # statistiche riassuntive, test e regressioni
  tidyverse # gestione e visualizzazione dati
)


## 4.3. Leggere i dati -----------------------------------------------------


# leggere i dati di Ebola
casi_ebola_grezzo <- import("data/linelist_raw.xlsx")

# 3.1. importare le informazioni degli ospedali
osp_info <- import("data/hosp_info.RDS")

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

#Pulizia dati ospedali
osp_info_pul <- osp_info %>% 
  mutate(hosp_name = recode(hosp_name,
                            "St. Mark's Maternity Hospital (SMMH)" = "SMMH")) %>% 
  filter(!(hosp_name == "Military Hospital" & level == "Primary"))

# SESSIONE 3 --------------------------------------------------------------


# Tabelle -----------------------------------------------------------------

#Step  2 aggregare dati per ospedale
casi_ospedale <- casi_ebola_pul %>% 
  group_by(hospital) %>% #raggruppamento per la variabile hospital 
  summarise(n_casi = n(), 
            media_eta = round(mean(eta_anni, na.rm = TRUE), digits = 1), 
            ult_infez = max(date_infection, na.rm = TRUE),
            n_fem     = sum(gender == "femmine", na.rm = TRUE),
            perc_fem  = round(n_fem/n_casi*100, digits = 1),
            .groups = "drop") #è buona praxis rimuovere raggruppamento alla fine

#per vedere il risultato
casi_ospedale


 

#3.2. pulire

inc_osp <- left_join(casi_ospedale, osp_info_pul, by = c("hospital" = "hosp_name"))

inc_osp_pul <- inc_osp %>%
  filter(!is.na(catchment_pop)) %>% 
  mutate(inc = round(n_casi/catchment_pop*10000, digits = 1)) %>% 
  select(-c(level, n_fem, catchment_pop)) %>% 
  arrange(-inc)

flex_inc_osp <- flextable(inc_osp_pul) %>% 
  autofit() %>% 
  set_header_labels(
    hospital = "Ospedale",
    n_casi = "N. casi",
    media_eta = "Età media dei casi",
    ult_infez = "Data ultima infezione",
    perc_fem = "% femmine tra i casi",
    inc = "Incidenza per 10.000"
    ) %>% 
  align(align = "center", part = "all") %>%
  bold(i = 1, bold = TRUE, part = "header") %>% 
  vline(part = "all", j = 1:6) %>% 
  bg(j = 6, i = ~ inc > 40, part = "body", bg = "red") 
flex_inc_osp

save_as_docx(flex_inc_osp, path =  "outputs/table_inc_osp.docx")
