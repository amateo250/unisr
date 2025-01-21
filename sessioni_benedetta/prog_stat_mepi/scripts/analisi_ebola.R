
#Sessione 1 - Lettura e descrizione dati ----------------------------------------------

####################
##Step 4.1: inizio##
####################

#Nuovo script creato

##################
##Step 4.1: fine##
##################

####################
##Step 4.2: inizio##
####################

pacman::p_load(rio, skimr, janitor, lubridate, epikit, gtsummary, tidyverse)

##################
##Step 4.2: fine##
##################

####################
##Step 4.3: inizio##
####################

casi_ebola_grezzo <- import("data/linelist_raw.xlsx")

##################
##Step 4.3: fine##
##################

####################
##Step 4.4: inizio##
####################

skim(casi_ebola_grezzo)

##################
##Step 4.4: fine##
##################

# Sessione 2 - Pulizia dei dati --------------------------------------------------------

#Step1

casi_ebola_pul <- casi_ebola_grezzo

colnames(casi_ebola_pul)

casi_ebola_pul <- casi_ebola_grezzo %>%
  
  #Step 2: rinominare colonne
  
  clean_names() %>%
  rename(date_hosp = hosp_date, date_infection = infection_date) %>%
  
  #Step 3: selezionare colonne
  
  select(-c("merged_header", "x28")) %>%
  select(case_id, starts_with("date_"), everything()) %>%
  
  #Step 4: creare e modificare colonne
  
  mutate(malattia = "Ebola") %>%
  mutate(age = as.numeric(age)) %>%
  mutate(
    gender = recode(gender, "m" = "f", "f" = "m"),
    hospital = recode(
      hospital,
      "Central Hopital" = "Central Hospital",
      "Mitylira Hospital" = "Military Hospital",
      "Mitylira Hopital" = "Military Hospital",
      "Military Hopital" = "Military Hospital",
      "Port Hopital" = "Port Hospital",
      "St. Marks Maternity Hopital (SMMH)" = "SMMH",
      "St. Mark's Maternity Hospital (SMMH)" = "SMMH"
    )
  ) %>%
  mutate(
    stato_caso = case_when(
      ct_blood < 20 ~ "confermato",
      ct_blood >= 20 &
        fever == "yes" & !is.na(source) ~ "sospetto",
      .default = "sotto_indagine"
    ),
    eta_anni = case_when(age_unit == "months" ~ age / 12, .default = age)
  ) %>%
  
  #Step 5: filtrare il database
  
  filter(
    !is.na(case_id)
    & stato_caso == "confermato"
    &
      date_infection >= as.POSIXct("2014-01-01", format = "%Y-%m-%d")
  )
