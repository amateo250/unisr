## Programmi Statistici - MEPI
## Autore: Alberto Mateo Urdiales
## Obiettivo: Analisi dei dati di un focolaio di Ebola
## Contatto: alberto.mateourdiales@iss.it
## Data ultimo aggiornamento: 21 gennaio 2025

##4.2

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

##4.3. 

# leggere i dati di Ebola
casi_ebola <- import("data/linelist_raw.xlsx")



#4.4.

# esplorare tutto il database con skimr
skim(casi_ebola)

# esplorare categorie di una variabile "character" o "factor"
tabyl(casi_ebola, gender)
tabyl(casi_ebola, hospital)
tabyl(casi_ebola, fever)

