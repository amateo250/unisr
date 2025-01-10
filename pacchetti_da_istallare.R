# Pacchetti R essenziali per il corso "Programmi Statistici" del Master II livello in Epidemiologia (MEPI)

# Università Vita-Salute San Raffaele

# ISTRUZIONI:
# 1. Assicurati di avere una buona connessione internet.
# 2. Evidenzia TUTTO il testo e il codice in questo script R (l'intero documento).
# 3. Clicca sul pulsante "Run" nell'angolo in alto a destra di questo pannello.
# 4. R verrà eseguito per un po' di tempo mentre scarica i pacchetti R sul tuo computer.
#    Vedrai l'elaborazione dei download nella "Console" in basso.
#    Se vedi la parola "warning" (avviso), generalmente va bene.
#    Se vedi la parola "error" (errore), c'è qualcosa che non va.
# 5. Dopo che i download sono terminati (riappare il simbolo ">" nella Console),
#    testalo incollando il testo seguente nella Console e premendo Invio:
#    library(tidyverse)
#    Non dovrebbe apparire alcun messaggio di errore.
# 6. Contatta il docente (alberto.mateourdiales@iss.it) se trovi qualsiasi problema.


# Di seguito sono riportati i comandi R che installano automaticamente i pacchetti R

# Assicura che il pacchetto "pacman" sia installato
install.packages("pacman")


# Scarica e installa i pacchetti (se non sono già installati)
#####################################################################
pacman::p_load(
  
  # gestione del progetto e dei file
  #############################
  rio,      # importazione/esportazione di molti tipi di dati
  openxlsx, # funzioni speciali per la gestione dei fogli di lavoro Excel
  
  # installazione e gestione dei pacchetti
  ################################
  remotes,  # installazione da github
  
  # gestione generale dei dati
  #########################
  tidyverse,    # include molti pacchetti per la gestione e presentazione dei dati ordinati
  #stringr,     # incluso in tidyverse
  #lubridate,   # incluso in tidyverse
  skimr,        # esplorazione dei dati
  epikit,       # funzioni utili per l'epidemiologia
  
  # tabelle e statistiche  
  #######################
  janitor,      # tabelle e pulizia dei dati
  gtsummary,    # creazione di tabelle descrittive e statistiche
  
  # grafici - generale
  #################
  #ggplot2,         # incluso in tidyverse
  cowplot,          # combinazione di grafici  
  RColorBrewer,     # scale di colori
  gghighlight,      # evidenzia un sottoinsieme
  ggrepel,          # etichette intelligenti
  ggExtra,          # grafici avanzati  
  tsibble,          # settimane epidemiologiche
  viridis,          # scale di colori adatte ai daltonici
  scales,           # funzioni di supporto
  
  #statistiche
  broom,      # per generare tabelle ordinate di analisi di regressione
  rstatix,    # per statistiche, inclusi test statistici
  ggfortify,  # visualizzazione dei dati per i risultati dell'analisi statistica
  MASS,            # per la regressione binomiale negativa
  epitools,        # per l'analisi univariata
  performance,     # valutazione delle prestazioni dei modelli di regressione
  epiR,            # per i rapporti di rischio aggiustati tra le strati
  
  # rapporti di routine
  #################
  rmarkdown,        # produzione di PDF, documenti Word, PowerPoint e file HTML
  
  
  # tabelle per la presentazione
  #########################
  knitr,            # generazione di rapporti R Markdown e tabelle HTML
  flextable)        # tabelle HTML
