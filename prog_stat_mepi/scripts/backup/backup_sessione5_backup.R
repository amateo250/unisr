## Programmi Statistici - MEPI
## Autore: Alberto Mateo Urdiales
## Obiettivo: Indagine epidemiologica su focolaio di gastroenterita UNISR
## Contatto: alberto.mateourdiales@iss.it
## Data ultimo aggiornamento: 21 marzo 2025

pacman::p_load(
  
  rio,             # per importare i dataset
  skimr,           # per esplorare i dati
  janitor,         # un pacchetto per la pulizia dei dati
  gtsummary,       # per creare tabelle di frequenza
  ggsurveillance,  # per la curva epidemica
  rstatix,         # per generare statistiche riassuntive
  epitools,        # per l'analisi univariata
  epikit,          # per creare categorie di età
  epiR,            # per i rapporti di rischio aggiustati tra le strati
  EpiStats,        # per l'analisi univariata e stratificata
  tidyverse        # gestione dei dati e visualizzazione
)

# 2.1 Leggere i dati ------------------------------------------------------


# Importare il dataset   
tira_grezzo <- import("data/tiramisu_unisr.txt") 


# 2.2. Esplorare i dati  --------------------------------------------------

# Ispezionare il dataset e generare statistiche di base usando la funzione skim  
skim(tira_grezzo)
# Glimpse è un'alternativa a skim che fornisce informazioni più accurate sulla natura delle variabili
glimpse(tira_grezzo)
# Per vedere le diverse categorie delle variabili
tabyl(tira_grezzo, tomato)


# 2.3. Esplorare 'age' in modo più dettagliato  ---------------------------

## Generare statistiche riassuntive 
summary(tira_grezzo$age)

### Creare un boxplot dell'età per stato di salute
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



# Step 3. Pulizia dei dati -----------------------------------

tira_pul <- tira_grezzo %>% # tira_pul sarà il nostro dataframe pulito dopo aver eseguito la pulizia su tira_grezzo
  
  
  ####3.1 Categorizzare i valori "mancanti" come NA 
  
  # questo cambierebbe una per una le colonne in variabili numeriche
  mutate(salmon = case_match(salmon, 
                             9 ~ NA_integer_,
                             .default = salmon)) %>% 
  
  # possiamo fare tutte le colonne contemporaneamente con across(). 
  mutate(across(.cols = c(salmon, horseradish, pork), 
                .fns = ~ case_match(.x, 
                                    9 ~ NA_integer_,
                                    .default = .x))) %>% 
  
  ####3.2 questo cambierebbe tutte le colonne di interesse in 'variabili character' con un solo comando
  mutate(across(.cols = !c(uniquekey, dateonset, age), # con il punto esclamativo prima di c() stiamo dicendo a R che vogliamo tutte le colonne, eccetto quelle nella lista c(), trasformate in character
                .fns = ~ as.character(.x)))  %>% 
  
  ####3.3 semplice ricodifica logica con if_else()
  mutate(tportion2 = if_else(tportion == "2" | tportion == "3", "2+", tportion))%>% 
  mutate(mportion2 = if_else(mportion == "2" | mportion == "3", "2+", mportion)) %>% 
  
  ####3.4. creare fasce d'età
  mutate(fascia_eta = age_categories(age,           # la colonna da usare per creare le categorie 
                                     lower = 0,     # il valore inferiore
                                     upper = 20,    # il valore superiore (vogliamo un gruppo di 20+)
                                     by = 20))      # poiché vogliamo solo due gruppi, anche questo è 20


# Verificare che le modifiche siano state effettuate con successo
glimpse(tira_pul)

# Verificare che le modifiche siano state effettuate con successo
tabyl(tira_pul, salmon)
tabyl(tira_pul, horseradish)
tabyl(tira_pul, tportion2)
tabyl(tira_pul, pork)
tabyl(tira_pul, fascia_eta)

#### 3.5 Boxplot dell'età in base allo stato di malattia
boxplot_eta <- tira_pul %>% 
  
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

### Salvare il boxplot
#### Un modo statico per salvarlo
ggsave(filename = "outputs/boxplot_eta.png", plot = boxplot_eta, width = 8, height = 6)
#### Un modo dinamico per farlo in modo che contenga la data dell'analisi
ggsave(filename = str_glue("outputs/boxplot_eta_",             # Prima scriviamo l'inizio del nome del file fino alla data 
                           str_replace_all(Sys.Date(), "-", ""),  # Sys.Date() ci dà la data dell'analisi. Str_replace_all() rimuove tutti i trattini 
                           ".png"),                               # Infine aggiungiamo l'estensione che è il tipo di file
       plot = boxplot_eta, width = 8, height = 6)

####3.6. esportare il dataset

export(tira_pul, "outputs/tira_pul.csv", row.names = F)



# Step 4. Analisi descrittiva ---------------------------------------------

#### 4.1. Curva epidemica

tira_pul %>% 
  
  ggplot(mapping = aes(x = dateonset)) + 
  
  geom_epicurve(date_resolution = "day", color = "darkgreen", fill = "lightgreen" ) +
  
  scale_x_date(date_breaks = "day") +      # vogliamo che ogni giorno sia mostrato sull'asse x
  
  labs(
    title = "Epicurve dei casi per data di insorgenza",
    y = "Numero di casi",
    x = ""                                 # lasciamo il titolo dell'asse x vuoto perché abbiamo già detto nel titolo che è la data di insorgenza
  ) +
  
  theme_bw() +                             # tema predefinito
  
  theme(axis.text.x = element_text(angle = 90)) # cambiamo l'angolo dei giorni sull'asse x in modo che sia leggibile. Importante che questo avvenga alla fine

#### 4.2. Analisi per persona

tira_pul %>% 
  
  select(sex, fascia_eta, ill) %>%  # selezioniamo le colonne di nostro interesse
  
  tbl_summary(by = ill, percent = "row") # assegniamo la colonna di raggruppamento e 
                                         #definiamo che vogliamo le percentuali per riga per stimare
                                         #i tassi di attacco per sesso e fascia di età.


# Step 5. Sviluppare e testare dell'ipotesi -------------------------------

#### 5.1. Calcolare i tassi di attacco specifici per alimento e la % di casi esposti

tira_pul %>% 
  
  select(tira, wmousse, dmousse, wmousse, beer, 
         redjelly, fruitsalad, tomato, mince, salmon,
         horseradish, chickenwin, roastbeef, pork, ill) %>% # selezioniamo le colonne di nostro interesse
  
  tbl_summary(by = ill, percent = "row") %>% # assegniamo la colonna di raggruppamento e definiamo che vogliamo le percentuali per riga
  
  add_overall()    # aggiungiamo i conteggi complessivi

#### 5.2. Stimare il rischio relativo delle diverse esposizioni

# Usare CS() da EpiStats per estimare il rischio relativo di wmousse
CS(tira_pul, ill, wmousse)


# Rischio relativo delle altre variabili con EpiStats::CSTable
# Creare un oggetto con tutte le variabili alimentari
variabili_cibo <- c("tira","wmousse",   
                    "dmousse","beer","redjelly","fruitsalad",
                    "tomato","mince","salmon",
                    "horseradish", "chickenwin",
                    "roastbeef","pork")

# Creare una tabella per tutte le variabili alimentari insieme
CSTable(tira_pul, "ill", variabili_cibo)


#### 5.3. Analisi stratificata - Confondimento e modificazione dell'effetto

# Stratificare per tiramisù; esposizione = birra
tira_beer <- csinter(x = tira_pul, 
                     cases = "ill", 
                     exposure = "beer", 
                     by = "tira")

tira_beer


