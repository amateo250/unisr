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

pacman::p_load(  rio, skimr, janitor, lubridate, epikit, gtsummary, tidyverse);

##################
##Step 4.2: fine##
##################

####################
##Step 4.3: inizio##
####################

casi_ebola_grezzo <- import("data/linelist_raw.xlsx");

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
