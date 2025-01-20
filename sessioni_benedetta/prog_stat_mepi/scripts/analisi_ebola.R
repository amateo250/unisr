##Step 4: inizio##

pacman::p_load(  rio, skimr, janitor, lubridate, epikit, gtsummary, tidyverse);

casi_ebola_grezzo <- import("data/linelist_raw.xlsx");

ncol(casi_ebola_grezzo)
nrow(casi_ebola_grezzo)
sapply(casi_ebola_grezzo, class)
sapply(casi_ebola_grezzo[, c("gender", "hospital", "outcome", "fever", "age")], table(useNA ="always"))
sum(is.na(casi_ebola_grezzo$age_unit))
median(casi_ebola_grezzo$wt_kg, na.rm = TRUE)
round(prop.table(table(casi_ebola_grezzo$gender, useNA ="always")),2)
min(casi_ebola_grezzo$`hosp date`, na.rm = TRUE)
head(casi_ebola_grezzo$`hosp date`)

skim(casi_ebola_grezzo)
tabyl(casi_ebola_grezzo, gender)