#Pulizia ambiente di lavoro
rm(list = ls(all.names = TRUE)) # Pulizia degli oggetti creati 
gc() # Pulizia della memoria RAM

#Librerie
library(dplyr)
library(tidyr)
library(tibble)
library(tidyverse)
library(scales)

#Importiamo i dati
path_tab_staz_appaltanti = "/Users/matteo/Desktop/Analisi dati in R/02_Analisi esplorativa dei dati/Tabelle di input/tab_staz_appaltanti.csv"
path_tab_gare = "/Users/matteo/Desktop/Analisi dati in R/02_Analisi esplorativa dei dati/Tabelle di input/tab_gare.csv"
path_tab_ipa = "/Users/matteo/Desktop/Analisi dati in R/02_Analisi esplorativa dei dati/dataset//ipa.txt"

#Creazione delle tabelle
tab_staz_appaltanti <- read.csv(file = path_tab_staz_appaltanti, header = TRUE, sep = ";", na.strings = "", stringsAsFactors=FALSE)
tab_gare <- read.csv(file = path_tab_gare, header = TRUE, sep = ";", na.strings = "", stringsAsFactors=FALSE)
tab_ipa <- read.delim(file = path_tab_ipa, stringsAsFactors=FALSE)
tab_ipa <- select(tab_ipa, des_amm, Cf, Indirizzo, Cap, Comune, Provincia, Regione, titolo_resp, nome_resp, cogn_resp)
tab_ipa <- tab_ipa %>% rename(CFStazapp = Cf)
tab_ipa <- tab_ipa[!duplicated(tab_ipa$CFStazapp), ] # Eliminio CF duplicati
tab_gare <- read.csv(file = path_tab_gare, header = TRUE, sep = ";", na.strings = "", stringsAsFactors=FALSE)


#Arricchimento della tabella con i dati iPA
tab_staz_appaltanti <- left_join(tab_staz_appaltanti, tab_ipa)

#Pulizia della tabella relativa alle stazioni appaltanti
tab_staz_appaltanti <- tab_staz_appaltanti %>% rename(Descrizione_stazioneapp = des_amm)
tab_staz_appaltanti <-subset(tab_staz_appaltanti,tab_staz_appaltanti$CFStazapp!="NULL")
names(tab_staz_appaltanti)
tab_staz_appaltanti <- tab_staz_appaltanti[!duplicated(tab_staz_appaltanti$CFStazapp),]

#Join tra la tabella relativa alle gare e le stazioni appaltanti
appalti <- left_join(tab_gare, tab_staz_appaltanti)

# ! Assunzione da tenere a mente: Selezione delle gare con esito = "aggiudicata"
#Volendo, è possibile rilassare questo vincolo
appalti_aggiudicati <- subset(appalti, appalti$Esito == "Aggiudicata")
