#CODICE PER IL CALCOLO DELL'INDICE 06 e successive analisi

#Pulizia ambiente di lavoro
rm(list = ls(all.names = TRUE)) # Pulizia degli oggetti creati 
gc() # Pulizia della memoria RAM

#Librerie
library(dplyr)
library(tidyr)
library(tibble)
library(tidyverse)
library(scales)
library(ggplot2)

#Importiamo i dati
# ! Occorre cambiare il path se si cambiano i dati di input
path_tab_staz_appaltanti = "/Users/matteo/Desktop/Analisi dati in R 2015/Output intermedi/Anno 2015/tab_staz_appaltanti.csv"
path_tab_gare = "/Users/matteo/Desktop/Analisi dati in R 2015/Output intermedi/Anno 2015/tab_gare.csv"
path_tab_ipa = "/Users/matteo/Desktop/Analisi dati in R 2015/Output intermedi/Anno 2015/ipa.txt"

#Creiamo le tabelle
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
names(appalti_aggiudicati)

#Conversione del type value di "Importo di aggiudicazione", "Ribaso di aggiudicazione" e "NumeroImpreseOfferenti"
appalti_aggiudicati$ImportoDiAggiudicazione <- as.numeric (gsub (",", ".", as.character(appalti_aggiudicati$ImportoDiAggiudicazione)))
appalti_aggiudicati$RibassoAggiudicazione <- as.numeric (gsub (",", ".", as.character(appalti_aggiudicati$RibassoAggiudicazione)))
appalti_aggiudicati$NumeroImpreseOfferenti <- as.integer(appalti_aggiudicati$NumeroImpreseOfferenti)

#Conversione della colonna "Data pubblicazione", "Data Scadenza Offerta" e "Data Aggiudicazione Definitiva" nel formato "date"
appalti_aggiudicati$DataPubblicazione <- as.Date(appalti_aggiudicati$DataPubblicazione)
appalti_aggiudicati$DataScadenzaOfferta <- as.Date(appalti_aggiudicati$DataScadenzaOfferta)
appalti_aggiudicati$DataAggiudicazioneDefinitiva <- as.Date(appalti_aggiudicati$DataAggiudicazioneDefinitiva)

glimpse(appalti_aggiudicati)

#Eliminiamo le righe dove l'importo di aggiudicazione e la data di aggiudicazione definitiva sono uguali a = NA
appalti_aggiudicati <- appalti_aggiudicati[!is.na(appalti_aggiudicati$ImportoDiAggiudicazione),]
appalti_aggiudicati <- appalti_aggiudicati[!is.na(appalti_aggiudicati$DataAggiudicazioneDefinitiva),]

#Aggiunta di una colonna che conta la differenza di date
appalti_aggiudicati <- appalti_aggiudicati  %>% mutate(Giorni_di_differenza = ((as.Date(as.character(DataAggiudicazioneDefinitiva), format="%Y-%m-%d"))
  - (as.Date(as.character(DataPubblicazione), format="%Y-%m-%d"))))

appalti_aggiudicati <- appalti_aggiudicati[appalti_aggiudicati$Giorni_di_differenza>=0,]

g <- appalti_aggiudicati

#Calcolo dell'indice
risultato_indice_6 <- g %>%
  group_by(CFStazapp) %>%
  summarise(numero_totale_gare = n(),
            giorni_totali_di_differenza = sum(as.Date(as.character(DataAggiudicazioneDefinitiva), format="%Y-%m-%d")
                                              -as.Date(as.character(DataPubblicazione), format="%Y-%m-%d")),
            importo_totale = sum(ImportoDiAggiudicazione),
            indice_06 = round((as.double((giorni_totali_di_differenza*importo_totale)))/(as.double((numero_totale_gare+giorni_totali_di_differenza))),3) #Calcolo indice pesato
  )


#FUNZIONE PUNTEGGIO
#Aggiungo punteggio (red flags)
risultato_indice_6 <- risultato_indice_6  %>% mutate(p_i6_15 = round(rescale(risultato_indice_6$indice_06, to = c(0,1)),2))

#######################

##Rescaling punteggio 1 in rapporto agli outliers
#Calcolo media
media_p6_2015 <- mean(na.omit(risultato_indice_6$p_i6_15))
sd_p6_2015 <- sd(na.omit(risultato_indice_6$p_i6_15))
somma_media_sd_p6_2015 <- media_p6_2015 + sd_p6_2015

#Creazione del vettore valori sospetti
outliers_p6_2015 <- risultato_indice_6[(risultato_indice_6$p_i6_15 >= somma_media_sd_p6_2015),]
valori_sospetti <- outliers_p6_2015$p_i6_15[!is.na(outliers_p6_2015$p_i6_15)]

##Aggiungo punteggio a "Corruption Indicator Score" per outliers
risultato_indice_6  <- risultato_indice_6 %>% mutate(p_i6_15_out = if_else(risultato_indice_6$p_i6_15 >= somma_media_sd_p6_2015, 
                                                                           round(rescale(risultato_indice_6$p_i6_15, to = c(0, 1), 
                                                                                         from = range(valori_sospetti, na.rm = TRUE, finite = TRUE)),2),
                                                                           if_else(risultato_indice_6$p_i6_15 < somma_media_sd_p6_2015, 0, NA_real_)))


####################### 

#Elimino righe con punteggio = NA
risultato_indice_6 <- risultato_indice_6[!is.na(risultato_indice_6$p_i6_15),]

#Elimino righe con punteggio = -Inf (differenza date negative)
risultato_indice_6 <- risultato_indice_6[risultato_indice_6$p_i6_15 != "-Inf",]

#Salvataggio delle colonne "Codice fiscale stazione appaltante e punteggio 6"
punteggio6 <- select(risultato_indice_6, "CFStazapp", "p_i6_15", "p_i6_15_out")
#! Una volta impostato, non serve cambiare path se si cambiano i dati di input
write.csv2(punteggio6,'/Users/matteo/Desktop/Analisi dati in R 2015/Risultati finali/punteggio6_2015.csv', row.names=FALSE)
