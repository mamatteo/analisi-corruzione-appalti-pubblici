#CODICE PER IL CALCOLO DELL'INDICE 01 e successive analisi

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
# ! Occorre cambiare il path se si cambiano i dati di input
path_tab_staz_appaltanti = "/Users/matteo/Desktop/Analisi dati in R 2015/Output intermedi/Anno 2015/tab_staz_appaltanti.csv"
path_tab_gare = "/Users/matteo/Desktop/Analisi dati in R 2015/Output intermedi/Anno 2015/tab_gare.csv"
path_tab_ipa = "/Users/matteo/Desktop/Analisi dati in R 2015/Output intermedi/Anno 2015/ipa.txt"

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


########################################################################################

#Conversione del type value di "Importo di aggiudicazione"
appalti_aggiudicati$ImportoDiAggiudicazione <- as.numeric (gsub (",", ".", as.character(appalti_aggiudicati$ImportoDiAggiudicazione)))

#Salviamo in un oggetto a parte, le gare il cui criterio di aggiudicazione è uguale a NULL
appalti_aggiudicati_senzacriterio <- subset(appalti_aggiudicati, appalti_aggiudicati$CriterioDiAggiudicazione == "NULL")
#Rimozione degli NA
appalti_aggiudicati_senzacriterio <- subset(appalti_aggiudicati_senzacriterio, appalti_aggiudicati_senzacriterio$ImportoDiAggiudicazione != "NA")
#Calcolo valore delle gare NULL
importo_tot_appalti_aggiudicati_senzacriterio = sum(appalti_aggiudicati_senzacriterio$ImportoDiAggiudicazione)
importo_tot_appalti_aggiudicati_senzacriterio #Valore delle gare senza un criterio di aggiudicazione

#Eliminiamo gli appalti aggiudicati senza un criterio di aggiudicazione
appalti_aggiudicati <- subset(appalti_aggiudicati, appalti_aggiudicati$CriterioDiAggiudicazione != "NULL")

#Calcolo valore delle gare con un criterio di aggiudicazione
importo_tot_appalti_aggiudicati_senzaNULL = sum(appalti_aggiudicati$ImportoDiAggiudicazione)
importo_tot_appalti_aggiudicati_senzaNULL #Valore delle gare con un criterio di aggiudicazione

g <- appalti_aggiudicati

#Calcolo dell'indice
risultato_indice_1 <- g %>%
  group_by(CFStazapp) %>%
  summarise(numero_totale_gare = n(),
            importo_totale = sum(ImportoDiAggiudicazione),
            gare_off_vantaggiosa = sum(CriterioDiAggiudicazione == "Offerta economicamente piu' vantaggiosa"),
            importo_totale_gara_vant = sum(ImportoDiAggiudicazione[CriterioDiAggiudicazione == "Offerta economicamente piu' vantaggiosa"]),
            gare_prezzo_basso = sum(CriterioDiAggiudicazione == "Prezzo piu' basso"),
            importo_totale_prezzobasso = sum(ImportoDiAggiudicazione[CriterioDiAggiudicazione == "Prezzo piu' basso"]),
            indice_01 = round((importo_totale_gara_vant/importo_totale),2), #Calcolo indice per importo
            )

#FUNZIONE PUNTEGGIO

#Calcolo media
media_indice_01 <- mean(na.omit(risultato_indice_1$indice_01))

#Creazione del vettore valori sospetti
valori_sospetti <- risultato_indice_1[(risultato_indice_1$indice_01 >= media_indice_01),]
valori_sospetti <- valori_sospetti$indice_01

#Aggiungo punteggio (red flags)
risultato_indice_1  <- risultato_indice_1  %>% mutate(p_i1_15 = if_else(risultato_indice_1$indice_01 >= media_indice_01, 
                                                                             round(rescale(risultato_indice_1$indice_01, to = c(0, 1), 
                                                                             from = range(valori_sospetti, na.rm = TRUE, finite = TRUE)),2),
                                                                             if_else(risultato_indice_1$indice_01 < media_indice_01, 0, NA_real_))) 



#######################

##Rescaling punteggio 1 in rapporto agli outliers
#Calcolo media
media_p1_2015 <- mean(na.omit(risultato_indice_1$p_i1_15))
sd_p1_2015 <- sd(na.omit(risultato_indice_1$p_i1_15))
somma_media_sd_p1_2015 <- media_p1_2015 + sd_p1_2015

#Creazione del vettore valori sospetti
outliers_p1_2015 <- risultato_indice_1[(risultato_indice_1$p_i1_15 >= somma_media_sd_p1_2015),]
valori_sospetti <- outliers_p1_2015$p_i1_15[!is.na(outliers_p1_2015$p_i1_15)]

##Aggiungo punteggio a "Corruption Indicator Score" per outliers
risultato_indice_1  <- risultato_indice_1  %>% mutate(p_i1_15_out = if_else(risultato_indice_1$p_i1_15 >= somma_media_sd_p1_2015, 
                                                                         round(rescale(risultato_indice_1$p_i1_15, to = c(0, 1), 
                                                                                       from = range(valori_sospetti, na.rm = TRUE, finite = TRUE)),2),
                                                                         if_else(risultato_indice_1$p_i1_15 < somma_media_sd_p1_2015, 0, NA_real_)))


####################### 

#Elimino righe con punteggio = NA
risultato_indice_1 <- risultato_indice_1[!is.na(risultato_indice_1$p_i1_15),]

#Salvataggio delle colonne "Codice fiscale stazione appaltante e punteggio 1"
punteggio1_2015 <- select(risultato_indice_1, "CFStazapp", "p_i1_15", "p_i1_15_out")
#! Una volta impostato, non serve cambiare path se si cambiano i dati di input
write.csv2(punteggio1_2015,'/Users/matteo/Desktop/Analisi dati in R 2015/Risultati finali/punteggio1_2015.csv', row.names=FALSE)

#Fine del calcolo dell'indicatore 01

########################
#Analisi sull'indice 01
########################

#Calcolo totali
totale_gare <- sum(risultato_indice_1$numero_totale_gare) + 0
totale_gare_off_vant <- sum(risultato_indice_1$gare_off_vantaggiosa) + 0
totale_gare_prezzo_basso <- sum(risultato_indice_1$gare_prezzo_basso) + 0

#Statistiche dell'indice 01
summary(risultato_indice_1$indice_01)

#Calcolo deviazione standard
sd(na.omit(risultato_indice_1$indice_01))

#Calcolo massimo
max_indice_01 <- max(na.omit(risultato_indice_1$indice_01))

#Estraiamo le righe con punteggio >= 0.00
stazioni_appaltanti_sospette <- (risultato_indice_1[risultato_indice_1$Punteggio_i1>0,])

#Estraiamo le righe con punteggio == 1
stazioni_appaltanti_a_uno <- (risultato_indice_1[risultato_indice_1$Punteggio_i1==1,])
mediauno <- round(mean(na.omit(stazioni_appaltanti_a_uno$numero_totale_gare)),1)

#Primo plot: evidenza del punteggio 1
c <- stazioni_appaltanti_sospette$Punteggio_i1
hist(c, breaks = 100, col="firebrick1", border="white")

#Rimozione valore 1 e nuovo plot
c <- c[!c %in% 1]
c <- c[!c %in% NA]
hist(c, breaks = 100, col="dodgerblue3", border="white")
