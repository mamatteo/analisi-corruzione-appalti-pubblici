
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
risultato_indice_1  <- risultato_indice_1  %>% mutate(Punteggio_i1 = if_else(risultato_indice_1$indice_01 >= media_indice_01, 
                                                                             round(rescale(risultato_indice_1$indice_01, to = c(0, 1), 
                                                                             from = range(valori_sospetti, na.rm = TRUE, finite = TRUE)),2),
                                                                             if_else(risultato_indice_1$indice_01 < media_indice_01, 0, NA_real_))) 


#Elimino righe con punteggio = NA
risultato_indice_1 <- risultato_indice_1[!is.na(risultato_indice_1$Punteggio_i1),]

#Salvataggio delle colonne "Codice fiscale stazione appaltante e punteggio 1"
punteggio1 <- select(risultato_indice_1, "CFStazapp", "Punteggio_i1")
write.csv2(punteggio1,'/Users/matteo/Desktop/Analisi dati in R/Risultati finali/punteggio1.csv', row.names=FALSE)
