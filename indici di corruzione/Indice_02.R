#CODICE PER IL CALCOLO DELL'INDICE 02 e successive analisi

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

#Creiamo le tabelle
tab_staz_appaltanti <- read.csv(file = path_tab_staz_appaltanti, header = TRUE, sep = ";", na.strings = "", stringsAsFactors=FALSE)
tab_gare <- read.csv(file = path_tab_gare, header = TRUE, sep = ";", na.strings = "", stringsAsFactors=FALSE)

tab_ipa <- read.delim(file = path_tab_ipa, stringsAsFactors=FALSE)
tab_ipa <- select(tab_ipa, des_amm, Cf, Indirizzo, Cap, Comune, Provincia, Regione, titolo_resp, nome_resp, cogn_resp)
tab_ipa <- tab_ipa %>% rename(CFStazapp = Cf)
tab_ipa <- tab_ipa[!duplicated(tab_ipa$CFStazapp), ] # Eliminio CF duplicati

tab_gare <- read.csv(file = path_tab_gare, header = TRUE, sep = ";", na.strings = "", stringsAsFactors=FALSE)

tab_aggiudicatari <- read.csv(file = path_tab_aggiudicatari, header = TRUE, sep = ";", na.strings = "", stringsAsFactors=FALSE)

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

#Conversione del type value di "Importo di aggiudicazione"
appalti_aggiudicati$ImportoDiAggiudicazione <- as.numeric (gsub (",", ".", as.character(appalti_aggiudicati$ImportoDiAggiudicazione)))

#Smistamento delle tipologie di scelta contraente in due macro-categorie
affidamenti_diretti <- c("Affidamento diretto a societa' in house",
                         "Affidamento diretto a società in house",
                         "Affidamento diretto a societa in house",
                         "Affidamento diretto a societa raggruppate/consorziate o controllate nelle concessioni di LL.PP", 
                         "Affidamento diretto a societa' raggruppate/consorziate o controllate nelle concessioni di LL.PP.",
                         "Affidamento diretto a società raggruppate/consorziate o controllate nelle concessioni di LL.PP.",
                         "Affidamento diretto ex art. 5 della legge n.381/91", 
                         "Affidamento diretto in adesione ad accordo quadro/convenzione",
                         "Affidamento in economia – Affidamento diretto",
                         "Affidamento in economia – Cottimo fiduciario",
                         "Affidamento in economia – cottimo fiduciario",
                         "Accordo quadro",
                         "Procedura negoziata senza previa pubblicazione")

procedure_competitive <- c("Procedura aperta", 
                           "Confronto competitivo in adesione ad accordo quadro/convenzione", 
                           "Dialogo Competitivo",
                           "Dialogo competitivo",
                           "Procedura ai sensi dei regolamenti degli organi costituzionali", 
                           "Procedura negoziata derivante da avvisi con cui si indice la gara",
                           "Procedura negoziata previa pubblicazione",
                           "Procedura negoziata senza previa indizione di gara art.221 D.Lgs. 163/2006",
                           "Procedura negoziata senza previa indizione di gara (ex art 221 DLgs 163)",
                           "Procedura ristretta",
                           "Procedura ristretta semplificata",
                           "Procedura ristretta derivante da avvisi con cui si indice una gara",
                           "Procedura selettiva ex art. 238 c.7, D.Lgs. 163/2006", 
                           "Sistema dinamico di acquisizione")

appalti_aggiudicati <- appalti_aggiudicati  %>% mutate(CategoriaProcedura = ifelse(appalti_aggiudicati$TipoSceltaContraente %in% affidamenti_diretti, "Affidamento diretto", 
                                                 ifelse(appalti_aggiudicati$TipoSceltaContraente %in% procedure_competitive, "Procedura competitiva", "Nessuna categoria")))

# ! Assunzione da tenere a mente: Selezione delle gare con TipoSceltaContraente != da "Nessuna categoria"
#Volendo, è possibile rilassare questo vincolo
appalti_aggiudicati <- subset(appalti_aggiudicati, appalti_aggiudicati$CategoriaProcedura != "Nessuna categoria")

g <- appalti_aggiudicati

#Calcolo dell'indice
risultato_indice_2 <- g %>%
  group_by(CFStazapp) %>%
  summarise(numero_totale_gare = n(),
            importo_totale = sum(ImportoDiAggiudicazione),
            nr_gare_chiuse = sum(CategoriaProcedura == "Affidamento diretto"),
            importo_totale_gare_chiuse = sum(ImportoDiAggiudicazione[CategoriaProcedura == "Affidamento diretto"]),
            nr_gare_aperte = sum(CategoriaProcedura == "Procedura competitiva"),
            importo_totale_gare_aperte = sum(ImportoDiAggiudicazione[CategoriaProcedura == "Procedura competitiva"]),
            indice_02 = ((nr_gare_chiuse/numero_totale_gare)*nr_gare_chiuse), #Calcolo indice per numero gare pesando il numero di gare
            )

#FUNZIONE PUNTEGGIO
#Aggiungo punteggio (red flags)
risultato_indice_2  <- risultato_indice_2  %>% mutate(p_i2_15 = round(rescale(risultato_indice_2$indice_02, to = c(0,1)),2))

#######################

##Rescaling punteggio 1 in rapporto agli outliers
#Calcolo media
media_p2_2015 <- mean(na.omit(risultato_indice_2$p_i2_15))
sd_p2_2015 <- sd(na.omit(risultato_indice_2$p_i2_15))
somma_media_sd_p2_2015 <- media_p2_2015 + sd_p2_2015

#Creazione del vettore valori sospetti
outliers_p2_2015 <- risultato_indice_2[(risultato_indice_2$p_i2_15 >= somma_media_sd_p2_2015),]
valori_sospetti <- outliers_p2_2015$p_i2_15[!is.na(outliers_p2_2015$p_i2_15)]

##Aggiungo punteggio a "Corruption Indicator Score" per outliers
risultato_indice_2  <- risultato_indice_2  %>% mutate(p_i2_15_out = if_else(risultato_indice_2$p_i2_15 >= somma_media_sd_p2_2015, 
                                                                            round(rescale(risultato_indice_2$p_i2_15, to = c(0, 1), 
                                                                                          from = range(valori_sospetti, na.rm = TRUE, finite = TRUE)),2),
                                                                            if_else(risultato_indice_2$p_i2_15 < somma_media_sd_p2_2015, 0, NA_real_)))


####################### 

#Elimino righe con punteggio = NA
risultato_indice_2 <- risultato_indice_2[!is.na(risultato_indice_2$p_i2_15),]


#Salvataggio delle colonne "Codice fiscale stazione appaltante e punteggio 2"
punteggio2 <- select(risultato_indice_2, "CFStazapp", "p_i2_15", "p_i2_15_out")
#! Una volta impostato, non serve cambiare path se si cambiano i dati di input
write.csv2(punteggio2,'/Users/matteo/Desktop/Analisi dati in R 2015/Risultati finali/punteggio2_2015.csv', row.names=FALSE)


#Fine del calcolo dell'indicatore 02

########################
#Analisi sull'indice 02
########################

#Statistiche dell'indice 02
summary(risultato_indice_2$indice_02)

#Calcolo media
media_indice_02 <- mean(na.omit(risultato_indice_2$p_i2_15))
round(media_indice_02,4)

#Calcolo deviazione standard
sd_indice_02 <- sd(na.omit(risultato_indice_2$p_i2_15))
round(sd_indice_02,2)

#Calcolo massimo
max_indice_02 <- max(na.omit(risultato_indice_2$p_i2_15)) 
round(max_indice_02)

#Calcolo minimo
min_indice_02 <- min(na.omit(risultato_indice_2$p_i2_15)) 
round(min_indice_02)

###Distribuzione indice

#Primo plot: evidenza del punteggio 2
c <- risultato_indice_2$p_i2_15

#Raw histogram
hist(c, breaks = 100, col="firebrick1", border="white", 
     xlab= "Valore dell'indice", 
     ylab = "Frequenza", 
     main = "Istogramma della distribuzione dell'indice 02") 

#Provare a giocare con diversi valori di xlim=c(1,5), ylim=c(0,1000) e diversi breaks
#Zoom_1
hist(c, breaks = 10000, col="darkorange", border="white", xlim=c(0.1,0.2),
     xlab= "Valore dell'indice", 
     ylab = "Frequenza", 
     main = "Istogramma della distribuzione dell'indice 02")

#Zoom_2
hist(c, breaks = 100, col="tomato1", border="white", xlim=c(0.01,0.2), ylim=c(0,100), 
     xlab= "Valore dell'indice", 
     ylab = "Frequenza", 
     main = "Istogramma della distribuzione dell'indice 02")
