#CODICE PER LE ANALISI PRELIMINARI SUI DATI

## | SETUP
#Librerie
library(dplyr)
library(tidyr)
library(tibble)
library(tidyverse)
library(naniar)
library(visdat)

#Pulizia ambiente di lavoro
rm(list = ls(all.names = TRUE)) # Pulizia degli oggetti creati 
gc() # Pulizia della memoria RAM


## | IMPORTARE I DATI
#Import dei dati
path_ds_appalti = "/Users/matteo/Desktop/Analisi dati in R/01_Leggere e preparare i dati/dataset/2015/Appalti2015.csv"
path_ds_oggettogare = "/Users/matteo/Desktop/Analisi dati in R/01_Leggere e preparare i dati/dataset/2015/Oggettigare2015.csv" 
path_ds_cigcup = "/Users/matteo/Desktop/Analisi dati in R/01_Leggere e preparare i dati/dataset/2015/CigCup2015.csv"
path_ds_aggiudicatari = "/Users/matteo/Desktop/Analisi dati in R/01_Leggere e preparare i dati/dataset/2015/Aggiudicatari2015.csv"

ds_appalti <- read.csv(file = path_ds_appalti, header = FALSE, sep = ";", na.strings = "", stringsAsFactors=FALSE)
ds_oggettogare <- read.csv(file = path_ds_oggettogare, header = FALSE, sep = ";", na.strings = "", stringsAsFactors=FALSE)
ds_cigcup <- read.csv(file = path_ds_cigcup, header = FALSE, sep = ";", na.strings = "", stringsAsFactors=FALSE)
ds_aggiudicatari <- read.csv(file = path_ds_aggiudicatari, header = FALSE, sep = ";", na.strings = "", stringsAsFactors=FALSE)

#Struttura dei dataframe
str(ds_appalti)
str(ds_oggettogare)
str(ds_cigcup)
str(ds_aggiudicatari)

## | AGGIUNTA HEADER
colnames(ds_appalti) <- c("CFStazapp","NomeStazapp","IDCentroCosto", "NomeCentroCosto", "DataPubblicazione", "DataScadenzaOfferta", "NumeroGara", "Cig", "CIGAccordoQuadro", "CPV", "DescrizioneCPV", "ImportoComplessivoGara", "NrLottoComponenti", "ImportoLotto", "CodiceSceltaContraente", "TipoSceltaContraente", "CodiceModalitaRealizzazione", "ModalitaRealizzazione", "CodicePrincipaleContratto", "OggettoPrincipaleContratto", "LuogoIstat", "LuogoNuts", "FlagEscluso", "MotivoEsclusione", "CodiceEsito", "Esito", "DataAggiudicazioneDefinitiva", "CriterioDiAggiudicazione", "ImportoDiAggiudicazione", "NumeroImpreseOfferenti", "RibassoAggiudicazione", "QeBaseImportoLavori", "QeBaseImportoServizi", "QeBaseImportoForniture", "QeBaseImportoSicurezza", "QeBaseUlterioriOneriNoRibasso", "QeBaseImportoProgettazione", "QeBaseSommeADisposizione", "DataStipulaContratto", "DataInizioEffettiva", "DataTermineContrattuale", "QeFineImportoLavori", "QeFineImportoServizi", "QeFineImportoForniture", "QeFineImportoSicurezza", "QeFineImportoProgettazione", "QeFineSommeADisposizione", "DataEffettivaUltimazione")
colnames(ds_oggettogare) <- c("NumeroGara", "OggettoGara", "Cig", "OggettoLotto")
colnames(ds_cigcup) <- c("Cig", "Cup")
colnames(ds_aggiudicatari) <- c("Cig", "CodiceFiscale", "DenominazioneAggiudicatario", "TipoAggiudicatario", "CodiceRuolo", "Ruolo", "CodiceGruppo", "FlagAggiudicatario")

## | MISSING DATA
vis_expect(ds_appalti, ~.x == "NULL")
vis_expect(ds_cigcup, ~.x == "NULL")
vis_expect(ds_oggettogare, ~.x == "NULL")
vis_expect(ds_aggiudicatari, ~.x == "NULL")

## | TYPE CHECKING
#Type checking su "ds_appalti"
#s_appalti$DataPubblicazione <- as.Date(ds_appalti$DataPubblicazione)
ds_appalti$DataScadenzaOfferta <- as.Date(ds_appalti$DataScadenzaOfferta)
ds_appalti$DataAggiudicazioneDefinitiva <- as.Date(ds_appalti$DataAggiudicazioneDefinitiva)
ds_appalti$DataStipulaContratto <- as.Date(ds_appalti$DataStipulaContratto)
ds_appalti$DataInizioEffettiva <- as.Date(ds_appalti$DataStipulaContratto)
ds_appalti$DataTermineContrattuale <- as.Date(ds_appalti$DataStipulaContratto)
ds_appalti$DataEffettivaUltimazione <- as.Date(ds_appalti$DataEffettivaUltimazione, format = "%y%d%m")

ds_appalti$NrLottoComponenti <- as.integer(ds_appalti$NrLottoComponenti)
ds_appalti$CodiceSceltaContraente <- as.integer(ds_appalti$CodiceSceltaContraente)
ds_appalti$CodiceModalitaRealizzazione <- as.integer(ds_appalti$CodiceModalitaRealizzazione)
ds_appalti$LuogoIstat <- as.integer(ds_appalti$LuogoIstat)
ds_appalti$CodiceEsito <- as.integer(ds_appalti$CodiceEsito)
ds_appalti$NumeroImpreseOfferenti <- as.integer(ds_appalti$NumeroImpreseOfferenti)

ds_appalti$ImportoComplessivoGara <- as.numeric(ds_appalti$ImportoComplessivoGara)
ds_appalti$ImportoDiAggiudicazione <- as.numeric(ds_appalti$ImportoDiAggiudicazione)
ds_appalti$RibassoAggiudicazione <- as.numeric(ds_appalti$RibassoAggiudicazione)
ds_appalti$QeBaseImportoServizi <- as.numeric(ds_appalti$QeBaseImportoServizi)
ds_appalti$QeBaseImportoLavori <- as.numeric(ds_appalti$QeBaseImportoLavori)
ds_appalti$QeBaseImportoForniture <- as.numeric(ds_appalti$QeBaseImportoForniture)
ds_appalti$QeBaseImportoSicurezza <- as.numeric(ds_appalti$QeBaseImportoSicurezza)
ds_appalti$QeBaseSommeADisposizione <- as.numeric(ds_appalti$QeBaseSommeADisposizione)
ds_appalti$QeFineImportoLavori <- as.numeric(ds_appalti$QeFineImportoLavori)
ds_appalti$QeFineImportoServizi <- as.numeric(ds_appalti$QeFineImportoServizi)
ds_appalti$QeFineImportoForniture <- as.numeric(ds_appalti$QeFineImportoForniture)
ds_appalti$QeFineImportoSicurezza <- as.numeric(ds_appalti$QeFineImportoSicurezza)
ds_appalti$QeFineImportoProgettazione <- as.numeric(ds_appalti$QeFineImportoProgettazione)
ds_appalti$QeFineSommeADisposizione <- as.numeric(ds_appalti$QeFineSommeADisposizione)

#Type checking su "ds_oggettogare"
ds_oggettogare$NumeroGara <- as.integer(ds_oggettogare$NumeroGara)

#Controllo che i type value siano corretti per tutte le colonne
glimpse(ds_appalti)
glimpse(ds_oggettogare)
glimpse(ds_cigcup)
glimpse(ds_aggiudicatari)

#Normalizzazione dei campi di testo dei dataset
#ds_appalti
ds_appalti$DescrizioneCPV <- tolower(ds_appalti$DescrizioneCPV)
ds_appalti$OggettoPrincipaleContratto <- tolower(ds_appalti$OggettoPrincipaleContratto)
ds_appalti$NomeStazapp <-  tolower(ds_appalti$NomeStazapp)
ds_appalti$NomeCentroCosto <-  tolower(ds_appalti$NomeCentroCosto)
#ds_aggiudicatari
ds_aggiudicatari$DenominazioneAggiudicatario <- tolower(ds_aggiudicatari$DenominazioneAggiudicatario)
#ds_oggettogare
ds_oggettogare$OggettoGara <- tolower(ds_oggettogare$OggettoGara)

## | GESTIONE ED ELIMINAZIONE "NA"
#Eliminazione NA
# Cancellazione righe che presentano il valore "NA" su tutte le colonne
ds_appalti <- ds_appalti[rowSums(is.na(ds_appalti)) != ncol(ds_appalti), ]
ds_oggettogare <- ds_oggettogare[rowSums(is.na(ds_oggettogare)) != ncol(ds_oggettogare), ]
ds_cigcup <- ds_cigcup[rowSums(is.na(ds_cigcup)) != ncol(ds_cigcup), ]
ds_aggiudicatari <- ds_aggiudicatari[rowSums(is.na(ds_aggiudicatari)) != ncol(ds_aggiudicatari), ]

ds_appalti <- ds_appalti[!is.na(ds_appalti$ImportoDiAggiudicazione),]
ds_appalti$ImportoDiAggiudicazione <- as.numeric(ds_appalti$ImportoDiAggiudicazione)
summary(ds_appalti$ImportoDiAggiudicazione)

## | CREAZIONE TABELLE FINALI
#Controllo sulla futura "tabella stazioni appaltanti"
if(nrow(ds_appalti) == sum(!is.na(ds_appalti$CFStazapp))){ # Il codice fiscale ci deve essere!
  print("Controllo superato. Ogni riga di 'ds_appalti' possiede un codice fiscale")
} else {
  print("Il controllo ha avuto esito negativo. Ci sono righe senza codice fiscale")
  print("Si procede all'eliminazione delle righe senza codice fiscale")
  ds_appalti_cf_ok <- ds_appalti %>% drop_na(ds_appalti$CFStazapp)
}

#Creazione della tabella stazioni appaltanti
tab_staz_appaltanti <- ds_appalti %>% select(CFStazapp, NomeStazapp, IDCentroCosto, NomeCentroCosto)


#Creazione della tabella aggiudicatari
tab_aggiudicatari <- ds_aggiudicatari %>% select(Cig, CodiceFiscale, DenominazioneAggiudicatario, TipoAggiudicatario)

#Creazione della tabella gare
#Verifica di quanti CIG ci sono in ds_appalti
ds = ds_appalti
# Il numero di CIG deve corrispondere al numero di righe del datataset. 
# Non ci devono essere valori mancanti. Nel caso vanno eliminati.
if(nrow(ds) == sum(!is.na(ds$Cig))){
  print("Controllo superato. Ogni riga possiede un CIG")
  print(paste0("Il numero di CIG della tabella di input è pari a ",nrow(ds)))
  ds_appalti_cig_ok = ds
} else {
  print("Il controllo ha avuto esito negativo. Ci sono righe senza CIG")
  print("Si procede all'eliminazione delle righe senza CIG")
  ds_complete <- ds %>% drop_na(Cig)
  print(paste0("Le righe iniziali erano pari a ",nrow(ds)))
  print(paste0("Le righe senza CIG sono pari a ",(nrow(ds)-nrow(ds_complete))))
  Sys.sleep(2)
  cat("\n")
  print("Rimozione delle righe prive di CIG...")
  print("Costruzione del nuovo dataset...")
  ds_appalti_cig_ok = ds_complete
  Sys.sleep(2)
  cat("\n")
  print("Processo terminato!")
  print("Il nuovo dataset contiene solo righe il cui CIG è presente")
  cat("\n")
  print("I dati sono stati salvati nel nuovo dataset 'ds_appalti_cig_ok'")
}

#Verifica di quanti CIG ci sono in ds_oggettogare
ds = ds_oggettogare
# Il numero di CIG deve corrispondere al numero di righe del datataset. 
# Non ci devono essere valori mancanti. Nel caso vanno eliminati.
if(nrow(ds) == sum(!is.na(ds$Cig))){
  print("Controllo superato. Ogni riga possiede un CIG")
  print(paste0("Il numero di CIG della tabella di input è pari a ",nrow(ds)))
  ds_oggettogare_cig_ok = ds
} else {
  print("Il controllo ha avuto esito negativo. Ci sono righe senza CIG")
  print("Si procede all'eliminazione delle righe senza CIG")
  ds_complete <- ds %>% drop_na(Cig)
  print(paste0("Le righe iniziali erano pari a ",nrow(ds)))
  print(paste0("Le righe senza CIG sono pari a ",(nrow(ds)-nrow(ds_complete))))
  Sys.sleep(2)
  cat("\n")
  print("Rimozione delle righe prive di CIG...")
  print("Costruzione del nuovo dataset...")
  ds_oggettogare_cig_ok = ds_complete
  Sys.sleep(2)
  cat("\n")
  print("Processo terminato!")
  print("Il nuovo dataset contiene solo righe il cui CIG è presente")
  cat("\n")
  print("I dati sono stati salvati nel nuovo dataset 'ds_oggettogare_cig_ok'")
}

# Eliminazione dei duplicati del dataset "ds_appalti_cig_ok" ("ds_appalti" privo di CIG mancanti)
appalti_con_cig_unico <- ds_appalti_cig_ok[!duplicated(ds_appalti_cig_ok$Cig), ] 

# Eliminazione dei duplicati del dataset "ds_oggettogare_cig_ok" ("ds_oggettogare" privo di CIG mancanti)
oggettogare_con_cig_unico <- ds_oggettogare_cig_ok[!duplicated(ds_oggettogare_cig_ok$Cig), ] 

# Join per costruire la tabella gare
temptab_gare1 <- appalti_con_cig_unico %>% select(NumeroGara, Cig, CIGAccordoQuadro, CPV, DescrizioneCPV, ImportoComplessivoGara, NrLottoComponenti, ImportoLotto, CodiceSceltaContraente, TipoSceltaContraente, DataPubblicazione, DataScadenzaOfferta, CFStazapp, CodiceEsito, Esito, DataAggiudicazioneDefinitiva, CriterioDiAggiudicazione, ImportoDiAggiudicazione, NumeroImpreseOfferenti, RibassoAggiudicazione, CodiceModalitaRealizzazione, ModalitaRealizzazione)
temptab_gare2 <- oggettogare_con_cig_unico %>% select(Cig, OggettoGara, OggettoLotto)
tab_gare <- inner_join(temptab_gare1, temptab_gare2)

## | SALVATAGGIO TABELLE FINALI
write.csv2(tab_staz_appaltanti,'/Users/matteo/Desktop/Analisi dati in R/02_Analisi esplorativa dei dati/Tabelle di input/tab_staz_appaltanti.csv', row.names=FALSE)
write.csv2(tab_gare,'/Users/matteo/Desktop/Analisi dati in R/02_Analisi esplorativa dei dati/Tabelle di input/tab_gare.csv', row.names=FALSE)
write.csv2(tab_aggiudicatari,'/Users/matteo/Desktop/Analisi dati in R/02_Analisi esplorativa dei dati/Tabelle di input/tab_aggiudicatari.csv', row.names=FALSE)
