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
path_tab_staz_appaltanti = "il-tuo-path-qui/tab_staz_appaltanti.csv"
path_tab_gare = "il-tuo-path-qui/tab_gare.csv"
path_tab_ipa = "il-tuo-path-qui/ipa.txt"

#Creazione delle tabelle
tab_amministrazioni <- read.csv(file = path_tab_amministrazioni, header = TRUE, sep = ";", na.strings = "", stringsAsFactors=FALSE)
names(tab_amministrazioni)
tab_ipa <- read.delim(file = path_tab_ipa, stringsAsFactors=FALSE)
tab_gare <- read.csv(file = path_tab_gare, header = TRUE, sep = ";", na.strings = "", stringsAsFactors=FALSE)
tab_aggiudicatari <- read.csv(file = path_tab_aggiudicatari, header = TRUE, sep = ";", na.strings = "", stringsAsFactors=FALSE)

tab_ipa <- select(tab_ipa, des_amm, Cf, Indirizzo, Cap, Comune, Provincia, Regione, titolo_resp, nome_resp, cogn_resp)
tab_ipa <- unite(tab_ipa, Indirizzo_completo, Indirizzo:Comune, sep = ", ")
tab_ipa <- tab_ipa %>% rename(CFAmmne = Cf)
tab_ipa <- tab_ipa[!duplicated(tab_ipa$CFAmmne), ] # Eliminio CF duplicati


#Attraverso un inner_join si procede ad arricchire la tabella relativa alle amministrazioni
tab_amministrazioni<-subset(tab_amministrazioni,tab_amministrazioni$CFAmmne!="NULL")
tab_amministrazioni_plus <- merge(x=tab_amministrazioni, y=tab_ipa, by="CFAmmne", all.x=TRUE)
names(tab_amministrazioni_plus)

#Si procede all’analisi della distribuzione delle amministrazioni presenti nei dati disponibili.
#Quante pubbliche amministrazioni diverse sono rappresentate dai dati?
temp_tab_amministrazioni_plus <- select(tab_amministrazioni_plus, "CFAmmne", "Regione")
unique_tab_amministrazioni_plus <- temp_tab_amministrazioni_plus[!duplicated(temp_tab_amministrazioni_plus$CFAmmne), ] # Elimino duplicati
nrow(unique_tab_amministrazioni_plus)

#Eliminazione dei campi vuoti ("NA") dalla colonna "Regione"
unique_tab_amministrazioni_plus<-subset(unique_tab_amministrazioni_plus,unique_tab_amministrazioni_plus$Regione!="")

conta_occorrenze_reg <- count(unique_tab_amministrazioni_plus, vars = "Regione")
conta_occorrenze_reg <-conta_occorrenze_reg[order(-conta_occorrenze_reg$freq),]
conta_occorrenze_reg

#Dove sono localizzate le amministrazioni presenti nei dataset?
nome_reg <- conta_occorrenze_reg$Regione
freq_reg <- conta_occorrenze_reg$freq

nome_reg
freq_reg

#Plot delle frequenze su mappa dell'Italia
range_of_colors <- list(low="#fff0f0", high="red3")

distribuzione_per_regione <- data.frame(
  denominazione = nome_reg,
  frequenza = freq_reg
)

mapIT(frequenza, denominazione, data=distribuzione_per_regione, guide.label="Distribuzione amministrazioni per regione", graphPar=range_of_colors)

#Qual è la loro distribuzione per provincia rispetto al numero di gare effettuate nel corso dell’anno?
#Qui vengono importati gli shapefile dei confini provinciali italiani. Dati ISTAT, aggiornati al 2018.
shape_province <- readOGR("il_tuo_path_qui/Limiti01012018_g/ProvCM01012018_g", "ProvCM01012018_g_WGS84")

#Calcolo delle occorrenze per provincia
temp_tab_amministrazioni_plus <- select(tab_amministrazioni_plus, "CFAmmne", "des_amm", "Provincia")

#Eliminazione dei campi vuoti ("NA") dalla colonna "Provincia"
nonull_tab_province<-subset(temp_tab_amministrazioni_plus,temp_tab_amministrazioni_plus$Provincia!="")

conta_occorrenze_prov <- count(nonull_tab_province, vars = "Provincia")
conta_occorrenze_prov <- conta_occorrenze_prov[order(-conta_occorrenze_prov$freq),]
conta_occorrenze_prov

#Quante province sono mappabili nei dati ISTAT?
conta_occorrenze_prov$Provincia %in% shape_province$SIGLA  

#Quali province vengono escluse?
conta_occorrenze_prov$Provincia[!conta_occorrenze_prov$Provincia %in% shape_province$SIGLA]

#Left join per aumentare gli attributi dello shapefile
shape_province@data <- left_join(shape_province@data, conta_occorrenze_prov, by = c('SIGLA' = "Provincia"))

#Plot della mappa
qtm(shape_province, fill= "freq", fill.palette="Blues", main = "Titolo")

# Numero gare e plot su grafico a torta
conta_gare_di_amministrazioni <- nonull_tab_province

conta_gare_di_amministrazioni <- count(conta_gare_di_amministrazioni, vars = "conta_gare_di_amministrazioni$des_amm") 
conta_gare_di_amministrazioni <- conta_gare_di_amministrazioni %>% mutate(ypos = (cumsum(conta_gare_di_amministrazioni$freq) - 0.5*conta_gare_di_amministrazioni$freq))

colnames(conta_gare_di_amministrazioni)[1] <- "NomeAmministrazione"
colnames(conta_gare_di_amministrazioni)[2] <- "Frequenza"

#È importante ordinare la colonna "Frequenza" in ordine decrescente
conta_gare_di_amministrazioni <- conta_gare_di_amministrazioni[order(-conta_gare_di_amministrazioni$Frequenza),]
head_conta_gare_di_amministrazioni <- head(conta_gare_di_amministrazioni, 9)
head_conta_gare_di_amministrazioni
nrow(conta_gare_di_amministrazioni)


#Stile del plot
t <- list(
  family = "Bitstream Charter",
  size = 12,
  color = toRGB("black"))

colors <- c("#CF000F", "#C93756", "#5D3F6A", "#1F4788", "#4B77BE", "#049372", "#16A085", "#407A52", "#F7CA18", "#317589")

#Pie chart amministrazioni
p <- plot_ly(head_conta_gare_di_amministrazioni, labels = ~NomeAmministrazione, values = ~Frequenza, type = 'pie',
             marker = list(colors = colors), height = 800) %>%
        layout(title = 'Le prime 10 pubbliche amministrazioni per numero gare',
               font=t,
         xaxis = list(showgrid = TRUE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = TRUE, zeroline = FALSE, showticklabels = FALSE),
         legend = list(x = 150, y = -20, orientation = 'h')
        ) %>%
        config(toImageButtonOptions = list(format = "svg"))
        
        
p

