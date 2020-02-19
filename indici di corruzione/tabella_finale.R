#CODICE PER LA COSTRUZIONE DELLA TABELLA FINALE

#Pulizia ambiente di lavoro
rm(list = ls(all.names = TRUE)) # Pulizia degli oggetti creati 
gc() # Pulizia della memoria RAM

#Librerie
library(dplyr)
library(tidyr)
library(formattable)
library(GGally)
library(ggplot2)

p1 <- read.csv(file = "/Users/matteo/Desktop/Analisi dati in R/Risultati finali/punteggio1.csv", header = TRUE, sep = ";", na.strings = "", stringsAsFactors=FALSE)
p2 <- read.csv(file = "/Users/matteo/Desktop/Analisi dati in R/Risultati finali/punteggio2.csv", header = TRUE, sep = ";", na.strings = "", stringsAsFactors=FALSE)
p3 <- read.csv(file = "/Users/matteo/Desktop/Analisi dati in R/Risultati finali/punteggio3.csv", header = TRUE, sep = ";", na.strings = "", stringsAsFactors=FALSE)
p4 <- read.csv(file = "/Users/matteo/Desktop/Analisi dati in R/Risultati finali/punteggio4.csv", header = TRUE, sep = ";", na.strings = "", stringsAsFactors=FALSE)
p5 <- read.csv(file = "/Users/matteo/Desktop/Analisi dati in R/Risultati finali/punteggio5.csv", header = TRUE, sep = ";", na.strings = "", stringsAsFactors=FALSE)
p6 <- read.csv(file = "/Users/matteo/Desktop/Analisi dati in R/Risultati finali/punteggio6.csv", header = TRUE, sep = ";", na.strings = "", stringsAsFactors=FALSE)


#Join
tabella_finale <- inner_join(p1, p2)
tabella_finale <- inner_join(tabella_finale, p3)
tabella_finale <- inner_join(tabella_finale, p4)
tabella_finale <- inner_join(tabella_finale, p5)
tabella_finale <- inner_join(tabella_finale, p6)


#Conversione delle colonne punteggi da "chr" a "dbl"
tabella_finale$Punteggio_i1 <- as.numeric(sub(",", ".", tabella_finale$Punteggio_i1, fixed = TRUE))
tabella_finale$Punteggio_i2 <- as.numeric(sub(",", ".", tabella_finale$Punteggio_i2, fixed = TRUE))
tabella_finale$Punteggio_i3 <- as.numeric(sub(",", ".", tabella_finale$Punteggio_i3, fixed = TRUE))
tabella_finale$Punteggio_i4 <- as.numeric(sub(",", ".", tabella_finale$Punteggio_i4, fixed = TRUE))
tabella_finale$Punteggio_i5 <- as.numeric(sub(",", ".", tabella_finale$Punteggio_i5, fixed = TRUE))
tabella_finale$Punteggio_i6 <- as.numeric(sub(",", ".", tabella_finale$Punteggio_i6, fixed = TRUE))

#Calcolo "Corruption Indicator Score"
tabella_finale$Corruption_Indicator_Score <- rowSums(tabella_finale[,2:7], na.rm=T)

tabella_finale <- tabella_finale %>% 
  rename(
    p1 = Punteggio_i1,
    p2 = Punteggio_i2,
    p3 = Punteggio_i3,
    p4 = Punteggio_i4,
    p5 = Punteggio_i5,
    p6 = Punteggio_i6,
  )

#Ordino la tabella finale per punteggi decrescenti
tabella_finale_ordinata <- tabella_finale[order(-tabella_finale$Corruption_Indicator_Score),]
t <-10
leprime_t <- head(tabella_finale_ordinata,t) 

#Le prime x in tabella
sign_formatter_01 <- formatter("span", 
                            style = x ~ style(color = ifelse(x > 0.5, "red",
                                                      ifelse(x == 0.5, "blue",
                                                      ifelse(x < 0.49, "green", "black"))), font.family = "Bitstream Charter"))



formattable(leprime_t, align =c("l","c","c","c","c", "c", "c", "c"), 
                               list(
                               p1 = sign_formatter_01,
                               p2 = sign_formatter_01,
                               p3 = sign_formatter_01,
                               p4 = sign_formatter_01,
                               p5 = sign_formatter_01,
                               p6 = sign_formatter_01))


#Correlazioni
#Punteggio_i1 con...
round(cor(tabella_finale$Punteggio_i1, tabella_finale$Punteggio_i2),2)
round(cor(tabella_finale$Punteggio_i1, tabella_finale$Punteggio_i3),2)
round(cor(tabella_finale$Punteggio_i1, tabella_finale$Punteggio_i4),2)
round(cor(tabella_finale$Punteggio_i1, tabella_finale$Punteggio_i5),2)
round(cor(tabella_finale$Punteggio_i1, tabella_finale$Punteggio_i6),2)

#Punteggio_i2 con...
round(cor(tabella_finale$Punteggio_i2, tabella_finale$Punteggio_i3),2)
round(cor(tabella_finale$Punteggio_i2, tabella_finale$Punteggio_i4),2)
round(cor(tabella_finale$Punteggio_i2, tabella_finale$Punteggio_i5),2)
round(cor(tabella_finale$Punteggio_i2, tabella_finale$Punteggio_i6),2)

#Punteggio_i3 con...
round(cor(tabella_finale$Punteggio_i3, tabella_finale$Punteggio_i4),2)
round(cor(tabella_finale$Punteggio_i3, tabella_finale$Punteggio_i5),2)
round(cor(tabella_finale$Punteggio_i3, tabella_finale$Punteggio_i6),2)

#Punteggio_i4 con...
round(cor(tabella_finale$Punteggio_i4, tabella_finale$Punteggio_i5),2)
round(cor(tabella_finale$Punteggio_i4, tabella_finale$Punteggio_i6),2)

#Matrice delle correlazioni
ggcorr(tabella_finale)





















