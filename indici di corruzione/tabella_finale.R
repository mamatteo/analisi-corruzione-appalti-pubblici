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
library(scales)

#! Una volta impostato, non serve cambiare path se si cambiano i dati di input
p1 <- read.csv(file = "/Users/matteo/Desktop/Analisi dati in R 2015/Risultati finali/punteggio1_2015.csv", header = TRUE, sep = ";", na.strings = "", stringsAsFactors=FALSE)
p2 <- read.csv(file = "/Users/matteo/Desktop/Analisi dati in R 2015/Risultati finali/punteggio2_2015.csv", header = TRUE, sep = ";", na.strings = "", stringsAsFactors=FALSE)
p3 <- read.csv(file = "/Users/matteo/Desktop/Analisi dati in R 2015/Risultati finali/punteggio3_2015.csv", header = TRUE, sep = ";", na.strings = "", stringsAsFactors=FALSE)
p4 <- read.csv(file = "/Users/matteo/Desktop/Analisi dati in R 2015/Risultati finali/punteggio4_2015.csv", header = TRUE, sep = ";", na.strings = "", stringsAsFactors=FALSE)
p5 <- read.csv(file = "/Users/matteo/Desktop/Analisi dati in R 2015/Risultati finali/punteggio5_2015.csv", header = TRUE, sep = ";", na.strings = "", stringsAsFactors=FALSE)
p6 <- read.csv(file = "/Users/matteo/Desktop/Analisi dati in R 2015/Risultati finali/punteggio6_2015.csv", header = TRUE, sep = ";", na.strings = "", stringsAsFactors=FALSE)


#Join
tabella_finale <- inner_join(p1, p2)
tabella_finale <- inner_join(tabella_finale, p3)
tabella_finale <- inner_join(tabella_finale, p4)
tabella_finale <- inner_join(tabella_finale, p5)
tabella_finale <- inner_join(tabella_finale, p6)


#Conversione delle colonne punteggi da "chr" a "dbl"
tabella_finale$p_i1_15 <- as.numeric(sub(",", ".", tabella_finale$p_i1_15, fixed = TRUE))
tabella_finale$p_i1_15_out <- as.numeric(sub(",", ".", tabella_finale$p_i1_15_out, fixed = TRUE))
tabella_finale$p_i2_15 <- as.numeric(sub(",", ".", tabella_finale$p_i2_15, fixed = TRUE))
tabella_finale$p_i2_15_out <- as.numeric(sub(",", ".", tabella_finale$p_i2_15_out, fixed = TRUE))
tabella_finale$p_i3_15 <- as.numeric(sub(",", ".", tabella_finale$p_i3_15, fixed = TRUE))
tabella_finale$p_i3_15_out <- as.numeric(sub(",", ".", tabella_finale$p_i3_15_out, fixed = TRUE))
tabella_finale$p_i4_15 <- as.numeric(sub(",", ".", tabella_finale$p_i4_15, fixed = TRUE))
tabella_finale$p_i4_15_out <- as.numeric(sub(",", ".", tabella_finale$p_i4_15_out, fixed = TRUE))
tabella_finale$p_i5_15 <- as.numeric(sub(",", ".", tabella_finale$p_i5_15, fixed = TRUE))
tabella_finale$p_i5_15_out <- as.numeric(sub(",", ".", tabella_finale$p_i5_15_out, fixed = TRUE))
tabella_finale$p_i6_15 <- as.numeric(sub(",", ".", tabella_finale$p_i6_15, fixed = TRUE))
tabella_finale$p_i6_15_out <- as.numeric(sub(",", ".", tabella_finale$p_i6_15_out, fixed = TRUE))

tabella_finale <- select(tabella_finale, "CFStazapp", "p_i1_15", "p_i2_15", "p_i3_15", "p_i4_15", "p_i5_15", "p_i6_15",
                         "p_i1_15_out", "p_i2_15_out", "p_i3_15_out", "p_i4_15_out", "p_i5_15_out", "p_i6_15_out" )

#Calcolo "Corruption Indicator Score"
tabella_finale$Corruption_Indicator_Score <- rowSums(tabella_finale[,2:7], na.rm=T)

#Calcolo "Corruption Indicator Score riscalato"
tabella_finale$Cis_Outliers <- rowSums(tabella_finale[,8:13], na.rm=T)

tabella_finale <- select(tabella_finale, "CFStazapp", "p_i1_15", "p_i1_15_out", "p_i2_15", "p_i2_15_out", "p_i3_15", "p_i3_15_out", 
                         "p_i4_15", "p_i4_15_out", "p_i5_15", "p_i5_15_out", "p_i6_15", "p_i6_15_out", "Corruption_Indicator_Score", "Cis_Outliers")



glimpse(tabella_finale)

ciao2015 <- sum(tabella_finale$Corruption_Indicator_Score)
mean(tabella_finale$Corruption_Indicator_Score)

write.csv2(tabella_finale,'/Users/matteo/Desktop/tabfinale2015.csv', row.names=FALSE)

cor(tabella_finale$Cis, tabella_finale$Cis_Out, method = c("pearson"))
cor(tabella_finale$Cis, tabella_finale$Cis_Out, method = c("kendall"))
cor(tabella_finale$Cis, tabella_finale$Cis_Out, method = c("spearman"))


#Seleziono p1, ..., p6 + Cis + Cis_outliers
tabella_finale <- select(tabella_finale, "CFStazapp", "p_i1_15", "p_i2_15", "p_i3_15", 
                         "p_i4_15", "p_i5_15", "p_i6_15", "Cis", "Cis_Outliers")

tabella_finale <- tabella_finale %>% 
  rename(
    p1 = p_i1_15,
    p2 = p_i2_15,
    p3 = p_i3_15,
    p4 = p_i4_15,
    p5 = p_i5_15,
    p6 = p_i6_15,
    p1_out = p_i1_15_out,
    p2_out = p_i2_15_out,
    p3_out = p_i3_15_out,
    p4_out = p_i4_15_out,
    p5_out = p_i5_15_out,
    p6_out = p_i6_15_out,
    Cis = Corruption_Indicator_Score,
    Cis_Out = Cis_Outliers)

ggcorr(tabella_finale, method = c("all.obs", "kendall"), label = TRUE)
names(tabella_finale)

x<-c(1,2,3)
y<-c(1,2,3)
cor(x,y)
df <- data.frame(x,y)
df
somma1 <- colSums(df[y,])
somma1


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
leprimedieci <- head(tabella_finale_ordinata,1000) 

#Le prime dieci in tabella
sign_formatter_01 <- formatter("span", 
                            style = x ~ style(color = ifelse(x > 0.5, "red",
                                                      ifelse(x == 0.5, "blue",
                                                      ifelse(x < 0.49, "green", "black"))), font.family = "Bitstream Charter"))



formattable(leprimedieci, align =c("l","c","c","c","c", "c", "c", "c"), 
                               list(
                               p_i1_15 = sign_formatter_01,
                               p_i2_15 = sign_formatter_01,
                               p_i3_15 = sign_formatter_01,
                               p_i4_15 = sign_formatter_01,
                               p_i5_15 = sign_formatter_01,
                               p_i5_15 = sign_formatter_01,
                               p_i1_15_out = sign_formatter_01,
                               p_i2_15_out = sign_formatter_01,
                               p_i3_15_out = sign_formatter_01,
                               p_i4_15_out = sign_formatter_01,
                               p_i5_15_out = sign_formatter_01,
                               p_i6_15_out = sign_formatter_01))


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

#Punteggio_ik con indice di corruzione finale...
round(cor(tabella_finale$Punteggio_i1, tabella_finale$Corruption_Indicator_Score),2)
round(cor(tabella_finale$Punteggio_i2, tabella_finale$Corruption_Indicator_Score),2)
round(cor(tabella_finale$Punteggio_i3, tabella_finale$Corruption_Indicator_Score),2)
round(cor(tabella_finale$Punteggio_i4, tabella_finale$Corruption_Indicator_Score),2)
round(cor(tabella_finale$Punteggio_i5, tabella_finale$Corruption_Indicator_Score),2)
round(cor(tabella_finale$Punteggio_i5, tabella_finale$Corruption_Indicator_Score),2)
ggcorr(tabella_finale)
