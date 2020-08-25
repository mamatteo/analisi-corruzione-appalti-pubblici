# Analisi della corruzione negli appalti pubblici

## Cos'è questo _repository_
Questo _repository_ contiene i codici utilizzati per condurre le analisi descritte nella mia Tesi Magistrale in Informatica. Tutto il codice è stato sviluppato in R, attraverso l'ambiente di sviluppo RStudio. 

Questo _repository_ nasce con un duplice intento: da un lato quello di riportare il codice prodotto e poterlo così più facilmente visualizzare. Dall'altro quello di condividere il codice prodotto con chiunque voglia contribuire al suo miglioramento.  

## Cosa trovi in questo _repository_ 
All'interno di questo repo, sono stati caricati i seguenti file: 
- `pulire_i_dati.R`: contiene le operazioni di _data cleaning_ svolte sui datatset;

La cartella "Indici di corruzione" invece, contiene i seguenti file:
- `indice_01.R`: contiene le operazioni necessarie al calcolo dell'indicatore di corruzione i;
- `indice_02.R`: contiene le operazioni necessarie al calcolo dell'indicatore di corruzione ii;
- `indice_03.R`: contiene le operazioni necessarie al calcolo dell'indicatore di corruzione iii;
- `indice_04.R`: contiene le operazioni necessarie al calcolo dell'indicatore di corruzione iv;
- `indice_05.R`: contiene le operazioni necessarie al calcolo dell'indicatore di corruzione v;
- `indice_06.R`: contiene le operazioni necessarie al calcolo dell'indicatore di corruzione vi;
- `tabella_finale.R`: consente di costruire la tabella finale con il riassunto di tutti i sei indicatori e il punteggio finale.

## 1) Pulizia dei dati (`pulire_i_dati.R`)
Il codice riportato nel file `pulire_i_dati.R` va completato con il proprio _path_ di input (la cartella da cui importare i dati) 

```
path_ds_appalti = ".../Appalti2015.csv"
path_ds_oggettogare = ".../Oggettigare2015.csv" 
path_ds_cigcup = ".../CigCup2015.csv"
path_ds_aggiudicatari = ".../Aggiudicatari2015.csv"
.
.
.
```
e con il proprio _path_ di output (la cartella dove salvare le tabelle finali). Si sostituiscano i tre punti con il proprio _path_. 

```
.
.
.
## | SALVATAGGIO TABELLE FINALI
write.csv2(tab_staz_appaltanti,'.../tab_staz_appaltanti.csv', row.names=FALSE)
write.csv2(tab_gare,'.../tab_gare.csv', row.names=FALSE)
write.csv2(tab_aggiudicatari,'.../tab_aggiudicatari.csv', row.names=FALSE)
```

## 2) Indici di corruzione (`indice_XY.R`)
Per ogni indicatore di corruzione studiato è stato realizzato uno script che si preoccupa di effettuarne il calcolo sui dati precedentemente puliti dal codice contenuto in `pulire_i_dati.R`.

## Licenza
Il codice presente in questo repo è distribuito secondo la liceza XYZ
