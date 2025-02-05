#install.packages("tidyverse")
library(readr) # (für read_csv, read_delim, readLines)
library(dplyr) # (für daten aufarbeitung, joins, etc)
library(tidyr) # (drop_na, Datenreinigung)
library(stringr) # (string replace funktion für die Textdateien mit beschissenen Formaten.)

#--------------------------------------------------------------------------------------------------------------------
#Allgemeiner Import, wird immer benötigt, Fahrzeugmodell basiert darauf

Fahrzeuge_OEM1_Typ11 <- read_csv(".\\Data\\Fahrzeug\\Fahrzeuge_OEM1_Typ11.csv")
Bestandteile_Fahrzeuge_OEM1_Typ11 <- read_csv2(".\\Data\\Fahrzeug\\Bestandteile_Fahrzeuge_OEM1_Typ11.csv ")
#nur in der Fehleranalyse ist die Betriebsdauer angegeben, nirgendwo anders
Fahrzeuge_OEM1_Typ11_Fehleranalyse <- read_csv(".\\Data\\Fahrzeug\\Fahrzeuge_OEM1_Typ11_Fehleranalyse.csv")

# join alle drei Haupttabellen, um pro Fahrzeug nur einen Datensatz zu haben. Wird hier einmal gejoined und
# dann nur auf die Fehlerhaften Datensätze beschränkt, um den Aufwand für die Komponenten später zu reduzieren
Fahrzeuge_OEM1_Typ11 <- Fahrzeuge_OEM1_Typ11 %>%
  left_join(Bestandteile_Fahrzeuge_OEM1_Typ11, by="ID_Fahrzeug") %>%
  left_join(Fahrzeuge_OEM1_Typ11_Fehleranalyse, by="ID_Fahrzeug") %>%
  drop_na() %>%
  select(ID_Fahrzeug, ID_Motor, ID_Schaltung, ID_Karosserie, ID_Sitze, Betriebsdauer = days, Produktionsdatum)

#rm löscht Variablen die später nicht mehr benötigt werden. gibt RAM wieder frei
rm(Bestandteile_Fahrzeuge_OEM1_Typ11, Fahrzeuge_OEM1_Typ11_Fehleranalyse)

# Funktion die Komponenten mit den Fahrzeugdaten joined und die nötigen Parameter extrahiert
# Variable Join ist ein String, da für Motoren, Schaltungen etc. verschiedene ID'S gejoined werden müssen
Komponente_Transform <- function (Komponente, Join = ""){
  Komponente <- Komponente %>%
    filter(!is.na(Fehlerhaft_Datum)) %>% # nur Fehlerhafte Komponenten betrachten
    left_join(Fahrzeuge_OEM1_Typ11, by = Join) %>%
    drop_na() %>%
    mutate(Lieferdauer = Produktionsdatum.y - Produktionsdatum.x) %>% # Lieferdauer berechnen, aus dem Produktionsdatum der Komponente und dem Produktionsdatum des Fahrzeugs
    select(c(Fehlerhaft_Fahrleistung = Fehlerhaft_Fahrleistung, Betriebsdauer, Herstellernummer,
             ID_Self = matches(Join) , ID_Parent = ID_Fahrzeug, Lieferdauer, Produktionsdatum = Produktionsdatum.x))
}

# Funktion die Einzelteile mit den Daten der Komponente joined
# Variable Join String für die Teil ID, die für jedes Einzelteil anders ist
Einzelteil_Transform <- function(Teil, Komponente, Join = ""){
  Teil <- Teil %>%
    left_join(Komponente, by=Join) %>%
    drop_na() %>%
    mutate(Lieferdauer = Produktionsdatum.y - Produktionsdatum.x) %>%
    select(c(Fehlerhaft_Fahrleistung = Fehlerhaft_Fahrleistung.x, Betriebsdauer, Herstellernummer = Herstellernummer.x,
             ID_Self = matches(Join), ID_Parent = ID_Self, Lieferdauer, Produktionsdatum = Produktionsdatum.x))
}


#--------------------------------------------------------------------------------------------------------------------
#Komponente K1BE1
#Aufbau für jede Komponente gleich:
# 1. Einlesen der Datei, ist bei Komma und Semikolon getrennten Dateien relativ einfach mit vorgefertigen Funktionen möglich
Komponente_K1BE1 <- read_csv(".\\Data\\Komponente\\Komponente_K1BE1.csv")
#2. normalisieren der Tabelle, so das alle Spalten den gleichen Namen tragen, und alle Spalten den richtigen Dateityp haben
# ISt wichtig damit die Funktion funktioniert, und man nicht alle Schritte für jede Komponente einzeln machen muss
Komponente_K1BE1 <- Komponente_K1BE1 %>%
  mutate(Produktionsdatum = as.Date(Produktionsdatum_Origin_01011970)) %>%
  select(c(Fehlerhaft_Fahrleistung, Fehlerhaft_Datum, Herstellernummer, ID_Motor, Produktionsdatum))
# 3. Aufruf Funktion
Komponente_K1BE1 <- Komponente_Transform(Komponente_K1BE1, "ID_Motor")

# 4. Einlesen Bestandteile der Komponente
Bestandteile_Komponente_K1BE1 <- read_csv2(".\\Data\\Komponente\\Bestandteile_Komponente_K1BE1.csv")
# 5. Joinen der Bestandteile der Komponente mit der KOmponente, um die Einzelteile zuordnen zu können
Komponente_K1BE1 <- Komponente_K1BE1 %>%
  left_join(Bestandteile_Komponente_K1BE1, by=join_by("ID_Self" == ID_K1BE1))

# 6. Einlesen der Einzelteildateien, hier in dem Beispiel txt Datei
# Da einzeilig und mit 5 Zeichen langem Delimiter, muss erst im String einiges ersetz werden, so das es effizient umgewandelt werden kann
Einzelteil_T01_str <- readLines(paste(".\\Data\\Einzelteil\\Einzelteil_T01.txt"), warn=FALSE)
Einzelteil_T01_str <- str_replace_all(Einzelteil_T01_str, "[|]", "")
Einzelteil_T01_str <- str_replace_all(Einzelteil_T01_str, "[[:space:]]{3}", "\t")
Einzelteil_T01_str <- str_replace_all(Einzelteil_T01_str, " \"", "\n\"") # replace linebreaks
#read_delim braucht eine Datei, deswegen wird es hier in eine temporäre Datei gespeichert, wo der bearbeitete String zwischengespeichert wird
tf <- tempfile()
writeLines(Einzelteil_T01_str, tf)
Einzelteil_T01 <- read_delim(tf, col_names = c("ID", "X1", "ID_T1", "Produktionsdatum", "Herstellernummer", "Werksnummer",
                                               "Fehlerhaft", "Fehlerhaft_Datum", "Fehlerhaft_Fahrleistung"), skip = 1)
#der String und die Temporäre Datei werden wieder gelöscht
rm(Einzelteil_T01_str, tf)
#Copy, weil die gleiche Datei für eine andere Komponente gebraucht wird. muss nicht zweimal eingelesen werden
Einzelteil_T01_K1DI1 <- Einzelteil_T01

#7. Einzelteil normalisieren und vorbereiten für Funktion
Einzelteil_T01 <- Einzelteil_T01 %>%
  select(c(Fehlerhaft_Fahrleistung, Herstellernummer, ID_T1, Produktionsdatum))
#8 Einzelteil Funktionsaufruf
Einzelteil_T01 <- Einzelteil_Transform(Einzelteil_T01, Komponente_K1BE1, "ID_T1")

# 9.für alle Einzelteile der Komponente wiederholen
Einzelteil_T02_str <- readLines(paste(".\\Data\\Einzelteil\\Einzelteil_T02.txt"), warn=FALSE)
Einzelteil_T02_str <- str_replace_all(Einzelteil_T02_str, "\t", "\n") # replace linebreaks
Einzelteil_T02_str <- str_replace_all(Einzelteil_T02_str, "  ", ",") # replace linebreaks
tf <- tempfile()
writeLines(Einzelteil_T02_str, tf)
Einzelteil_T02 <- read_delim(tf, col_names = c("ID", "X1", "ID_T2", "Produktionsdatum", "Herstellernummer", "Werksnummer",
                                               "Fehlerhaft", "Fehlerhaft_Datum", "Fehlerhaft_Fahrleistung"), skip = 1)
Einzelteil_T02_K1DI1 <- Einzelteil_T02
rm(Einzelteil_T02_str, tf)
Einzelteil_T02 <- Einzelteil_T02 %>%
  #filter(!is.na(Fehlerhaft_Datum)) %>%
  select(c(Fehlerhaft_Fahrleistung, Herstellernummer, ID_T2, Produktionsdatum))
Einzelteil_T02 <- Einzelteil_Transform(Einzelteil_T02, Komponente_K1BE1, "ID_T2")

Einzelteil_T03_str <- readLines(paste(".\\Data\\Einzelteil\\Einzelteil_T03.txt"), warn=FALSE)
Einzelteil_T03_str <- str_replace_all(Einzelteil_T03_str, "", "\n")
tf <- tempfile()
writeLines(Einzelteil_T03_str, tf)
Einzelteil_T03 <- read_delim(tf, col_names = c("ID", "X1", "ID_T3", "Herstellernummer", "Werksnummer",
                                               "Fehlerhaft", "Fehlerhaft_Datum", "Fehlerhaft_Fahrleistung", "Produktionsdatum"), skip = 1)
rm(Einzelteil_T03_str, tf)
Einzelteil_T03 <- Einzelteil_T03 %>%
  #filter(!is.na(Fehlerhaft_Datum)) %>%
  mutate(Produktionsdatum = as.Date(Produktionsdatum)) %>%
  select(c(Fehlerhaft_Fahrleistung, Herstellernummer, ID_T3, Produktionsdatum))
Einzelteil_T03 <- Einzelteil_Transform(Einzelteil_T03, Komponente_K1BE1, "ID_T3")


Einzelteil_T04 <- read_csv2(".\\Data\\Einzelteil\\Einzelteil_T04.csv")
Einzelteil_T04 <- Einzelteil_T04 %>%
  #filter(!is.na(Fehlerhaft_Datum)) %>%
  mutate(Produktionsdatum = as.Date(Produktionsdatum_Origin_01011970)) %>%
  select(c(Fehlerhaft_Fahrleistung, Herstellernummer, ID_T4 = ID_T04, Produktionsdatum))
Einzelteil_T04 <- Einzelteil_Transform(Einzelteil_T04, Komponente_K1BE1, "ID_T4")

#10. Zusammenfügen aller Einzelteile in die Tabelle der Komponente.
# Select um alle Spalten raus zu werfen die nur für das joinen wichtig waren.
Komponente_K1BE1 <- Komponente_K1BE1 %>%
  select(c(Fehlerhaft_Fahrleistung, Betriebsdauer, Herstellernummer, ID_Self, ID_Parent, Lieferdauer, Produktionsdatum)) %>%
  bind_rows(list(Einzelteil_T01, Einzelteil_T02, Einzelteil_T03, Einzelteil_T04))

#11. Clean up
rm(Einzelteil_T01, Einzelteil_T02, Einzelteil_T03, Einzelteil_T04, Bestandteile_Komponente_K1BE1)

#--------------------------------------------------------------------------------------------------------------------
#Komponente K1DI1

# 12. Wiederhole Schema für alle Komponenten & Einzelteile
Komponente_K1DI1 <- read_csv(".\\Data\\Komponente\\Komponente_K1DI1.csv")
Komponente_K1DI1 <- Komponente_K1DI1 %>%
  select(c(Fehlerhaft_Fahrleistung = Fehlerhaft_Fahrleistung.x, Fehlerhaft_Datum = Fehlerhaft_Datum.x,
           Herstellernummer = Herstellernummer.x, ID_Motor = ID_Motor.x, Produktionsdatum = Produktionsdatum.x))

Komponente_K1DI1 <- Komponente_Transform(Komponente_K1DI1, "ID_Motor")
Bestandteile_Komponente_K1DI1 <- read_csv2(".\\Data\\Komponente\\Bestandteile_Komponente_K1DI1.csv")

Komponente_K1DI1 <- Komponente_K1DI1 %>%
  left_join(Bestandteile_Komponente_K1DI1, by=join_by("ID_Self" == ID_K1DI1))

Einzelteil_T01_K1DI1 <- Einzelteil_T01_K1DI1 %>%
  #filter(!is.na(Fehlerhaft_Datum)) %>%
  select(c(Fehlerhaft_Fahrleistung, Herstellernummer, ID_T1, Produktionsdatum))
Einzelteil_T01_K1DI1 <- Einzelteil_Transform(Einzelteil_T01_K1DI1, Komponente_K1DI1, "ID_T1")

Einzelteil_T02_K1DI1 <- Einzelteil_T02_K1DI1 %>%
  #filter(!is.na(Fehlerhaft_Datum)) %>%
  select(c(Fehlerhaft_Fahrleistung, Herstellernummer, ID_T2, Produktionsdatum))
Einzelteil_T02_K1DI1 <- Einzelteil_Transform(Einzelteil_T02_K1DI1, Komponente_K1DI1, "ID_T2")

Einzelteil_T05 <- read_csv(".\\Data\\Einzelteil\\Einzelteil_T05.csv")
Einzelteil_T05 <- Einzelteil_T05 %>%
  #filter(!is.na(Fehlerhaft_Datum)) %>%
  select(c(Fehlerhaft_Fahrleistung = Fehlerhaft_Fahrleistung.x, Herstellernummer = Herstellernummer.x,
           ID_T5 = ID_T05.x, Produktionsdatum = Produktionsdatum.x))
Einzelteil_T05 <- Einzelteil_Transform(Einzelteil_T05, Komponente_K1DI1, "ID_T5")

Einzelteil_T06 <- read_csv(".\\Data\\Einzelteil\\Einzelteil_T06.csv")
Einzelteil_T06 <- Einzelteil_T06 %>%
  #filter(!is.na(Fehlerhaft_Datum)) %>%
  mutate(Produktionsdatum = as.Date(Produktionsdatum_Origin_01011970)) %>%
  select(c(Fehlerhaft_Fahrleistung, Herstellernummer, ID_T6 = ID_T06, Produktionsdatum))
Einzelteil_T06 <- Einzelteil_Transform(Einzelteil_T06, Komponente_K1DI1, "ID_T6")

Komponente_K1DI1 <- Komponente_K1DI1 %>%
  select(c(Fehlerhaft_Fahrleistung, Betriebsdauer, Herstellernummer, ID_Self, ID_Parent, Lieferdauer, Produktionsdatum)) %>%
  bind_rows(list(Einzelteil_T01_K1DI1, Einzelteil_T02_K1DI1, Einzelteil_T05, Einzelteil_T06))

rm(Einzelteil_T01_K1DI1, Einzelteil_T02_K1DI1, Einzelteil_T05, Einzelteil_T06, Bestandteile_Komponente_K1DI1)

#--------------------------------------------------------------------------------------------------------------------
#K2LE1

Komponente_K2LE1_str <- readLines(paste(".\\Data\\Komponente\\Komponente_K2LE1.txt"), warn=FALSE)
Komponente_K2LE1_str <- str_replace_all(Komponente_K2LE1_str, "", "\n") # replace linebreaks
Komponente_K2LE1_str <- str_replace_all(Komponente_K2LE1_str, "II", "\t") # replace tabs (coloumns)
tf <- tempfile()
writeLines(Komponente_K2LE1_str, tf)
Komponente_K2LE1 <- read_delim(tf, col_names = c("ID", "X1", "ID_Sitze", "Produktionsdatum", "Herstellernummer",
                                                 "Werksnummer", "Fehlerhaft", "Fehlerhaft_Datum", "Fehlerhaft_Fahrleistung"), skip = 1)
rm(Komponente_K2LE1_str, tf)

Komponente_K2LE1 <- Komponente_K2LE1 %>%
  select(c(Fehlerhaft_Fahrleistung, Fehlerhaft_Datum, Herstellernummer, ID_Sitze, Produktionsdatum))
Komponente_K2LE1 <- Komponente_Transform(Komponente_K2LE1, "ID_Sitze")

Bestandteile_Komponente_K2LE1 <- read_csv2(".\\Data\\Komponente\\Bestandteile_Komponente_K2LE1.csv")
Komponente_K2LE1 <- Komponente_K2LE1 %>%
  left_join(Bestandteile_Komponente_K2LE1, by = join_by("ID_Self" == "ID_K2LE1"))

Einzelteil_T11_str <- readLines(paste(".\\Data\\Einzelteil\\Einzelteil_T11.txt"), warn=FALSE)
Einzelteil_T11_str <- str_replace_all(Einzelteil_T11_str, "", "\n") # replace linebreaks
tf <- tempfile()
writeLines(Einzelteil_T11_str, tf)
Einzelteil_T11 <- read_delim(tf, col_names = c("ID", "X1", "ID_T11", "Herstellernummer", "Werksnummer",
                                               "Fehlerhaft", "Fehlerhaft_Datum", "Fehlerhaft_Fahrleistung",  "Produktionsdatum"), skip = 1)
Einzelteil_T11_K2ST1 <- Einzelteil_T11
rm(Einzelteil_T11_str, tf)
Einzelteil_T11 <- Einzelteil_T11 %>%
  #filter(!is.na(Fehlerhaft_Datum)) %>%
  mutate(Produktionsdatum = as.Date(Produktionsdatum)) %>%
  select(c(Fehlerhaft_Fahrleistung, Herstellernummer, ID_T11, Produktionsdatum))
Einzelteil_T11 <- Einzelteil_Transform(Einzelteil_T11, Komponente_K2LE1, "ID_T11")


Einzelteil_T14 <- read_csv2(".\\Data\\Einzelteil\\Einzelteil_T14.csv")
Einzelteil_T14 <- Einzelteil_T14 %>%
  #filter(!is.na(Fehlerhaft_Datum)) %>%
  mutate(Produktionsdatum = as.Date(Produktionsdatum_Origin_01011970)) %>%
  select(c(Fehlerhaft_Fahrleistung, Herstellernummer, ID_T14, Produktionsdatum))
Einzelteil_T14 <- Einzelteil_Transform(Einzelteil_T14, Komponente_K2LE1, "ID_T14")

Einzelteil_T15 <- read_csv2(".\\Data\\Einzelteil\\Einzelteil_T15.csv")
Einzelteil_T15 <- Einzelteil_T15 %>%
  #filter(!is.na(Fehlerhaft_Datum)) %>%
  select(c(Fehlerhaft_Fahrleistung = Fehlerhaft_Fahrleistung.x, Herstellernummer = Herstellernummer.x, ID_T15 = ID_T15.x, Produktionsdatum = Produktionsdatum.x))
Einzelteil_T15 <- Einzelteil_Transform(Einzelteil_T15, Komponente_K2LE1, "ID_T15")

Komponente_K2LE1 <- Komponente_K2LE1 %>%
  select(c(Fehlerhaft_Fahrleistung, Betriebsdauer, Herstellernummer, ID_Self, ID_Parent, Lieferdauer, Produktionsdatum)) %>%
  bind_rows(list(Einzelteil_T11, Einzelteil_T14, Einzelteil_T15))
rm(Einzelteil_T11, Einzelteil_T14, Einzelteil_T15, Bestandteile_Komponente_K2LE1)

#--------------------------------------------------------------------------------------------------------------------
# Komponente_K2ST1

Komponente_K2ST1 <- read_delim(".\\Data\\Komponente\\Komponente_K2ST1.txt", delim="|",
                               col_names = c("ID", "X1", "ID_Sitze", "Produktionsdatum", "Herstellernummer", "Werksnummer",
                                             "Fehlerhaft", "Fehlerhaft_Datum", "Fehlerhaft_Fahrleistung"), skip = 1)

Komponente_K2ST1 <- Komponente_K2ST1 %>%
  select(c(Fehlerhaft_Fahrleistung, Fehlerhaft_Datum, Herstellernummer, ID_Sitze, Produktionsdatum))
Komponente_K2ST1 <- Komponente_Transform(Komponente_K2ST1, "ID_Sitze")

Bestandteile_Komponente_K2ST1 <- read_csv2(".\\Data\\Komponente\\Bestandteile_Komponente_K2ST1.csv")
Komponente_K2ST1 <- Komponente_K2ST1 %>%
  left_join(Bestandteile_Komponente_K2ST1, by = join_by("ID_Self" == "ID_K2ST1"))

Einzelteil_T11_K2ST1 <- Einzelteil_T11_K2ST1 %>%
  #filter(!is.na(Fehlerhaft_Datum)) %>%
  mutate(Produktionsdatum = as.Date(Produktionsdatum)) %>%
  select(c(Fehlerhaft_Fahrleistung, Herstellernummer, ID_T11, Produktionsdatum))
Einzelteil_T11_K2ST1 <- Einzelteil_Transform(Einzelteil_T11_K2ST1, Komponente_K2ST1, "ID_T11")

Einzelteil_T12 <- read_csv2(".\\Data\\Einzelteil\\Einzelteil_T12.csv")
Einzelteil_T12 <- Einzelteil_T12 %>%
  #filter(!is.na(Fehlerhaft_Datum)) %>%
  select(c(Fehlerhaft_Fahrleistung = Fehlerhaft_Fahrleistung.x, Herstellernummer = Herstellernummer.x, ID_T12 = ID_T12.x,
           Produktionsdatum = Produktionsdatum.x))
Einzelteil_T12 <- Einzelteil_Transform(Einzelteil_T12, Komponente_K2ST1, "ID_T12")

Einzelteil_T13 <- read_csv2(".\\Data\\Einzelteil\\Einzelteil_T13.csv")
Einzelteil_T13 <- Einzelteil_T13 %>%
  mutate(Produktionsdatum = as.Date(Produktionsdatum_Origin_01011970)) %>%
  select(c(Fehlerhaft_Fahrleistung, Herstellernummer, ID_T13, Produktionsdatum))
Einzelteil_T13 <- Einzelteil_Transform(Einzelteil_T13, Komponente_K2ST1, "ID_T13")

Komponente_K2ST1 <- Komponente_K2ST1 %>%
  select(c(Fehlerhaft_Fahrleistung, Betriebsdauer, Herstellernummer, ID_Self, ID_Parent, Lieferdauer, Produktionsdatum)) %>%
  bind_rows(list(Einzelteil_T11_K2ST1, Einzelteil_T12, Einzelteil_T13))
rm(Einzelteil_T11_K2ST1, Einzelteil_T12, Einzelteil_T13, Bestandteile_Komponente_K2ST1)

#--------------------------------------------------------------------------------------------------------------------
# Komponente_K3AG1

Komponente_K3AG1 <- read_csv(".\\Data\\Komponente\\Komponente_K3AG1.csv")

Komponente_K3AG1 <- Komponente_K3AG1 %>%
  select(c(Fehlerhaft_Fahrleistung = Fehlerhaft_Fahrleistung.x, Fehlerhaft_Datum = Fehlerhaft_Datum.x,
           Herstellernummer = Herstellernummer.x, ID_Schaltung = ID_Schaltung.x, Produktionsdatum = Produktionsdatum.x))
Komponente_K3AG1 <- Komponente_Transform(Komponente_K3AG1, "ID_Schaltung")

Bestandteile_Komponente_K3AG1 <- read_csv2(".\\Data\\Komponente\\Bestandteile_Komponente_K3AG1.csv")
Komponente_K3AG1 <- Komponente_K3AG1 %>%
  left_join(Bestandteile_Komponente_K3AG1, by = join_by("ID_Self" == "ID_K3AG1"))

Einzelteil_T21 <- read_csv2(".\\Data\\Einzelteil\\Einzelteil_T21.csv")
Einzelteil_T21_K3SG1 <- Einzelteil_T21
Einzelteil_T21 <- Einzelteil_T21 %>%
  #filter(!is.na(Fehlerhaft_Datum)) %>%
  mutate(Produktionsdatum = as.Date(Produktionsdatum_Origin_01011970)) %>%
  select(c(Fehlerhaft_Fahrleistung, Herstellernummer, ID_T21, Produktionsdatum))
Einzelteil_T21 <- Einzelteil_Transform(Einzelteil_T21, Komponente_K3AG1, "ID_T21")

Einzelteil_T24_str <- readLines(paste(".\\Data\\Einzelteil\\Einzelteil_T24.txt"), warn=FALSE)
Einzelteil_T24_str <- str_replace_all(Einzelteil_T24_str, "", "\n") # replace linebreaks
Einzelteil_T24_str <- str_replace_all(Einzelteil_T24_str, "  ", ",")
tf <- tempfile()
writeLines(Einzelteil_T24_str, tf)
Einzelteil_T24 <- read_delim(tf, col_names = c("ID", "X1", "ID_T24", "Produktionsdatum", "Herstellernummer", "Werksnummer",
                                               "Fehlerhaft", "Fehlerhaft_Datum", "Fehlerhaft_Fahrleistung"), skip = 1)
rm(Einzelteil_T24_str, tf)
Einzelteil_T24 <- Einzelteil_T24 %>%
  #filter(!is.na(Fehlerhaft_Datum)) %>%
  select(c(Fehlerhaft_Fahrleistung, Herstellernummer, ID_T24, Produktionsdatum))
Einzelteil_T24 <- Einzelteil_Transform(Einzelteil_T24, Komponente_K3AG1, "ID_T24")

Einzelteil_T25 <- read_csv(".\\Data\\Einzelteil\\Einzelteil_T25.csv")
Einzelteil_T25 <- Einzelteil_T25 %>%
  #filter(!is.na(Fehlerhaft_Datum)) %>%
  mutate(Produktionsdatum = as.Date(Produktionsdatum_Origin_01011970)) %>%
  select(c(Fehlerhaft_Fahrleistung, Herstellernummer, ID_T25, Produktionsdatum))
Einzelteil_T25 <- Einzelteil_Transform(Einzelteil_T25, Komponente_K3AG1, "ID_T25")

Komponente_K3AG1 <- Komponente_K3AG1 %>%
  select(c(Fehlerhaft_Fahrleistung, Betriebsdauer, Herstellernummer, ID_Self, ID_Parent, Lieferdauer, Produktionsdatum)) %>%
  bind_rows(list( Einzelteil_T21, Einzelteil_T24, Einzelteil_T25))
rm(Einzelteil_T21, Einzelteil_T24, Einzelteil_T25, Bestandteile_Komponente_K3AG1)

#--------------------------------------------------------------------------------------------------------------------
# Komponente_K3SG1

Komponente_K3SG1 <- read_csv(".\\Data\\Komponente\\Komponente_K3SG1.csv")

Komponente_K3SG1 <- Komponente_K3SG1 %>%
  select(c(Fehlerhaft_Fahrleistung = Fehlerhaft_Fahrleistung.x, Fehlerhaft_Datum = Fehlerhaft_Datum.x,
           Herstellernummer = Herstellernummer.x, ID_Schaltung = ID_Schaltung.x, Produktionsdatum = Produktionsdatum.x))
Komponente_K3SG1 <- Komponente_Transform(Komponente_K3SG1, "ID_Schaltung")

Bestandteile_Komponente_K3SG1 <- read_csv2(".\\Data\\Komponente\\Bestandteile_Komponente_K3SG1.csv")
Komponente_K3SG1 <- Komponente_K3SG1 %>%
  left_join(Bestandteile_Komponente_K3SG1, by = join_by("ID_Self" == "ID_K3SG1"))

Einzelteil_T21_K3SG1 <- Einzelteil_T21_K3SG1 %>%
  #filter(!is.na(Fehlerhaft_Datum)) %>%
  mutate(Produktionsdatum = as.Date(Produktionsdatum_Origin_01011970)) %>%
  select(c(Fehlerhaft_Fahrleistung, Herstellernummer, ID_T21, Produktionsdatum))
Einzelteil_T21_K3SG1 <- Einzelteil_Transform(Einzelteil_T21_K3SG1, Komponente_K3SG1, "ID_T21")

Einzelteil_T22_str <- readLines(paste(".\\Data\\Einzelteil\\Einzelteil_T22.txt"), warn=FALSE)
Einzelteil_T22_str <- str_replace_all(Einzelteil_T22_str, "NA\"", "NA\n\"")
tf <- tempfile()
writeLines(Einzelteil_T22_str, tf)
Einzelteil_T22 <- read_delim(tf, col_names = c("ID", "X1", "ID_T22", "Produktionsdatum", "Herstellernummer", "Werksnummer",
                                               "Fehlerhaft", "Fehlerhaft_Datum", "Fehlerhaft_Fahrleistung"), skip = 1)
rm(Einzelteil_T22_str, tf)
Einzelteil_T22 <- Einzelteil_T22 %>%
  #filter(!is.na(Fehlerhaft_Datum)) %>%
  select(c(Fehlerhaft_Fahrleistung, Herstellernummer, ID_T22, Produktionsdatum))
Einzelteil_T22 <- Einzelteil_Transform(Einzelteil_T22, Komponente_K3SG1, "ID_T22")

Einzelteil_T23 <- read_csv2(".\\Data\\Einzelteil\\Einzelteil_T23.csv")
Einzelteil_T23 <- Einzelteil_T23 %>%
  #filter(!is.na(Fehlerhaft_Datum)) %>%
  select(c(Fehlerhaft_Fahrleistung = Fehlerhaft_Fahrleistung.x, Herstellernummer = Herstellernummer.x, ID_T23 = ID_T23.x,
           Produktionsdatum = Produktionsdatum.x))
Einzelteil_T23 <- Einzelteil_Transform(Einzelteil_T23, Komponente_K3SG1, "ID_T23")

Komponente_K3SG1 <- Komponente_K3SG1 %>%
  select(c(Fehlerhaft_Fahrleistung, Betriebsdauer, Herstellernummer, ID_Self, ID_Parent, Lieferdauer, Produktionsdatum)) %>%
  bind_rows(list(Einzelteil_T21_K3SG1, Einzelteil_T22, Einzelteil_T23))
rm(Einzelteil_T21_K3SG1, Einzelteil_T22, Einzelteil_T23, Bestandteile_Komponente_K3SG1)
#--------------------------------------------------------------------------------------------------------------------
# Komponente_K4

Komponente_K4 <- read_csv2(".\\Data\\Komponente\\Komponente_K4.csv")

Komponente_K4 <- Komponente_K4 %>%
  select(c(Fehlerhaft_Fahrleistung = Fehlerhaft_Fahrleistung.x, Fehlerhaft_Datum = Fehlerhaft_Datum.x,
           Herstellernummer = Herstellernummer.x, ID_Karosserie = ID_Karosserie.x, Produktionsdatum = Produktionsdatum.x))
Komponente_K4 <- Komponente_Transform(Komponente_K4, "ID_Karosserie")

Bestandteile_Komponente_K4 <- read_csv2(".\\Data\\Komponente\\Bestandteile_Komponente_K4.csv")
Komponente_K4 <- Komponente_K4 %>%
  left_join(Bestandteile_Komponente_K4, by = join_by("ID_Self" == "ID_K4"))

Einzelteil_T30 <- read_csv(".\\Data\\Einzelteil\\Einzelteil_T30.csv")
Einzelteil_T30 <- Einzelteil_T30 %>%
  #filter(!is.na(Fehlerhaft_Datum)) %>%
  select(c(Fehlerhaft_Fahrleistung = Fehlerhaft_Fahrleistung.x, Herstellernummer = Herstellernummer.x, ID_T30 = ID_T30.x, Produktionsdatum = Produktionsdatum.x))
Einzelteil_T30 <- Einzelteil_Transform(Einzelteil_T30, Komponente_K4, "ID_T30")

Einzelteil_T31_str <- readLines(paste(".\\Data\\Einzelteil\\Einzelteil_T31.txt"), warn=FALSE)
Einzelteil_T31_str <- str_replace_all(Einzelteil_T31_str, "", "\n")
Einzelteil_T31_str <- str_replace_all(Einzelteil_T31_str, "  ", ",")
tf <- tempfile()
writeLines(Einzelteil_T31_str, tf)
Einzelteil_T31 <- read_delim(tf, col_names = c("ID", "X1", "ID_T31", "Herstellernummer", "Werksnummer",
                                               "Fehlerhaft", "Fehlerhaft_Datum", "Fehlerhaft_Fahrleistung", "Produktionsdatum"), skip = 1)
rm(Einzelteil_T31_str, tf)
Einzelteil_T31 <- Einzelteil_T31 %>%
  #filter(!is.na(Fehlerhaft_Datum)) %>%
  mutate(Produktionsdatum = as.Date(Produktionsdatum)) %>%
  select(c(Fehlerhaft_Fahrleistung, Herstellernummer, ID_T31, Produktionsdatum))
Einzelteil_T31 <- Einzelteil_Transform(Einzelteil_T31, Komponente_K4, "ID_T31")

Einzelteil_T32 <- read_csv2(".\\Data\\Einzelteil\\Einzelteil_T32.csv")
Einzelteil_T32 <- Einzelteil_T32 %>%
  #filter(!is.na(Fehlerhaft_Datum)) %>%
  select(c(Fehlerhaft_Fahrleistung = Fehlerhaft_Fahrleistung.x, Herstellernummer = Herstellernummer.x, ID_T32 = ID_T32.x,
           Produktionsdatum = Produktionsdatum.x))
Einzelteil_T32 <- Einzelteil_Transform(Einzelteil_T32, Komponente_K4, "ID_T32")

Komponente_K4 <- Komponente_K4 %>%
  select(c(Fehlerhaft_Fahrleistung, Betriebsdauer, Herstellernummer, ID_Self, ID_Parent, Lieferdauer, Produktionsdatum)) %>%
  bind_rows(list(Einzelteil_T30, Einzelteil_T31, Einzelteil_T32))
rm(Einzelteil_T30, Einzelteil_T31, Einzelteil_T32, Bestandteile_Komponente_K4)


#Füge alle Komponenten hintereinander in eine neue Tabelle
result <- bind_rows(list(Komponente_K1BE1, Komponente_K1DI1 ,Komponente_K2LE1, Komponente_K2ST1, Komponente_K3AG1,
                          Komponente_K3SG1, Komponente_K4)) #%>%
            #filter(Produktionsdatum >= "2013-01-01" & Produktionsdatum <= "2015-12-31")

#schreibe Datei auf festplatte
write_csv(result, "Final_Data_Group11.csv")

# muss noch raus
test <- read_csv("Final_Data_Group11.csv")
test2 <- left_join(test, test, by = join_by(ID_Parent == ID_Self)) %>%
  drop_na()