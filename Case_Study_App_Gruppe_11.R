# install.packages("tidyverse")
library(readr)
library(dplyr)
library(tidyr)
library(vroom)
library(stringr)
library(ggplot2)


#--------------------------------------------------------------------------------------------------------------------
#Allgemeiner Import, wird immer benötigt
Fahrzeuge_OEM1_Typ11_Fehleranalyse <- read_csv(".\\Data\\Fahrzeug\\Fahrzeuge_OEM1_Typ11_Fehleranalyse.csv")
Bestandteile_Fahrzeuge_OEM1_Typ11 <- read_csv2(".\\Data\\Fahrzeug\\Bestandteile_Fahrzeuge_OEM1_Typ11.csv ")
Fahrzeuge_OEM1_Typ11 <- read_csv(".\\Data\\Fahrzeug\\Fahrzeuge_OEM1_Typ11.csv")
# --------------------------------------------------------

#--------------------------------------------------------------------------------------------------------------------
#Komponente K1BE1

Komponente_K1BE1 <- read_csv(".\\Data\\Komponente\\Komponente_K1BE1.csv")
Komponente_K1BE1 <- Komponente_K1BE1 %>%
  filter(!is.na(Fehlerhaft_Datum)) %>%
  mutate(Produktionsdatum = as.Date(Produktionsdatum_Origin_01011970))%>%
  select(c(Fehlerhaft_Fahrleistung,X1, Herstellernummer, ID_Motor, Produktionsdatum)) %>%
  #filter(Produktionsdatum >= "2013-01-01" & Produktionsdatum <= "2015-12-31")
  left_join(Bestandteile_Fahrzeuge_OEM1_Typ11, by = "ID_Motor") %>% #id fahrzeug
  left_join(Fahrzeuge_OEM1_Typ11_Fehleranalyse, by = "ID_Fahrzeug") %>% #days
  drop_na() %>%
  left_join(Fahrzeuge_OEM1_Typ11, by="ID_Fahrzeug") %>%
  mutate(Lieferdauer = Produktionsdatum.y - Produktionsdatum.x) %>%
  select(c(Fehlerhaft_Fahrleistung = Fehlerhaft_Fahrleistung.x, Betriebsdauer = days,
           Herstellernummer = Herstellernummer.x, ID_Self = ID_Motor, ID_Parent = ID_Fahrzeug, Lieferdauer,
           Produktionsdatum = Produktionsdatum.x))

Bestandteile_Komponente_K1BE1 <- read_csv2(".\\Data\\Komponente\\Bestandteile_Komponente_K1BE1.csv")

Einzelteil_T01_str <- readLines(paste(".\\Data\\Einzelteil\\Einzelteil_T01.txt", sep = ""))
Einzelteil_T01_str <- str_replace_all(Einzelteil_T01_str, "[|]", "")
Einzelteil_T01_str <- str_replace_all(Einzelteil_T01_str, "[[:space:]]{3}", "\t")
Einzelteil_T01_str <- str_replace_all(Einzelteil_T01_str, " \"", "\n\"") # replace linebreaks
tf <- tempfile()
writeLines(Einzelteil_T01_str, tf)
Einzelteil_T01 <- read_delim(tf, col_names = c("ID", "X1", "ID_T1", "Produktionsdatum", "Herstellernummer", "Werksnummer",
                                               "Fehlerhaft", "Fehlerhaft_Datum", "Fehlerhaft_Fahrleistung"), skip = 1)
rm(Einzelteil_T01_str, tf)
Einzelteil_T01_K1DI1 <- Einzelteil_T01
Einzelteil_T01 <- Einzelteil_T01 %>%
  #filter(!is.na(Fehlerhaft_Datum)) %>%
  select(c(Fehlerhaft_Fahrleistung, Herstellernummer, ID_T1, Produktionsdatum)) %>%
  left_join(Bestandteile_Komponente_K1BE1, by="ID_T1") %>%
  left_join(Komponente_K1BE1, by = join_by(ID_K1BE1 == "ID_Self")) %>%
  drop_na() %>%
  mutate(Lieferdauer = Produktionsdatum.y - Produktionsdatum.x) %>%
  select(c(Fehlerhaft_Fahrleistung = Fehlerhaft_Fahrleistung.x, Betriebsdauer, Herstellernummer = Herstellernummer.x,
           ID_Self = ID_T1, ID_Parent = ID_K1BE1, Lieferdauer, Produktionsdatum = Produktionsdatum.x))


Einzelteil_T02_str <- readLines(paste(".\\Data\\Einzelteil\\Einzelteil_T02.txt", sep = ""))
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
  select(c(Fehlerhaft_Fahrleistung, Herstellernummer, ID_T2, Produktionsdatum)) %>%
  left_join(Bestandteile_Komponente_K1BE1, by="ID_T2") %>%
  left_join(Komponente_K1BE1, by = join_by(ID_K1BE1 == "ID_Self")) %>%
  drop_na() %>%
  mutate(Lieferdauer = Produktionsdatum.y - Produktionsdatum.x) %>%
  select(c(Fehlerhaft_Fahrleistung = Fehlerhaft_Fahrleistung.x, Betriebsdauer, Herstellernummer = Herstellernummer.x,
           ID_Self = ID_T2, ID_Parent = ID_K1BE1, Lieferdauer, Produktionsdatum = Produktionsdatum.x))


Einzelteil_T03_str <- readLines(paste(".\\Data\\Einzelteil\\Einzelteil_T03.txt", sep = ""))
Einzelteil_T03_str <- str_replace_all(Einzelteil_T03_str, "", "\n")
tf <- tempfile()
writeLines(Einzelteil_T03_str, tf)
Einzelteil_T03 <- read_delim(tf, col_names = c("ID", "X1", "ID_T3", "Herstellernummer", "Werksnummer",
                                               "Fehlerhaft", "Fehlerhaft_Datum", "Fehlerhaft_Fahrleistung", "Produktionsdatum"), skip = 1)
rm(Einzelteil_T03_str, tf)
Einzelteil_T03 <- Einzelteil_T03 %>%
  #filter(!is.na(Fehlerhaft_Datum)) %>%
  mutate(Produktionsdatum = as.Date(Produktionsdatum)) %>%
  select(c(Fehlerhaft_Fahrleistung, Herstellernummer, ID_T3, Produktionsdatum)) %>%
  left_join(Bestandteile_Komponente_K1BE1, by="ID_T3") %>%
  left_join(Komponente_K1BE1, by = join_by(ID_K1BE1 == "ID_Self")) %>%
  drop_na() %>%
  mutate(Lieferdauer = Produktionsdatum.y - Produktionsdatum.x) %>%
  select(c(Fehlerhaft_Fahrleistung = Fehlerhaft_Fahrleistung.x, Betriebsdauer, Herstellernummer = Herstellernummer.x,
           ID_Self = ID_T3, ID_Parent = ID_K1BE1, Lieferdauer, Produktionsdatum = Produktionsdatum.x))


Einzelteil_T04 <- read_csv2(".\\Data\\Einzelteil\\Einzelteil_T04.csv")
Einzelteil_T04 <- Einzelteil_T04 %>%
  #filter(!is.na(Fehlerhaft_Datum)) %>%
  mutate(Produktionsdatum = as.Date(Produktionsdatum_Origin_01011970)) %>%
  select(c(Fehlerhaft_Fahrleistung, Herstellernummer, ID_T4 = ID_T04, Produktionsdatum)) %>%
  left_join(Bestandteile_Komponente_K1BE1, by="ID_T4") %>%
  left_join(Komponente_K1BE1, by = join_by(ID_K1BE1 == "ID_Self")) %>%
  drop_na() %>%
  mutate(Lieferdauer = Produktionsdatum.y - Produktionsdatum.x) %>%
  select(c(Fehlerhaft_Fahrleistung = Fehlerhaft_Fahrleistung.x, Betriebsdauer, Herstellernummer = Herstellernummer.x,
           ID_Self = ID_T4, ID_Parent = ID_K1BE1, Lieferdauer, Produktionsdatum = Produktionsdatum.x))

Komponente_K1BE1 <- bind_rows(list(Komponente_K1BE1, Einzelteil_T01, Einzelteil_T02, Einzelteil_T03, Einzelteil_T04))
rm(Einzelteil_T01, Einzelteil_T02, Einzelteil_T03, Einzelteil_T04, Bestandteile_Komponente_K1BE1)

#--------------------------------------------------------------------------------------------------------------------
#Komponente K1DI1

Komponente_K1DI1 <- read_csv(".\\Data\\Komponente\\Komponente_K1DI1.csv")
Komponente_K1DI1 <- Komponente_K1DI1 %>%
  filter(!is.na(Fehlerhaft_Datum.x)) %>%
  select(c(Fehlerhaft_Fahrleistung.x,X1, Herstellernummer.x, ID_Motor.x, Produktionsdatum.x)) %>%
  #filter(Produktionsdatum.x >= "2013-01-01" & Produktionsdatum.x <= "2015-12-31") %>%
  left_join(Bestandteile_Fahrzeuge_OEM1_Typ11, by = join_by(ID_Motor.x == ID_Motor)) %>%
  left_join(Fahrzeuge_OEM1_Typ11_Fehleranalyse, by = "ID_Fahrzeug") %>%
  drop_na() %>%
  left_join(Fahrzeuge_OEM1_Typ11, by="ID_Fahrzeug") %>%
  mutate(Lieferdauer = Produktionsdatum - Produktionsdatum.x) %>%
  select(c(Fehlerhaft_Fahrleistung = Fehlerhaft_Fahrleistung.x, Betriebsdauer = days, Herstellernummer = Herstellernummer.x,
           ID_Self = ID_Motor.x, ID_Parent = ID_Fahrzeug, Lieferdauer, Produktionsdatum))

Bestandteile_Komponente_K1DI1 <- read_csv2(".\\Data\\Komponente\\Bestandteile_Komponente_K1DI1.csv")

Einzelteil_T01_K1DI1 <- Einzelteil_T01_K1DI1 %>%
  #filter(!is.na(Fehlerhaft_Datum)) %>%
  select(c(Fehlerhaft_Fahrleistung, Herstellernummer, ID_T1, Produktionsdatum)) %>%
  left_join(Bestandteile_Komponente_K1DI1, by="ID_T1") %>%
  left_join(Komponente_K1DI1, by = join_by(ID_K1DI1 == "ID_Self")) %>%
  drop_na() %>%
  mutate(Lieferdauer = Produktionsdatum.y - Produktionsdatum.x) %>%
  select(c(Fehlerhaft_Fahrleistung = Fehlerhaft_Fahrleistung.x, Betriebsdauer, Herstellernummer = Herstellernummer.x,
           ID_Self = ID_T1, ID_Parent = ID_K1DI1, Lieferdauer, Produktionsdatum = Produktionsdatum.x))


Einzelteil_T02_K1DI1 <- Einzelteil_T02_K1DI1 %>%
  #filter(!is.na(Fehlerhaft_Datum)) %>%
  select(c(Fehlerhaft_Fahrleistung, Herstellernummer, ID_T2, Produktionsdatum)) %>%
  left_join(Bestandteile_Komponente_K1DI1, by="ID_T2") %>%
  left_join(Komponente_K1DI1, by = join_by(ID_K1DI1 == "ID_Self")) %>%
  drop_na() %>%
  mutate(Lieferdauer = Produktionsdatum.y - Produktionsdatum.x) %>%
  select(c(Fehlerhaft_Fahrleistung = Fehlerhaft_Fahrleistung.x, Betriebsdauer, Herstellernummer = Herstellernummer.x,
           ID_Self = ID_T2, ID_Parent = ID_K1DI1, Lieferdauer, Produktionsdatum = Produktionsdatum.x))

Einzelteil_T05 <- read_csv(".\\Data\\Einzelteil\\Einzelteil_T05.csv")
Einzelteil_T05 <- Einzelteil_T05 %>%
  #filter(!is.na(Fehlerhaft_Datum)) %>%
  select(c(Fehlerhaft_Fahrleistung.x, Herstellernummer.x, ID_T5 = ID_T05.x, Produktionsdatum.x)) %>%
  left_join(Bestandteile_Komponente_K1DI1, by="ID_T5") %>%
  left_join(Komponente_K1DI1, by = join_by(ID_K1DI1 == ID_Self)) %>%
  drop_na() %>%
  mutate(Lieferdauer = Produktionsdatum - Produktionsdatum.x) %>%
  select(c(Fehlerhaft_Fahrleistung = Fehlerhaft_Fahrleistung.x, Betriebsdauer, Herstellernummer = Herstellernummer.x,
           ID_Self = ID_T5, ID_Parent = ID_K1DI1, Lieferdauer, Produktionsdatum = Produktionsdatum.x))

Einzelteil_T06 <- read_csv(".\\Data\\Einzelteil\\Einzelteil_T06.csv")
Einzelteil_T06 <- Einzelteil_T06 %>%
  #filter(!is.na(Fehlerhaft_Datum)) %>%
  mutate(Produktionsdatum = as.Date(Produktionsdatum_Origin_01011970)) %>%
  select(c(Fehlerhaft_Fahrleistung, Herstellernummer, ID_T6 = ID_T06, Produktionsdatum)) %>%
  left_join(Bestandteile_Komponente_K1DI1, by="ID_T6") %>%
  left_join(Komponente_K1DI1, by = join_by(ID_K1DI1 == ID_Self)) %>%
  drop_na() %>%
  mutate(Lieferdauer = Produktionsdatum.y - Produktionsdatum.x) %>%
  select(c(Fehlerhaft_Fahrleistung = Fehlerhaft_Fahrleistung.x, Betriebsdauer, Herstellernummer = Herstellernummer.x,
           ID_Self = ID_T6, ID_Parent = ID_K1DI1, Lieferdauer, Produktionsdatum = Produktionsdatum.x))

Komponente_K1DI1 <- bind_rows(list(Komponente_K1DI1, Einzelteil_T01_K1DI1, Einzelteil_T02_K1DI1, Einzelteil_T05, Einzelteil_T06))
rm(Einzelteil_T01_K1DI1, Einzelteil_T02_K1DI1, Einzelteil_T05, Einzelteil_T06, Bestandteile_Komponente_K1DI1)

#--------------------------------------------------------------------------------------------------------------------
#K2LE1

Komponente_K2LE1_str <- readLines(paste(".\\Data\\Komponente\\Komponente_K2LE1.txt", sep = ""))
Komponente_K2LE1_str <- str_replace_all(Komponente_K2LE1_str, "", "\n") # replace linebreaks
Komponente_K2LE1_str <- str_replace_all(Komponente_K2LE1_str, "II", "\t") # replace tabs (coloumns)
tf <- tempfile()
writeLines(Komponente_K2LE1_str, tf)
Komponente_K2LE1 <- read_delim(tf, col_names = c("ID", "X1", "ID_Sitze", "Produktionsdatum", "Herstellernummer",
                                                 "Werksnummer", "Fehlerhaft", "Fehlerhaft_Datum", "Fehlerhaft_Fahrleistung"), skip = 1)
rm(Komponente_K2LE1_str, tf)

Komponente_K2LE1 <- Komponente_K2LE1 %>%
  filter(!is.na(Fehlerhaft_Datum)) %>%
  select(c(Fehlerhaft_Fahrleistung, Herstellernummer, ID_Sitze, Produktionsdatum)) %>%
  left_join(Bestandteile_Fahrzeuge_OEM1_Typ11, by = "ID_Sitze") %>%
  left_join(Fahrzeuge_OEM1_Typ11_Fehleranalyse, by = "ID_Fahrzeug") %>%
  drop_na() %>%
  left_join(Fahrzeuge_OEM1_Typ11, by = "ID_Fahrzeug") %>%
  mutate(Lieferdauer = Produktionsdatum.y - Produktionsdatum.x) %>%
  select(c(Fehlerhaft_Fahrleistung = Fehlerhaft_Fahrleistung.x, Betriebsdauer = days, Herstellernummer = Herstellernummer.x,
           ID_Self = ID_Sitze, ID_Parent = ID_Fahrzeug, Lieferdauer, Produktionsdatum = Produktionsdatum.x))           # ist sitze richtig?

Bestandteile_Komponente_K2LE1 <- read_csv2(".\\Data\\Komponente\\Bestandteile_Komponente_K2LE1.csv")

Einzelteil_T11_str <- readLines(paste(".\\Data\\Einzelteil\\Einzelteil_T11.txt", sep = ""))
Einzelteil_T11_str <- str_replace_all(Einzelteil_T11_str, "", "\n") # replace linebreaks
#Einzelteil_T11_str <- str_replace_all(Einzelteil_T11_str, "\t", ",")
tf <- tempfile()
writeLines(Einzelteil_T11_str, tf)
Einzelteil_T11 <- read_delim(tf, col_names = c("ID", "X1", "ID_T11", "Herstellernummer", "Werksnummer",
                                               "Fehlerhaft", "Fehlerhaft_Datum", "Fehlerhaft_Fahrleistung",  "Produktionsdatum"), skip = 1)
Einzelteil_T11_K2ST1 <- Einzelteil_T11
rm(Einzelteil_T11_str, tf)
Einzelteil_T11 <- Einzelteil_T11 %>%
  #filter(!is.na(Fehlerhaft_Datum)) %>%
  mutate(Produktionsdatum = as.Date(Produktionsdatum)) %>%
  select(c(Fehlerhaft_Fahrleistung, Herstellernummer, ID_T11, Produktionsdatum)) %>%
  left_join(Bestandteile_Komponente_K2LE1, by="ID_T11") %>%
  left_join(Komponente_K2LE1, by = join_by(ID_K2LE1 == ID_Self)) %>%
  drop_na() %>%
  mutate(Lieferdauer = Produktionsdatum.y - Produktionsdatum.x) %>%
  select(c(Fehlerhaft_Fahrleistung = Fehlerhaft_Fahrleistung.x, Betriebsdauer, Herstellernummer = Herstellernummer.x,
           ID_Self = ID_T11, ID_Parent = ID_K2LE1, Lieferdauer, Produktionsdatum = Produktionsdatum.x))


Einzelteil_T14 <- read_csv2(".\\Data\\Einzelteil\\Einzelteil_T14.csv")
Einzelteil_T14 <- Einzelteil_T14 %>%
  #filter(!is.na(Fehlerhaft_Datum)) %>%
  mutate(Produktionsdatum = as.Date(Produktionsdatum_Origin_01011970)) %>%
  select(c(Fehlerhaft_Fahrleistung, Herstellernummer, ID_T14, Produktionsdatum)) %>%
  left_join(Bestandteile_Komponente_K2LE1, by="ID_T14") %>%
  left_join(Komponente_K2LE1, by = join_by(ID_K2LE1 == ID_Self)) %>%
  drop_na() %>%
  mutate(Lieferdauer = Produktionsdatum.y - Produktionsdatum.x) %>%
  select(c(Fehlerhaft_Fahrleistung = Fehlerhaft_Fahrleistung.x, Betriebsdauer, Herstellernummer = Herstellernummer.x,
           ID_Self = ID_T14, ID_Parent = ID_K2LE1, Lieferdauer, Produktionsdatum = Produktionsdatum.x))

Einzelteil_T15 <- read_csv2(".\\Data\\Einzelteil\\Einzelteil_T15.csv")
Einzelteil_T15 <- Einzelteil_T15 %>%
  #filter(!is.na(Fehlerhaft_Datum)) %>%
  select(c(Fehlerhaft_Fahrleistung = Fehlerhaft_Fahrleistung.x, Herstellernummer = Herstellernummer.x, ID_T15 = ID_T15.x, Produktionsdatum = Produktionsdatum.x)) %>%
  left_join(Bestandteile_Komponente_K2LE1, by="ID_T15") %>%
  left_join(Komponente_K2LE1, by = join_by(ID_K2LE1 == ID_Self)) %>%
  drop_na() %>%
  mutate(Lieferdauer = Produktionsdatum.y - Produktionsdatum.x) %>%
  select(c(Fehlerhaft_Fahrleistung = Fehlerhaft_Fahrleistung.x, Betriebsdauer, Herstellernummer = Herstellernummer.x,
           ID_Self = ID_T15, ID_Parent = ID_K2LE1, Lieferdauer, Produktionsdatum = Produktionsdatum.x))

Komponente_K2LE1 <- bind_rows(list(Komponente_K2LE1, Einzelteil_T11, Einzelteil_T14, Einzelteil_T15))
rm(Einzelteil_T11, Einzelteil_T14, Einzelteil_T15, Bestandteile_Komponente_K2LE1)

#--------------------------------------------------------------------------------------------------------------------
# Komponente_K2ST1

Komponente_K2ST1 <- read_delim(".\\Data\\Komponente\\Komponente_K2ST1.txt", delim="|",
                               col_names = c("ID", "X1", "ID_Sitze", "Produktionsdatum", "Herstellernummer", "Werksnummer",
                                             "Fehlerhaft", "Fehlerhaft_Datum", "Fehlerhaft_Fahrleistung"), skip = 1)
Komponente_K2ST1 <- Komponente_K2ST1 %>%
  filter(!is.na(Fehlerhaft_Datum)) %>%
  select(c(Fehlerhaft_Fahrleistung, Herstellernummer, ID_Sitze, Produktionsdatum)) %>%
  left_join(Bestandteile_Fahrzeuge_OEM1_Typ11, by = "ID_Sitze") %>%
  left_join(Fahrzeuge_OEM1_Typ11_Fehleranalyse, by = "ID_Fahrzeug") %>%
  drop_na() %>%
  left_join(Fahrzeuge_OEM1_Typ11, by = "ID_Fahrzeug") %>%
  mutate(Lieferdauer = Produktionsdatum.y - Produktionsdatum.x) %>%
  select(c(Fehlerhaft_Fahrleistung = Fehlerhaft_Fahrleistung.x, Betriebsdauer = days, Herstellernummer = Herstellernummer.x,
           ID_Self = ID_Sitze, ID_Parent = ID_Fahrzeug, Lieferdauer, Produktionsdatum = Produktionsdatum.x))

Bestandteile_Komponente_K2ST1 <- read_csv2(".\\Data\\Komponente\\Bestandteile_Komponente_K2ST1.csv")

Einzelteil_T11_K2ST1 <- Einzelteil_T11_K2ST1 %>%
  #filter(!is.na(Fehlerhaft_Datum)) %>%
  mutate(Produktionsdatum = as.Date(Produktionsdatum)) %>%
  select(c(Fehlerhaft_Fahrleistung, Herstellernummer, ID_T11, Produktionsdatum)) %>%
  left_join(Bestandteile_Komponente_K2ST1, by="ID_T11") %>%
  left_join(Komponente_K2ST1, by = join_by(ID_K2ST1 == ID_Self)) %>%
  drop_na() %>%
  mutate(Lieferdauer = Produktionsdatum.y - Produktionsdatum.x) %>%
  select(c(Fehlerhaft_Fahrleistung = Fehlerhaft_Fahrleistung.x, Betriebsdauer, Herstellernummer = Herstellernummer.x,
           ID_Self = ID_T11, ID_Parent = ID_K2ST1, Lieferdauer, Produktionsdatum = Produktionsdatum.x))

Einzelteil_T12 <- read_csv2(".\\Data\\Einzelteil\\Einzelteil_T12.csv")
Einzelteil_T12 <- Einzelteil_T12 %>%
  #filter(!is.na(Fehlerhaft_Datum)) %>%
  select(c(Fehlerhaft_Fahrleistung = Fehlerhaft_Fahrleistung.x, Herstellernummer = Herstellernummer.x, ID_T12 = ID_T12.x, Produktionsdatum = Produktionsdatum.x)) %>%
  left_join(Bestandteile_Komponente_K2ST1, by="ID_T12") %>%
  left_join(Komponente_K2ST1, by = join_by(ID_K2ST1 == ID_Self)) %>%
  drop_na() %>%
  mutate(Lieferdauer = Produktionsdatum.y - Produktionsdatum.x) %>%
  select(c(Fehlerhaft_Fahrleistung = Fehlerhaft_Fahrleistung.x, Betriebsdauer, Herstellernummer = Herstellernummer.x,
           ID_Self = ID_T12, ID_Parent = ID_K2ST1, Lieferdauer, Produktionsdatum = Produktionsdatum.x))

Einzelteil_T13 <- read_csv2(".\\Data\\Einzelteil\\Einzelteil_T13.csv")
Einzelteil_T13 <- Einzelteil_T13 %>%
  mutate(Produktionsdatum = as.Date(Produktionsdatum_Origin_01011970)) %>%
  select(c(Fehlerhaft_Fahrleistung, Herstellernummer, ID_T13, Produktionsdatum)) %>%
  left_join(Bestandteile_Komponente_K2ST1, by="ID_T13") %>%
  left_join(Komponente_K2ST1, by = join_by(ID_K2ST1 == ID_Self)) %>%
  drop_na() %>%
  mutate(Lieferdauer = Produktionsdatum.y - Produktionsdatum.x) %>%
  select(c(Fehlerhaft_Fahrleistung = Fehlerhaft_Fahrleistung.x, Betriebsdauer, Herstellernummer = Herstellernummer.x,
           ID_Self = ID_T13, ID_Parent = ID_K2ST1, Lieferdauer, Produktionsdatum = Produktionsdatum.x))

Komponente_K2ST1 <- bind_rows(list(Komponente_K2ST1, Einzelteil_T11_K2ST1, Einzelteil_T12, Einzelteil_T13))
rm(Einzelteil_T11_K2ST1, Einzelteil_T12, Einzelteil_T13, Bestandteile_Komponente_K2ST1)

#--------------------------------------------------------------------------------------------------------------------
# Komponente_K3AG1

Komponente_K3AG1 <- read_csv(".\\Data\\Komponente\\Komponente_K3AG1.csv")
Komponente_K3AG1 <- Komponente_K3AG1 %>%
  filter(!is.na(Fehlerhaft_Datum.x)) %>%
  #filter(Produktionsdatum.x >= "2013-01-01" & Produktionsdatum.x <= "2015-12-31") %>%
  select(c(Fehlerhaft_Fahrleistung.x, Herstellernummer.x, ID_Schaltung.x, Produktionsdatum.x)) %>%
  left_join(Bestandteile_Fahrzeuge_OEM1_Typ11, by = join_by(ID_Schaltung.x == ID_Schaltung)) %>%
  left_join(Fahrzeuge_OEM1_Typ11_Fehleranalyse, by = "ID_Fahrzeug") %>%
  drop_na() %>%
  left_join(Fahrzeuge_OEM1_Typ11, by="ID_Fahrzeug") %>%
  mutate(Lieferdauer = Produktionsdatum - Produktionsdatum.x) %>%
  select(c(Fehlerhaft_Fahrleistung = Fehlerhaft_Fahrleistung.x, Betriebsdauer = days, Herstellernummer = Herstellernummer.x,
           ID_Self = ID_Schaltung.x, ID_Parent = ID_Fahrzeug, Lieferdauer, Produktionsdatum = Produktionsdatum.x))

Bestandteile_Komponente_K3AG1 <- read_csv2(".\\Data\\Komponente\\Bestandteile_Komponente_K3AG1.csv")

Einzelteil_T21 <- read_csv2(".\\Data\\Einzelteil\\Einzelteil_T21.csv")
Einzelteil_T21_K3SG1 <- Einzelteil_T21
Einzelteil_T21 <- Einzelteil_T21 %>%
  #filter(!is.na(Fehlerhaft_Datum)) %>%
  mutate(Produktionsdatum = as.Date(Produktionsdatum_Origin_01011970)) %>%
  select(c(Fehlerhaft_Fahrleistung, Herstellernummer, ID_T21, Produktionsdatum)) %>%
  left_join(Bestandteile_Komponente_K3AG1, by="ID_T21") %>%
  left_join(Komponente_K3AG1, by = join_by(ID_K3AG1 == ID_Self)) %>%
  drop_na() %>%
  mutate(Lieferdauer = Produktionsdatum.y - Produktionsdatum.x) %>%
  select(c(Fehlerhaft_Fahrleistung = Fehlerhaft_Fahrleistung.x, Betriebsdauer, Herstellernummer = Herstellernummer.x,
           ID_Self = ID_T21, ID_Parent = ID_K3AG1, Lieferdauer, Produktionsdatum = Produktionsdatum.x))

Einzelteil_T24_str <- readLines(paste(".\\Data\\Einzelteil\\Einzelteil_T24.txt", sep = ""))
Einzelteil_T24_str <- str_replace_all(Einzelteil_T24_str, "", "\n") # replace linebreaks
Einzelteil_T24_str <- str_replace_all(Einzelteil_T24_str, "  ", ",")
tf <- tempfile()
writeLines(Einzelteil_T24_str, tf)
Einzelteil_T24 <- read_delim(tf, col_names = c("ID", "X1", "ID_T24", "Produktionsdatum", "Herstellernummer", "Werksnummer",
                                               "Fehlerhaft", "Fehlerhaft_Datum", "Fehlerhaft_Fahrleistung"), skip = 1)
rm(Einzelteil_T24_str, tf)
Einzelteil_T24 <- Einzelteil_T24 %>%
  #filter(!is.na(Fehlerhaft_Datum)) %>%
  select(c(Fehlerhaft_Fahrleistung, Herstellernummer, ID_T24, Produktionsdatum)) %>%
  left_join(Bestandteile_Komponente_K3AG1, by="ID_T24") %>%
  left_join(Komponente_K3AG1, by = join_by(ID_K3AG1 == ID_Self)) %>%
  drop_na() %>%
  mutate(Lieferdauer = Produktionsdatum.y - Produktionsdatum.x) %>%
  select(c(Fehlerhaft_Fahrleistung = Fehlerhaft_Fahrleistung.x, Betriebsdauer, Herstellernummer = Herstellernummer.x,
           ID_Self = ID_T24, ID_Parent = ID_K3AG1, Lieferdauer, Produktionsdatum = Produktionsdatum.x))


Einzelteil_T25 <- read_csv(".\\Data\\Einzelteil\\Einzelteil_T25.csv")
Einzelteil_T25 <- Einzelteil_T25 %>%
  #filter(!is.na(Fehlerhaft_Datum)) %>%
  mutate(Produktionsdatum = as.Date(Produktionsdatum_Origin_01011970)) %>%
  select(c(Fehlerhaft_Fahrleistung, Herstellernummer, ID_T25, Produktionsdatum)) %>%
  left_join(Bestandteile_Komponente_K3AG1, by="ID_T25") %>%
  left_join(Komponente_K3AG1, by = join_by(ID_K3AG1 == ID_Self)) %>%
  drop_na() %>%
  mutate(Lieferdauer = Produktionsdatum.y - Produktionsdatum.x) %>%
  select(c(Fehlerhaft_Fahrleistung = Fehlerhaft_Fahrleistung.x, Betriebsdauer, Herstellernummer = Herstellernummer.x,
           ID_Self = ID_T25, ID_Parent = ID_K3AG1, Lieferdauer, Produktionsdatum = Produktionsdatum.x))

Komponente_K3AG1 <- bind_rows(list(Komponente_K3AG1, Einzelteil_T21, Einzelteil_T24, Einzelteil_T25))
rm(Einzelteil_T21, Einzelteil_T24, Einzelteil_T25, Bestandteile_Komponente_K3AG1)

#--------------------------------------------------------------------------------------------------------------------
# Komponente_K3SG1

Komponente_K3SG1 <- read_csv(".\\Data\\Komponente\\Komponente_K3SG1.csv")
Komponente_K3SG1 <- Komponente_K3SG1 %>%
  filter(!is.na(Fehlerhaft_Datum.x)) %>%
  #filter(Produktionsdatum.x >= "2013-01-01" & Produktionsdatum.x <= "2015-12-31") %>%
  select(c(Fehlerhaft_Fahrleistung.x, Herstellernummer.x, ID_Schaltung.x, Produktionsdatum.x)) %>%
  left_join(Bestandteile_Fahrzeuge_OEM1_Typ11, by = join_by(ID_Schaltung.x == ID_Schaltung)) %>%
  left_join(Fahrzeuge_OEM1_Typ11_Fehleranalyse, by = "ID_Fahrzeug") %>%
  drop_na() %>%
  left_join(Fahrzeuge_OEM1_Typ11, by="ID_Fahrzeug") %>%
  mutate(Lieferdauer = Produktionsdatum - Produktionsdatum.x) %>%
  select(c(Fehlerhaft_Fahrleistung = Fehlerhaft_Fahrleistung.x, Betriebsdauer = days, Herstellernummer = Herstellernummer.x,
           ID_Self = ID_Schaltung.x, ID_Parent = ID_Fahrzeug, Lieferdauer, Produktionsdatum = Produktionsdatum.x))

Bestandteile_Komponente_K3SG1 <- read_csv2(".\\Data\\Komponente\\Bestandteile_Komponente_K3SG1.csv")

Einzelteil_T21_K3SG1 <- Einzelteil_T21_K3SG1 %>%
  #filter(!is.na(Fehlerhaft_Datum)) %>%
  mutate(Produktionsdatum = as.Date(Produktionsdatum_Origin_01011970)) %>%
  select(c(Fehlerhaft_Fahrleistung, Herstellernummer, ID_T21, Produktionsdatum)) %>%
  left_join(Bestandteile_Komponente_K3SG1, by="ID_T21") %>%
  left_join(Komponente_K3SG1, by = join_by(ID_K3SG1 == ID_Self)) %>%
  drop_na() %>%
  mutate(Lieferdauer = Produktionsdatum.y - Produktionsdatum.x) %>%
  select(c(Fehlerhaft_Fahrleistung = Fehlerhaft_Fahrleistung.x, Betriebsdauer, Herstellernummer = Herstellernummer.x,
           ID_Self = ID_T21, ID_Parent = ID_K3SG1, Lieferdauer, Produktionsdatum = Produktionsdatum.x))

Einzelteil_T22_str <- readLines(paste(".\\Data\\Einzelteil\\Einzelteil_T22.txt", sep = ""))
Einzelteil_T22_str <- str_replace_all(Einzelteil_T22_str, "NA\"", "NA\n\"") # replace linebreaks
#Einzelteil_T22_str <- str_replace_all(Einzelteil_T22_str, "  ", ",") # replace linebreaks
tf <- tempfile()
writeLines(Einzelteil_T22_str, tf)
Einzelteil_T22 <- read_delim(tf, col_names = c("ID", "X1", "ID_T22", "Produktionsdatum", "Herstellernummer", "Werksnummer",
                                               "Fehlerhaft", "Fehlerhaft_Datum", "Fehlerhaft_Fahrleistung"), skip = 1)
rm(Einzelteil_T22_str, tf)
Einzelteil_T22 <- Einzelteil_T22 %>%
  #filter(!is.na(Fehlerhaft_Datum)) %>%
  select(c(Fehlerhaft_Fahrleistung, Herstellernummer, ID_T22, Produktionsdatum)) %>%
  left_join(Bestandteile_Komponente_K3SG1, by="ID_T22") %>%
  left_join(Komponente_K3SG1, by = join_by(ID_K3SG1 == ID_Self)) %>%
  drop_na() %>%
  mutate(Lieferdauer = Produktionsdatum.y - Produktionsdatum.x) %>%
  select(c(Fehlerhaft_Fahrleistung = Fehlerhaft_Fahrleistung.x, Betriebsdauer, Herstellernummer = Herstellernummer.x,
           ID_Self = ID_T22, ID_Parent = ID_K3SG1, Lieferdauer, Produktionsdatum = Produktionsdatum.x))

Einzelteil_T23 <- read_csv2(".\\Data\\Einzelteil\\Einzelteil_T23.csv")
Einzelteil_T23 <- Einzelteil_T23 %>%
  #filter(!is.na(Fehlerhaft_Datum)) %>%
  select(c(Fehlerhaft_Fahrleistung = Fehlerhaft_Fahrleistung.x, Herstellernummer = Herstellernummer.x, ID_T23 = ID_T23.x, Produktionsdatum = Produktionsdatum.x)) %>%
  left_join(Bestandteile_Komponente_K3SG1, by="ID_T23") %>%
  left_join(Komponente_K3SG1, by = join_by(ID_K3SG1 == ID_Self)) %>%
  drop_na() %>%
  mutate(Lieferdauer = Produktionsdatum.y - Produktionsdatum.x) %>%
  select(c(Fehlerhaft_Fahrleistung = Fehlerhaft_Fahrleistung.x, Betriebsdauer, Herstellernummer = Herstellernummer.x,
           ID_Self = ID_T23, ID_Parent = ID_K3SG1, Lieferdauer, Produktionsdatum = Produktionsdatum.x))

Komponente_K3SG1 <- bind_rows(list(Komponente_K3SG1, Einzelteil_T21_K3SG1, Einzelteil_T22, Einzelteil_T23))
rm(Einzelteil_T21_K3SG1, Einzelteil_T22, Einzelteil_T23, Bestandteile_Komponente_K3SG1)
#--------------------------------------------------------------------------------------------------------------------
# Komponente_K4

Komponente_K4 <- read_csv2(".\\Data\\Komponente\\Komponente_K4.csv")
Komponente_K4 <- Komponente_K4 %>%
  filter(!is.na(Fehlerhaft_Datum.x)) %>%
  select(c(Fehlerhaft_Fahrleistung.x, Herstellernummer.x, ID_Karosserie.x, Produktionsdatum.x)) %>%
  left_join(Bestandteile_Fahrzeuge_OEM1_Typ11, by = join_by(ID_Karosserie.x == ID_Karosserie)) %>%
  left_join(Fahrzeuge_OEM1_Typ11_Fehleranalyse, by = "ID_Fahrzeug") %>%
  filter(!is.na(Fehlerhaft_Fahrleistung)) %>%
  left_join(Fahrzeuge_OEM1_Typ11, by="ID_Fahrzeug") %>%
  mutate(Lieferdauer = Produktionsdatum - Produktionsdatum.x) %>%
  select(c(Fehlerhaft_Fahrleistung = Fehlerhaft_Fahrleistung.x, Betriebsdauer = days, Herstellernummer = Herstellernummer.x,
           ID_Self = ID_Karosserie.x, ID_Parent = ID_Fahrzeug, Lieferdauer, Produktionsdatum = Produktionsdatum.x))

Bestandteile_Komponente_K4 <- read_csv2(".\\Data\\Komponente\\Bestandteile_Komponente_K4.csv")

Einzelteil_T30 <- read_csv(".\\Data\\Einzelteil\\Einzelteil_T30.csv")
Einzelteil_T30 <- Einzelteil_T30 %>%
  #filter(!is.na(Fehlerhaft_Datum)) %>%
  select(c(Fehlerhaft_Fahrleistung = Fehlerhaft_Fahrleistung.x, Herstellernummer = Herstellernummer.x, ID_T30 = ID_T30.x, Produktionsdatum = Produktionsdatum.x)) %>%
  left_join(Bestandteile_Komponente_K4, by="ID_T30") %>%
  left_join(Komponente_K4, by = join_by(ID_K4 == ID_Self)) %>%
  drop_na() %>%
  mutate(Lieferdauer = Produktionsdatum.y - Produktionsdatum.x) %>%
  select(c(Fehlerhaft_Fahrleistung = Fehlerhaft_Fahrleistung.x, Betriebsdauer, Herstellernummer = Herstellernummer.x,
           ID_Self = ID_T30, ID_Parent = ID_K4, Lieferdauer, Produktionsdatum = Produktionsdatum.x))

Einzelteil_T31_str <- readLines(paste(".\\Data\\Einzelteil\\Einzelteil_T31.txt", sep = ""))
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
  select(c(Fehlerhaft_Fahrleistung, Herstellernummer, ID_T31, Produktionsdatum)) %>%
  left_join(Bestandteile_Komponente_K4, by="ID_T31") %>%
  left_join(Komponente_K4, by = join_by(ID_K4 == ID_Self)) %>%
  drop_na() %>%
  mutate(Lieferdauer = Produktionsdatum.y - Produktionsdatum.x) %>%
  select(c(Fehlerhaft_Fahrleistung = Fehlerhaft_Fahrleistung.x, Betriebsdauer, Herstellernummer = Herstellernummer.x,
           ID_Self = ID_T31, ID_Parent = ID_K4, Lieferdauer, Produktionsdatum = Produktionsdatum.x))

Einzelteil_T32 <- read_csv2(".\\Data\\Einzelteil\\Einzelteil_T32.csv")
Einzelteil_T32 <- Einzelteil_T32 %>%
  #filter(!is.na(Fehlerhaft_Datum)) %>%
  select(c(Fehlerhaft_Fahrleistung = Fehlerhaft_Fahrleistung.x, Herstellernummer = Herstellernummer.x, ID_T32 = ID_T32.x, Produktionsdatum = Produktionsdatum.x)) %>%
  left_join(Bestandteile_Komponente_K4, by="ID_T32") %>%
  left_join(Komponente_K4, by = join_by(ID_K4 == ID_Self)) %>%
  drop_na() %>%
  mutate(Lieferdauer = Produktionsdatum.y - Produktionsdatum.x) %>%
  select(c(Fehlerhaft_Fahrleistung = Fehlerhaft_Fahrleistung.x, Betriebsdauer, Herstellernummer = Herstellernummer.x,
           ID_Self = ID_T32, ID_Parent = ID_K4, Lieferdauer, Produktionsdatum = Produktionsdatum.x))

Komponente_K4 <- bind_rows(list(Komponente_K4, Einzelteil_T30, Einzelteil_T31, Einzelteil_T32))
rm(Einzelteil_T30, Einzelteil_T31, Einzelteil_T32, Bestandteile_Komponente_K4)

result <- bind_rows(list(Komponente_K1BE1, Komponente_K1DI1 ,Komponente_K2LE1, Komponente_K2ST1, Komponente_K3AG1,
                          Komponente_K3SG1, Komponente_K4)) #%>%
            #filter(Produktionsdatum >= "2013-01-01" & Produktionsdatum <= "2015-12-31")

write_csv(result, "Datensatz.csv")

#Grouping and Testing for Plot
#grouping will be relevant for shiny app
Herstellerdaten <- Komponente_K1BE1 %>%
  group_by(Herstellernummer) %>%
  summarise(absolut=n())

plot_data_Laufleistung <- Komponente_K1BE1 %>%
  mutate(cuts = cut(Fehlerhaft_Fahrleistung.x, seq(27000, 34000, length.out = 10))) %>%
  group_by(cuts, Herstellernummer.x) %>%
  summarise(n=n())

plot_data_Laufleistung <- plot_data_Laufleistung %>%
  left_join(Herstellerdaten, by = "Herstellernummer") %>%
  mutate(relativ = n/absolut)


ggplot(data=plot_data_Laufleistung, aes(x=cuts, y=cumsum(relativ), group=Herstellernummer)) +
  geom_line()
