# install.packages("tidyverse")
library(readr)
library(dplyr)
library(vroom)
library(stringr)
library(ggplot2)
# data2 <- read_csv("D:\\RStudio\\Binning\\data.csv ")
# results <- read_delim('results.txt', delim = '   ')
# results <- with_edition(1, read_delim("results.txt", delim = " "))

# Example for importing txt data
my_txt <- readLines(paste(".\\Data\\Einzelteil\\Einzelteil_T20.txt", sep = ""))
# txt <- str_trunc(my_txt, 1000)
txt <- str_replace_all(my_txt, "\" \"", "\"\n\"") # replace linebreaks
txt <- str_replace_all(txt, "[|]", "")
txt <- str_replace_all(txt, "[[:space:]]{3}", "\t")
tf <- tempfile()
writeLines(txt, tf)
Einzelteile_T20 <- read_delim(tf, col_names = c("X1", "ID","ID_T20", "Herstellernummer", "Werksnummer", "Fehlerhaft",
                                                "Fehlerhaft_Datum", "Fehlerhaft_Fahrleistung", "Produktionsdatum"), skip = 1)




# -----------------------------------
# Fahrzeug allgemein
Fahrzeuge_OEM1_Typ11 <- read_csv(".\\Data\\Fahrzeug\\Fahrzeuge_OEM1_Typ11.csv")
Fahrzeuge_OEM1_Typ11_Fehleranalyse <- read_csv(".\\Data\\Fahrzeug\\Fahrzeuge_OEM1_Typ11_Fehleranalyse.csv")
Bestandteile_Fahrzeuge_OEM1_Typ11 <- read_csv2(".\\Data\\Fahrzeug\\Bestandteile_Fahrzeuge_OEM1_Typ11.csv ")

# ------------------------------------
# Karosserie
Komponente_K4 <- read_csv2(".\\Data\\Komponente\\Komponente_K4.csv")
Bestandteile_Komponente_K4 <- read_csv2(".\\Data\\Komponente\\Bestandteile_Komponente_K4.csv")
Einzelteil_T30 <- read_csv(".\\Data\\Einzelteil\\Einzelteil_T30.csv")
Einzelteil_T31 <- read_delim_chunked(".\\Data\\Einzelteil\\Einzelteil_T31.txt", delim = "  ")
Einzelteil_T31 <- with_edition(1, read_delim(".\\Data\\Einzelteil\\Einzelteil_T31.txt", delim = "  "))

Einzelteil_T32 <- read_csv2(".\\Data\\Einzelteil\\Einzelteil_T32.csv")

# --------------------------------------
# Schaltung
# Schaltgetriebe
Komponente_K3SG1 <- read_csv(".\\Data\\Komponente\\Komponente_K3SG1.csv")
Bestandteile_Komponente_K3AG1 <- read_csv2(".\\Data\\Komponente\\Bestandteile_Komponente_K3AG1.csv")
Einzelteil_T21 <- read_csv2(".\\Data\\Einzelteil\\Einzelteil_T21.csv")
# Einzelteil_T22 <- read_tsv(".\\Data\\Einzelteil\\Einzelteil_T22.txt")
Einzelteil_T23 <- read_csv2(".\\Data\\Einzelteil\\Einzelteil_T23.csv")

# Automatikgetriebe
Komponente_K3AG1 <- read_csv(".\\Data\\Komponente\\Komponente_K3AG1.csv")
Bestandteile_Komponente_K3SG1 <- read_csv2(".\\Data\\Komponente\\Bestandteile_Komponente_K3SG1.csv")
# Einzelteil_T21.csv Doppelt
# !!! Einzelteil_T24 <- read_tsv(".\\Data\\Einzelteil\\Einzelteil_T24.txt")
Einzelteil_T25 <- read_csv(".\\Data\\Einzelteil\\Einzelteil_T25.csv")

# Sitze
Komponente_K2LE1 <- read_csv2(".\\Data\\Komponente\\Komponente_K2LE1.csv")

Bestandteile_Komponente_K2LE1 <- read_csv2(".\\Data\\Komponente\\Bestandteile_Komponente_K2LE1.csv")
Einzelteil_T11.txt <- read_tsv(".\\Data\\Einzelteil\\Einzelteil_T11.txt")

Einzelteil_T14 <- read_csv2(".\\Data\\Einzelteil\\Einzelteil_T14.csv")
Einzelteil_T15 <- read_csv2(".\\Data\\Einzelteil\\Einzelteil_T15.csv")
# Bestandteile_Komponente_K2LE1 Doppelt
# Einzelteil_T11.txt Doppelt
Einzelteil_T12 <- read_csv2(".\\Data\\Einzelteil\\Einzelteil_T12.csv")
Einzelteil_T13 <- read_csv2(".\\Data\\Einzelteil\\Einzelteil_T13.csv")

# Motor
Komponente_K1BE1 <- read_csv(".\\Data\\Komponente\\Komponente_K1BE1.csv")
Bestandteile_Komponente_K1BE1 <- read_csv2(".\\Data\\Komponente\\Bestandteile_Komponente_K1BE1.csv")
# !! Einzelteil_T01 <- read_delim(".\\Data\\Einzelteil\\Einzelteil_T01.txt", delim = " | | ")
# !! Einzelteil_T02 <- read_tsv(".\\Data\\Einzelteil\\Einzelteil_T01.txt")
# !! Einzelteil_T03 <- read_delim(".\\Data\\Einzelteil\\Einzelteil_T01.txt", delim = "|")
Einzelteil_T04 <- read_csv2(".\\Data\\Einzelteil\\Einzelteil_T04.csv")

Komponente_K1DI1 <- read_csv(".\\Data\\Komponente\\Komponente_K1DI1.csv")
Bestandteile_Komponente_K1DI1 <- read_csv2(".\\Data\\Komponente\\Bestandteile_Komponente_K1DI1.csv")
# Einzelteil_T01.txt Doppelt
# Einzelteil_T02.txt Doppelt
Einzelteil_T05 <- read_csv(".\\Data\\Einzelteil\\Einzelteil_T05.csv")
Einzelteil_T06 <- read_csv(".\\Data\\Einzelteil\\Einzelteil_T06.csv")

# -----------------------------------------------------------------------------------
# Filtern der Daten nach Teilen die fehlerhaft sind
# -----------------------------------
# Fahrzeug allgemein
Fahrzeuge_OEM1_Typ11_sorted <- Fahrzeuge_OEM1_Typ11 %>%
  filter(!is.na(Fehlerhaft_Datum))
Fahrzeuge_OEM1_Typ11_Fehleranalyse_sorted <- Fahrzeuge_OEM1_Typ11_Fehleranalyse %>%
  filter(!is.na(Fehlerhaft_Datum))

# ------------------------------------
# Karosserie
Komponente_K4_sorted <- Komponente_K4 %>%
  filter(!is.na(Fehlerhaft_Datum))
Einzelteil_T30_sorted <-Einzelteil_T30 %>%
  filter(!is.na(Fehlerhaft_Datum))
# Einzelteil_T31.txt
Einzelteil_T32_sorted <- Einzelteil_T32 %>%
  filter(!is.na(Fehlerhaft_Datum.x))

# --------------------------------------
# Schaltung
# Schaltgetriebe
Komponente_K3SG1_sorted <- Komponente_K3SG1 %>%
  filter(!is.na(Fehlerhaft_Datum.x))

Einzelteil_T21_sorted <- Einzelteil_T21 %>%
  filter(!is.na(Fehlerhaft_Datum))
# Einzelteil_T22.txt
Einzelteil_T23_sorted <- Einzelteil_T23 %>%
  filter(!is.na(Fehlerhaft_Datum.x))

# Automatikgetriebe
Komponente_K3AG1_sorted <- Komponente_K3AG1 %>%
  filter(!is.na(Fehlerhaft_Datum.x))
# Einzelteil_T21.csv Doppelt
# Einzelteil_T24.txt
Einzelteil_T25_sorted <- Einzelteil_T25 %>%
  filter(!is.na(Fehlerhaft_Datum))

# Sitze
# Komponente_K2LE1s.txt
# Einzelteil_T11.txt
Einzelteil_T14_sorted <- Einzelteil_T14 %>%
  filter(!is.na(Fehlerhaft_Datum))
Einzelteil_T15_sorted <- Einzelteil_T15 %>%
  filter(!is.na(Fehlerhaft_Datum.x))
# Komponente_K2LE1s.txt
# Bestandteile_Komponente_K2LE1 Doppelt
# Einzelteil_T11.txt Doppelt
Einzelteil_T12_sorted <- Einzelteil_T12 %>%
  filter(!is.na(Fehlerhaft_Datum.x))
Einzelteil_T13_sorted <- Einzelteil_T13 %>%
  filter(!is.na(Fehlerhaft_Datum))

# Motor
Komponente_K1BE1_sorted <- Komponente_K1BE1 %>%
  filter(!is.na(Fehlerhaft_Datum))
# Einzelteil_T01.txt
# Einzelteil_T02.txt
# Einzelteil_T03.txt
Einzelteil_T04_sorted <- Einzelteil_T04 %>%
  filter(!is.na(Fehlerhaft_Datum))

Komponente_K1DI1_sorted <- Komponente_K1DI1 %>%
  filter(!is.na(Fehlerhaft_Datum.x))
# Einzelteil_T01.txt Doppelt
# Einzelteil_T02.txt Doppelt
Einzelteil_T05_sorted <- Einzelteil_T05 %>%
  filter(!is.na(Fehlerhaft_Datum.x))
Einzelteil_T06_sorted <- Einzelteil_T06 %>%
  filter(!is.na(Fehlerhaft_Datum))

# Nur zum Testen, muss später rausgenommen werden ---------
Fahrzeuge_OEM1_Typ11_Fehleranalyse <- read_csv(".\\Data\\Fahrzeug\\Fahrzeuge_OEM1_Typ11_Fehleranalyse.csv")
Bestandteile_Fahrzeuge_OEM1_Typ11 <- read_csv2(".\\Data\\Fahrzeug\\Bestandteile_Fahrzeuge_OEM1_Typ11.csv ")
Fahrzeuge_OEM1_Typ11 <- read_csv(".\\Data\\Fahrzeug\\Fahrzeuge_OEM1_Typ11.csv")
# --------------------------------------------------------


#Join Tables to get Betriebsstunden
#a LOT of na's, reduces values from 200k to 17k, problem?


Komponente_K1BE1 <- read_csv(".\\Data\\Komponente\\Komponente_K1BE1.csv")
Komponente_K1BE1 <- Komponente_K1BE1 %>%
  filter(!is.na(Fehlerhaft_Datum)) %>%
  mutate(Produktionsdatum = as.Date(Produktionsdatum_Origin_01011970))%>%
  select(c(Fehlerhaft_Fahrleistung,X1, Herstellernummer, ID_Motor, Produktionsdatum)) %>%
  filter(Produktionsdatum >= "2013-01-01" & Produktionsdatum <= "2015-12-31") %>%

  left_join(Bestandteile_Fahrzeuge_OEM1_Typ11, by = "ID_Motor") %>% #id fahrzeug
  left_join(Fahrzeuge_OEM1_Typ11_Fehleranalyse, by = "ID_Fahrzeug") %>% #days
  filter(!is.na(Fehlerhaft_Fahrleistung.y)) %>%
  left_join(Fahrzeuge_OEM1_Typ11, by="ID_Fahrzeug") %>%
  mutate(Lieferdauer = Produktionsdatum.y - Produktionsdatum.x) %>%
  select(c(Fehlerhaft_Fahrleistung = Fehlerhaft_Fahrleistung.x, Betriebsdauer = days,
           Herstellernummer = Herstellernummer.x, ID_Komponente = ID_Motor, ID_Fahrzeug, Lieferdauer,
           Produktionsdatum = Produktionsdatum.x))



Komponente_K1DI1 <- read_csv(".\\Data\\Komponente\\Komponente_K1DI1.csv")
Komponente_K1DI1 <- Komponente_K1DI1 %>%
  filter(!is.na(Fehlerhaft_Datum.x)) %>%
  select(c(Fehlerhaft_Fahrleistung.x,X1, Herstellernummer.x, ID_Motor.x, Produktionsdatum.x)) %>%
  left_join(Bestandteile_Fahrzeuge_OEM1_Typ11, by = join_by(ID_Motor.x == ID_Motor)) %>%
  left_join(Fahrzeuge_OEM1_Typ11_Fehleranalyse, by = "ID_Fahrzeug") %>%

  filter(!is.na(Fehlerhaft_Fahrleistung)) %>%
  left_join(Fahrzeuge_OEM1_Typ11, by="ID_Fahrzeug") %>%
  mutate(Lieferdauer = Produktionsdatum - Produktionsdatum.x) %>%
  select(c(Fehlerhaft_Fahrleistung = Fehlerhaft_Fahrleistung.x, Betriebsdauer = days, Herstellernummer = Herstellernummer.x,
           ID_Komponente = ID_Motor.x, ID_Fahrzeug, Lieferdauer))

# NAs in Betriebsdauer...
my_txt <- readLines(paste(".\\Data\\Komponente\\Komponente_K2LE1.txt", sep = ""))
txt <- str_replace_all(my_txt, "", "\n") # replace linebreaks
txt <- str_replace_all(txt, "II", "\t") # replace tabs (coloumns)
tf <- tempfile()
writeLines(txt, tf)
Komponente_K2LE1 <- read_delim(tf, col_names = c("ID", "X1", "ID_Sitze", "Produktionsdatum", "Herstellernummer",
                                                 "Werksnummer", "Fehlerhaft", "Fehlerhaft_Datum", "Fehlerhaft_Fahrleistung"), skip = 1)
Komponente_K2LE1 <- Komponente_K2LE1 %>%
  filter(!is.na(Fehlerhaft_Datum)) %>%
  select(c(Fehlerhaft_Fahrleistung, Herstellernummer, ID_Sitze, Produktionsdatum)) %>%
  left_join(Bestandteile_Fahrzeuge_OEM1_Typ11, by = "ID_Sitze") %>%
  filter(!is.na(ID_Fahrzeug)) %>%                                           # Da manche ID_Sitze nur ID_Fahrzeug aus OEM1_Typ12 oä haben
  left_join(Fahrzeuge_OEM1_Typ11_Fehleranalyse, by = "ID_Fahrzeug") %>%
  filter(!is.na(Fehlerhaft_Fahrleistung.x)) %>%
  left_join(Fahrzeuge_OEM1_Typ11, by = "ID_Fahrzeug") %>%
  mutate(Lieferdauer = Produktionsdatum.y - Produktionsdatum.x) %>%
  select(c(Fehlerhaft_Fahrleistung = Fehlerhaft_Fahrleistung.x, Betriebsdauer = days, Herstellernummer = Herstellernummer.x,
           ID_Komponente = ID_Sitze, ID_Fahrzeug, Lieferdauer))           # ist sitze richtig?


# NAs in Betriebsdauer...
Komponente_K2ST1 <- read_delim(".\\Data\\Komponente\\Komponente_K2ST1.txt", delim="|",
                               col_names = c("ID", "X1", "ID_Sitze", "Produktionsdatum", "Herstellernummer", "Werksnummer",
                                             "Fehlerhaft", "Fehlerhaft_Datum", "Fehlerhaft_Fahrleistung"), skip = 1)
Komponente_K2ST1 <- Komponente_K2ST1 %>%
  filter(!is.na(Fehlerhaft_Datum)) %>%
  select(c(Fehlerhaft_Fahrleistung, Herstellernummer, ID_Sitze, Produktionsdatum)) %>%
  left_join(Bestandteile_Fahrzeuge_OEM1_Typ11, by = "ID_Sitze") %>%
  left_join(Fahrzeuge_OEM1_Typ11_Fehleranalyse, by = "ID_Fahrzeug") %>%
  filter(!is.na(Fehlerhaft_Fahrleistung.x)) %>%
  left_join(Fahrzeuge_OEM1_Typ11, by = "ID_Fahrzeug") %>%
  mutate(Lieferdauer = Produktionsdatum.y - Produktionsdatum.x) %>%
  select(c(Fehlerhaft_Fahrleistung = Fehlerhaft_Fahrleistung.x, Betriebsdauer = days, Herstellernummer = Herstellernummer.x,
           ID_Komponente = ID_Sitze, ID_Fahrzeug, Lieferdauer)) %>%
  filter(!is.na(Betriebsdauer))


Komponente_K3AG1 <- read_csv(".\\Data\\Komponente\\Komponente_K3AG1.csv")
Komponente_K3AG1 <- Komponente_K3AG1 %>%
  filter(!is.na(Fehlerhaft_Datum.x)) %>%
  select(c(Fehlerhaft_Fahrleistung.x, Herstellernummer.x, ID_Schaltung.x, Produktionsdatum.x)) %>%
  left_join(Bestandteile_Fahrzeuge_OEM1_Typ11, by = join_by(ID_Schaltung.x == ID_Schaltung)) %>%
  left_join(Fahrzeuge_OEM1_Typ11_Fehleranalyse, by = "ID_Fahrzeug") %>%
  filter(!is.na(Fehlerhaft_Fahrleistung)) %>%
  left_join(Fahrzeuge_OEM1_Typ11, by="ID_Fahrzeug") %>%
  mutate(Lieferdauer = Produktionsdatum - Produktionsdatum.x) %>%
  select(c(Fehlerhaft_Fahrleistung = Fehlerhaft_Fahrleistung.x, Betriebsdauer = days, Herstellernummer = Herstellernummer.x,
           ID_Komponente = ID_Schaltung.x, ID_Fahrzeug, Lieferdauer))


Komponente_K3SG1 <- read_csv(".\\Data\\Komponente\\Komponente_K3SG1.csv")
Komponente_K3SG1 <- Komponente_K3SG1 %>%
  filter(!is.na(Fehlerhaft_Datum.x)) %>%
  select(c(Fehlerhaft_Fahrleistung.x, Herstellernummer.x, ID_Schaltung.x, Produktionsdatum.x)) %>%
  left_join(Bestandteile_Fahrzeuge_OEM1_Typ11, by = join_by(ID_Schaltung.x == ID_Schaltung)) %>%
  left_join(Fahrzeuge_OEM1_Typ11_Fehleranalyse, by = "ID_Fahrzeug") %>%
  filter(!is.na(Fehlerhaft_Fahrleistung)) %>%
  left_join(Fahrzeuge_OEM1_Typ11, by="ID_Fahrzeug") %>%
  mutate(Lieferdauer = Produktionsdatum - Produktionsdatum.x) %>%
  select(c(Fehlerhaft_Fahrleistung = Fehlerhaft_Fahrleistung.x, Betriebsdauer = days, Herstellernummer = Herstellernummer.x,
           ID_Komponente = ID_Schaltung.x, ID_Fahrzeug, Lieferdauer))


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
           ID_Komponente = ID_Karosserie.x, ID_Fahrzeug, Lieferdauer))


tabelle <- bind_rows(list(Komponente_K1BE1, Komponente_K1DI1 ,Komponente_K2LE1, Komponente_K2ST1, Komponente_K3AG1,
                          Komponente_K3SG1, Komponente_K4)) %>%
            filter(Produktionsdatum >= "2013-01-01" & Produktionsdatum <= "2015-12-31")

#Grouping and Testing for Plot
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


