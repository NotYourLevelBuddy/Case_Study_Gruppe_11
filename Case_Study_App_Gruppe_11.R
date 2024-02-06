# install.packages("tidyverse")
library(readr)
library(dplyr)
# data2 <- read_csv("D:\\RStudio\\Binning\\data.csv ")

# Fahrzeug allgemein
Fahrzeuge_OEM1_Typ11 <- read_csv(".//Data//Fahrzeug//Fahrzeuge_OEM1_Typ11.csv") %>%
  filter(!is.na(Fehlerhaft_Datum))
Fahrzeuge_OEM1_Typ11_Fehleranalyse <- read_csv(".//Data//Fahrzeug//Fahrzeuge_OEM1_Typ11_Fehleranalyse.csv") %>%
  filter(!is.na(Fehlerhaft_Datum))
Bestandteile_Fahrzeuge_OEM1_Typ11 <- read_csv(".//Data//Fahrzeug//Bestandteile_Fahrzeuge_OEM1_Typ11.csv ")

# Karosserie
Komponente_K4 <- read_csv(".//Data//Komponente//Komponente_K4.csv")
Bestandteile_Komponente_K4 <- read_csv(".//Data//Komponente//Bestandteile_Komponente_K4.csv")
Einzelteil_T30 <- read_csv(".//Data//Einzelteil//Einzelteil_T30.csv") %>%
  filter(!is.na(Fehlerhaft_Datum))
# Einzelteil_T31.txt
Einzelteil_T32 <- read_csv(".//Data//Einzelteil//Einzelteil_T32.csv")
# names(Einzelteil_T32)[colnames(Einzelteil_T32) == 'Fehlerhaft_Datum.x'] <- 'new.var.name'
Einzelteil_T32 <- Einzelteil_T32[]

# Schaltung
# Schaltgetriebe

Komponente_K3SG1 <- read_csv(".//Data//Komponente//Komponente_K3SG1.csv")
Bestandteile_Komponente_K3AG1 <- read_csv(".//Data//Komponente//Bestandteile_Komponente_K3AG1.csv")
Einzelteil_T21 <- read_csv(".//Data//Einzelteil//Einzelteil_T21.csv") %>%
  filter(!is.na(Fehlerhaft_Datum))
# Einzelteil_T22.txt
Einzelteil_T23 <- read_csv(".//Data//Einzelteil//Einzelteil_T23.csv") %>%
  filter(!is.na(Fehlerhaft_Datum))

# Automatikgetriebe
Komponente_K3AG1 <- read_csv(".//Data//Komponente//Komponente_K3AG1.csv") %>%
  filter(!is.na(Fehlerhaft_Datum))
Bestandteile_Komponente_K3SG1 <- read_csv(".//Data//Komponente//Bestandteile_Komponente_K3SG1.csv") %>%
  filter(!is.na(Fehlerhaft_Datum))
# Einzelteil_T21.csv Doppelt
# Einzelteil_T24.txt
Einzelteil_T25 <- read_csv(".//Data//Einzelteil//Einzelteil_T25.csv") %>%
  filter(!is.na(Fehlerhaft_Datum))

# Sitze
# Komponente_K2LE1s.txt
Bestandteile_Komponente_K2LE1 <- read_csv(".//Data//Komponente//Bestandteile_Komponente_K2LE1.csv") %>%
  filter(!is.na(Fehlerhaft_Datum))
# Einzelteil_T11.txt
Einzelteil_T14 <- read_csv(".//Data//Einzelteil//Einzelteil_T14.csv") %>%
  filter(!is.na(Fehlerhaft_Datum))
Einzelteil_T15 <- read_csv(".//Data//Einzelteil//Einzelteil_T15.csv") %>%
  filter(!is.na(Fehlerhaft_Datum))

# Komponente_K2LE1s.txt
# Bestandteile_Komponente_K2LE1 Doppelt
# Einzelteil_T11.txt Doppelt
Einzelteil_T12 <- read_csv(".//Data//Einzelteil//Einzelteil_T12.csv") %>%
  filter(!is.na(Fehlerhaft_Datum))
Einzelteil_T13 <- read_csv(".//Data//Einzelteil//Einzelteil_T13.csv") %>%
  filter(!is.na(Fehlerhaft_Datum))

# Motor
Komponente_K1BE1 <- read_csv(".//Data//Komponente//Komponente_K1BE1.csv") %>%
  filter(!is.na(Fehlerhaft_Datum))
Bestandteile_Komponente_K1BE1 <- read_csv(".//Data//Komponente//Bestandteile_Komponente_K1BE1.csv") %>%
  filter(!is.na(Fehlerhaft_Datum))
# Einzelteil_T01.txt
# Einzelteil_T02.txt
# Einzelteil_T03.txt
Einzelteil_T04 <- read_csv(".//Data//Einzelteil//Einzelteil_T04.csv") %>%
  filter(!is.na(Fehlerhaft_Datum))

Komponente_K1DI1 <- read_csv(".//Data//Komponente//Komponente_K1DI1.csv") %>%
  filter(!is.na(Fehlerhaft_Datum))
Bestandteile_Komponente_K1DI1 <- read_csv(".//Data//Komponente//Bestandteile_Komponente_K1DI1.csv") %>%
  filter(!is.na(Fehlerhaft_Datum))
# Einzelteil_T01.txt Doppelt
# Einzelteil_T02.txt Doppelt
Einzelteil_T05 <- read_csv(".//Data//Einzelteil//Einzelteil_T05.csv") %>%
  filter(!is.na(Fehlerhaft_Datum))
Einzelteil_T06 <- read_csv(".//Data//Einzelteil//Einzelteil_T06.csv") %>%
  filter(!is.na(Fehlerhaft_Datum))

Einzelteil_T06
