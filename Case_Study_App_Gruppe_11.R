# install.packages("tidyverse")
library(readr)
library(dplyr)
# data2 <- read_csv("D:\\RStudio\\Binning\\data.csv ")

# -----------------------------------
# Fahrzeug allgemein
Fahrzeuge_OEM1_Typ11 <- read_csv(".//Data//Fahrzeug//Fahrzeuge_OEM1_Typ11.csv")
Fahrzeuge_OEM1_Typ11_Fehleranalyse <- read_csv(".//Data//Fahrzeug//Fahrzeuge_OEM1_Typ11_Fehleranalyse.csv")
Bestandteile_Fahrzeuge_OEM1_Typ11 <- read_csv2(".//Data//Fahrzeug//Bestandteile_Fahrzeuge_OEM1_Typ11.csv ")

# ------------------------------------
# Karosserie
Komponente_K4 <- read_csv2(".//Data//Komponente//Komponente_K4.csv")
Bestandteile_Komponente_K4 <- read_csv2(".//Data//Komponente//Bestandteile_Komponente_K4.csv")
Einzelteil_T30 <- read_csv(".//Data//Einzelteil//Einzelteil_T30.csv")
# Einzelteil_T31.txt
Einzelteil_T32 <- read_csv2(".//Data//Einzelteil//Einzelteil_T32.csv")

# --------------------------------------
# Schaltung
# Schaltgetriebe
Komponente_K3SG1 <- read_csv(".//Data//Komponente//Komponente_K3SG1.csv")
Bestandteile_Komponente_K3AG1 <- read_csv2(".//Data//Komponente//Bestandteile_Komponente_K3AG1.csv")
Einzelteil_T21 <- read_csv2(".//Data//Einzelteil//Einzelteil_T21.csv")
# Einzelteil_T22.txt
Einzelteil_T23 <- read_csv2(".//Data//Einzelteil//Einzelteil_T23.csv") %>%
  filter(!is.na(Fehlerhaft_Datum.x))

# Automatikgetriebe
Komponente_K3AG1 <- read_csv(".//Data//Komponente//Komponente_K3AG1.csv") %>%
  filter(!is.na(Fehlerhaft_Datum.x))
Bestandteile_Komponente_K3SG1 <- read_csv2(".//Data//Komponente//Bestandteile_Komponente_K3SG1.csv")
# Einzelteil_T21.csv Doppelt
# Einzelteil_T24.txt
Einzelteil_T25 <- read_csv(".//Data//Einzelteil//Einzelteil_T25.csv") %>%
  filter(!is.na(Fehlerhaft_Datum))

# Sitze
# Komponente_K2LE1s.txt
Bestandteile_Komponente_K2LE1 <- read_csv2(".//Data//Komponente//Bestandteile_Komponente_K2LE1.csv")
# Einzelteil_T11.txt
Einzelteil_T14 <- read_csv2(".//Data//Einzelteil//Einzelteil_T14.csv") %>%
  filter(!is.na(Fehlerhaft_Datum))
Einzelteil_T15 <- read_csv2(".//Data//Einzelteil//Einzelteil_T15.csv") %>%
  filter(!is.na(Fehlerhaft_Datum.x))
# Komponente_K2LE1s.txt
# Bestandteile_Komponente_K2LE1 Doppelt
# Einzelteil_T11.txt Doppelt
Einzelteil_T12 <- read_csv2(".//Data//Einzelteil//Einzelteil_T12.csv") %>%
  filter(!is.na(Fehlerhaft_Datum.x))
Einzelteil_T13 <- read_csv2(".//Data//Einzelteil//Einzelteil_T13.csv") %>%
  filter(!is.na(Fehlerhaft_Datum))

# Motor
Komponente_K1BE1 <- read_csv(".//Data//Komponente//Komponente_K1BE1.csv") %>%
  filter(!is.na(Fehlerhaft_Datum))
Bestandteile_Komponente_K1BE1 <- read_csv2(".//Data//Komponente//Bestandteile_Komponente_K1BE1.csv")
# Einzelteil_T01.txt
# Einzelteil_T02.txt
# Einzelteil_T03.txt
Einzelteil_T04 <- read_csv2(".//Data//Einzelteil//Einzelteil_T04.csv") %>%
  filter(!is.na(Fehlerhaft_Datum))

Komponente_K1DI1 <- read_csv(".//Data//Komponente//Komponente_K1DI1.csv") %>%
  filter(!is.na(Fehlerhaft_Datum.x))
Bestandteile_Komponente_K1DI1 <- read_csv2(".//Data//Komponente//Bestandteile_Komponente_K1DI1.csv")
# Einzelteil_T01.txt Doppelt
# Einzelteil_T02.txt Doppelt
Einzelteil_T05 <- read_csv(".//Data//Einzelteil//Einzelteil_T05.csv") %>%
  filter(!is.na(Fehlerhaft_Datum.x))
Einzelteil_T06 <- read_csv(".//Data//Einzelteil//Einzelteil_T06.csv") %>%
  filter(!is.na(Fehlerhaft_Datum))

# -----------------------------------------------------------------------------------
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

