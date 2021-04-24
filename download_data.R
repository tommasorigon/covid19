# Dati regionali
raw_regione <- read.csv(file = "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv", stringsAsFactors = FALSE)

# Dati provinciali
raw_prov <- read.csv(file = "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province.csv", stringsAsFactors = FALSE)

# Dati vaccini
raw_vacc_regione <- read.csv(file = "https://raw.githubusercontent.com/italia/covid19-opendata-vaccini/master/dati/somministrazioni-vaccini-latest.csv", stringsAsFactors = FALSE)

# Popolazione delle regioni e delle province
pop_regioni <- read.csv(file = "regioni.csv", stringsAsFactors = T)
pop_province <- read.csv(file = "province.csv", stringsAsFactors = T)

# Organizzazione dei dati ------------------------------

data_reg <- data.frame(
  data = as.Date(strtrim(raw_regione$data, 10)),
  regione = raw_regione$denominazione_regione,
  decessi = raw_regione$deceduti,
  contagiati = raw_regione$nuovi_positivi,
  terapia_intensiva = raw_regione$terapia_intensiva,
  tamponi = raw_regione$tamponi
)

data_vacc_reg <- data.frame(
  data = as.Date(strtrim(raw_vacc_regione$data_somministrazione, 10)),
  regione = raw_vacc_regione$nome_area,
  fornitore = raw_vacc_regione$fornitore,
  prima_dose = raw_vacc_regione$prima_dose,
  seconda_dose = raw_vacc_regione$seconda_dose,
  dosi_totali = raw_vacc_regione$prima_dose + raw_vacc_regione$seconda_dose,
  fascia_anagrafica = raw_vacc_regione$fascia_anagrafica
)

data_prov <- data.frame(
  data = as.Date(strtrim(raw_prov$data, 10)),
  regione = raw_prov$denominazione_regione,
  sigla = raw_prov$sigla_provincia,
  provincia = raw_prov$denominazione_provincia,
  contagiati = raw_prov$totale_casi,
  decessi = NA,
  terapia_intensiva = NA,
  tamponi = NA
)

data_vacc_reg$regione[data_vacc_reg$regione == "Provincia Autonoma Bolzano / Bozen"] <- "P.A. Bolzano"
data_vacc_reg$regione[data_vacc_reg$regione == "Provincia Autonoma Trento"] <- "P.A. Trento"
data_vacc_reg$regione[data_vacc_reg$regione == "Valle d'Aosta / Vallée d'Aoste"] <- "Valle d'Aosta"
data_vacc_reg$regione[data_vacc_reg$regione == "Friuli-Venezia Giulia"] <- "Friuli Venezia Giulia"

data_prov$sigla[is.na(data_prov$sigla)] <- "NAP"
data_prov <- data_prov[data_prov$provincia != "In fase di definizione/aggiornamento", ]
data_prov <- data_prov[data_prov$provincia != "Fuori Regione / Provincia Autonoma", ]

max_date <- max(data_reg$data)
min_date <- min(data_reg$data)

data_reg <- merge(data_reg, pop_regioni, by.x = "regione", by.y = "Regione")
data_prov <- merge(data_prov, pop_province, by.x = "provincia", by.y = "Provincia")
data_vacc_reg <- merge(data_vacc_reg, pop_regioni, by.x = "regione", by.y = "Regione")

# Escludo l'ultimo giorno --- tipicamente viene caricato per gradi e questo comporta confusione
data_vacc_reg <- data_vacc_reg[data_vacc_reg$data < max(data_vacc_reg$data),]
