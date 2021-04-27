library(tidyverse)
library(KFAS)
library(gtrendsR)

# load regional data
load_regione <- function() {
  dt <- jsonlite::fromJSON(txt = "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-json/dpc-covid19-ita-regioni.json")
  dt$data <- as.Date(dt$data)
  pos <-
    dt %>%
    pivot_wider(data, names_from = denominazione_regione, values_from = nuovi_positivi) %>%
    mutate(Italia = rowSums(.[, 2:22]))
  dec <-
    dt %>%
    pivot_wider(data, names_from = denominazione_regione, values_from = deceduti) %>%
    mutate_at(2:22, function(x) x - lag(x)) %>%
    mutate(Italia = rowSums(.[, 2:22]))
  icu <-
    dt %>%
    pivot_wider(data, names_from = denominazione_regione, values_from = terapia_intensiva) %>%
    mutate(Italia = rowSums(.[, 2:22]))
  
  list(positivi = pos, decessi = dec, terapie_intensive = icu)
}

# load province data
load_provincia <- function() {
  dt <- jsonlite::fromJSON(txt = "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-json/dpc-covid19-ita-province.json")
  dt$data <- as.Date(dt$data)
  dt %>%
    filter(denominazione_provincia != "In fase di definizione/aggiornamento",
           denominazione_provincia != "Fuori Regione / Provincia Autonoma") %>%
    pivot_wider(data, names_from = denominazione_provincia, values_from = totale_casi) %>%
    mutate_at(2:108, function(x) x - lag(x)) %>%
    mutate(Italia = rowSums(.[, 2:108]))
}

# load regional vaccine
load_vaccini <- function() {
  raw_vacc_regione <- jsonlite::fromJSON(txt = "https://raw.githubusercontent.com/italia/covid19-opendata-vaccini/master/dati/somministrazioni-vaccini-latest.json")$data
  data_vacc_reg <- data.frame(
    data = as.Date(strtrim(raw_vacc_regione$data_somministrazione, 10)),
    regione = raw_vacc_regione$nome_area,
    fornitore = raw_vacc_regione$fornitore,
    prima_dose = raw_vacc_regione$prima_dose,
    seconda_dose = raw_vacc_regione$seconda_dose,
    dosi_totali = raw_vacc_regione$prima_dose + raw_vacc_regione$seconda_dose,
    fascia_anagrafica = raw_vacc_regione$fascia_anagrafica
  ) 
  data_vacc_reg <- data_vacc_reg %>% filter(data < max(data))
  data_vacc_reg$regione[data_vacc_reg$regione == "Provincia Autonoma Bolzano / Bozen"] <- "P.A. Bolzano"
  data_vacc_reg$regione[data_vacc_reg$regione == "Provincia Autonoma Trento"] <- "P.A. Trento"
  data_vacc_reg$regione[data_vacc_reg$regione == "Valle d'Aosta / Vallée d'Aoste"] <- "Valle d'Aosta"
  data_vacc_reg$regione[data_vacc_reg$regione == "Friuli-Venezia Giulia"] <- "Friuli Venezia Giulia"
  data_vacc_reg
}

# divide by population
# x is a data.frame with dates in first column and time series in the next columns
# pop is a data.frame with territory name and population
divide_by_pop <- function(x, pop, mult = 100000) {
  for (i in names(x)[-1]) {
    if (i == "Italia") {
      x[[i]] <- x[[i]] / sum(pop$Pop) * mult
      next
    }
    x[[i]] <- x[[i]] / pop$Pop[pop[[1]] == i] * mult
  }
  x
}

# x is a data.frame with dates in first column and time series in the next columns
estimate_ssm <- function(x) {
  x[x < 0] <- NA   # negative values are substituted with NA
  k <- dim(x)[2] - 1
  n <- dim(x)[1]
  models <- vector("list", k)   # models container
  names(models) <- names(x)[-1] # each model is named as each time series
  for (i in 1:k) {
    y <- as.numeric(log(1 + x[[1 + i]]))
    mod <- SSModel(y ~ SSMtrend(2, list(0, NA)) + SSMseasonal(7, NA),
                   H = NA)
    vy <- var(diff(y), na.rm = TRUE)
    models[[i]] <- fitSSM(mod, log(c(vy / 100, vy / 100, vy / 100)))$model
  }
  models
}

# x is a data.frame with dates in first column and time series in the next columns
# models is a list with the models in the same order as the time series in x
smoother <- function(x, models) {
  x[x < 0] <- NA   # negative values are substituted with NA
  k <- dim(x)[2] - 1
  n <- dim(x)[1]
  levels <- matrix(0, n, k, dimnames = list(NULL, names(x)[-1]))
  rates  <- matrix(0, n, k, dimnames = list(NULL, names(x)[-1]))
  for (i in 1:k) {
    y <- as.numeric(log(1 + x[[1 + i]]))
    mod <- models[[i]]
    mod$y <- as.matrix(y)
    attr(mod, "n") <- length(y)
    kfs <- KFS(mod, smoothing = "state", filtering = "none")
    levels[, i] <- as.numeric(exp(kfs$alphahat[, 1] + 0.5 * kfs$V[1, 1, ])) - 1
    levels[levels < 0] <- 0
    # rates[, i] <- (levels[, i] - lag(levels[, i])) / lag(levels[, i]) #as.numeric(kfs$alphahat[, 2])
    rates[, i] <- as.numeric(exp(kfs$alphahat[, 2] + 0.5 * kfs$V[2, 2, ])) - 1
  }
  list(
    livello  = cbind(data = x[[1]], as_tibble(levels)),
    crescita = cbind(data = x[[1]], as_tibble(rates * 100))
  )  
}

# function to laod gtrend daata
get_gtrend <- function(keyword = "sintomi covid",
                       region  = "Italia",
                       from    = "2020-09-15",
                       to      = as.character(Sys.Date()),
                       output  = c("xts", "data.frame")) {
  codes <- c(
    "Abruzzo" =                "IT-65",
    "Basilicata" =             "IT-77",
    "Calabria" =               "IT-78",
    "Campania" =               "IT-72",
    "Emilia-Romagna" =         "IT-45",
    "Friuli Venezia Giulia" =  "IT-36",
    "Lazio" =                  "IT-62",
    "Liguria" =                "IT-42",
    "Lombardia" =              "IT-25",
    "Marche" =                 "IT-57",
    "Molise" =                 "IT-67",
    "P.A. Bolzano" =           "IT-BZ", # SOLO CITTA'
    "P.A. Trento" =            "IT-TN", # SOLO CITTA'
    "Piemonte" =               "IT-21",
    "Puglia" =                 "IT-75",
    "Sardegna" =               "IT-88",
    "Sicilia" =                "IT-82",
    "Toscana" =                "IT-52",
    "Umbria" =                 "IT-55",
    "Valle d'Aosta" =          "IT-23",
    "Veneto" =                 "IT-34",
    "Italia" =                 "IT")
  regnames <- names(codes)
  names(regnames) <- codes
  dt <- gtrends(keyword = keyword,
                geo = codes[region],
                time = paste(from, to),
                onlyInterest = TRUE)$interest_over_time
  out <- dt %>%
    mutate(region = regnames[geo], hits = as.numeric(hits)) %>%
    select(date, hits, keyword, region)
  if (output[1] == "xts") {
    outxts <- out %>%
      pivot_wider(names_from = keyword:region, values_from = hits,
                  names_sep = " ") %>%
      arrange(date)
    return(xts(outxts[, -1], outxts$date))
  }
  out
}

# Estimate and carry out smoothing
get_cmp <- function(x) {
  x[x < 0] <- NA
  k <- dim(x)[2] - 1
  n <- dim(x)[1]
  levels <- matrix(0, n, k, dimnames = list(NULL, names(x)[-1]))
  rates <- matrix(0, n, k, dimnames = list(NULL, names(x)[-1]))
  for (i in 1:k) {
    y <- as.numeric(log(1 + x[[1 + i]]))
    mod <- SSModel(y ~ SSMtrend(2, list(0, NA)) + SSMseasonal(7, NA),
                   H = NA
    )
    vy <- var(diff(y), na.rm = TRUE)
    fit <- fitSSM(mod, log(c(vy / 100, vy / 100, vy / 100)))
    kfs <- KFS(fit$model, smoothing = "state")
    levels[, i] <- as.numeric(exp(kfs$alphahat[, 1] + 0.5 * kfs$V[1, 1, ])) - 1
    levels[levels < 0] <- 0
    # rates[, i] <- (levels[, i] - lag(levels[, i])) / lag(levels[, i]) #as.numeric(kfs$alphahat[, 2])
    rates[, i] <- as.numeric(exp(kfs$alphahat[, 2] + 0.5 * kfs$V[2, 2, ])) - 1
  }
  list(
    livello  = cbind(data = x[[1]], as_tibble(levels)),
    crescita = cbind(data = x[[1]], as_tibble(rates * 100))
  )  
}
