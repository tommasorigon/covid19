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
  dt <- tibble(jsonlite::fromJSON(txt = "https://raw.githubusercontent.com/italia/covid19-opendata-vaccini/master/dati/somministrazioni-vaccini-latest.json")$data)
  dt <- dt %>% mutate(
    data = as.Date(strtrim(dt$data_somministrazione, 10)),
    regione = dt$nome_area,
    fornitore = dt$fornitore,
    prima_dose = dt$prima_dose,
    seconda_dose = dt$seconda_dose,
    dosi_totali = dt$prima_dose + dt$seconda_dose,
    fascia_anagrafica = factor(dt$fascia_anagrafica)
  ) 
  # dt <- dt %>% filter(data < max(data))
  dt$regione[dt$regione == "Provincia Autonoma Bolzano / Bozen"] <- "P.A. Bolzano"
  dt$regione[dt$regione == "Provincia Autonoma Trento"] <- "P.A. Trento"
  dt$regione[dt$regione == "Valle d'Aosta / Vallée d'Aoste"] <- "Valle d'Aosta"
  dt$regione[dt$regione == "Friuli-Venezia Giulia"] <- "Friuli Venezia Giulia"
  
  dt <- dt %>% mutate(ciclo_concluso = seconda_dose) %>%
    mutate(ciclo_concluso = replace(ciclo_concluso, fornitore == "Janssen", prima_dose[fornitore == "Janssen"]))
}

vaccini_eta <- function(dt, pop){
  
  pop_values <- (pop %>% group_by(eta_class2) %>% summarise(popolazione = sum(popolazione)))$popolazione[c(3:10,2)]
  pop_values <- c(pop_values, sum(pop_values))
  
  prima <- prima_int <- 
    dt %>%
    pivot_wider(data, names_from = fascia_anagrafica, values_from = prima_dose, values_fn = sum, values_fill = 0) %>%
    mutate(Tutte = rowSums(.[, 2:10], na.rm = T))
  
  for (i in 2:ncol(prima)) {
    prima[, i] <- cumsum(prima[, i]) / pop_values[i - 1] * 100
    prima_int[, i] <- cumsum(prima_int[, i])
  }

  ciclo_concluso <- ciclo_concluso_int <-
    dt %>%
    pivot_wider(data, names_from = fascia_anagrafica, values_from = ciclo_concluso, values_fn = sum, values_fill = 0) %>%
    mutate(Tutte = rowSums(.[, 2:10], na.rm = T))
  
  for (i in 2:ncol(ciclo_concluso)) {
    ciclo_concluso[, i] <- cumsum(ciclo_concluso[, i]) / pop_values[i - 1] * 100
    ciclo_concluso_int[, i] <- cumsum(ciclo_concluso_int[, i])
  }
  
  list(prima = prima, prima_int = prima_int, ciclo_concluso = ciclo_concluso, ciclo_concluso_int = ciclo_concluso_int)
}

vaccini_reg <- function(dt, pop){
  
  pop_values <- (pop %>% filter(eta_class2 != "0-15") %>% group_by(regione) %>% summarise(popolazione = sum(popolazione)))$popolazione

  dt <- dt %>% group_by(regione) %>%
    summarize(prima_dose = sum(prima_dose), ciclo_concluso = sum(ciclo_concluso)) %>% 
    mutate(popolazione = pop_values) %>%
    mutate(prima_dose_perc = prima_dose / pop_values * 100) %>%
    mutate(ciclo_concluso_perc = ciclo_concluso / pop_values * 100)
  dt <- dt[c(1,4,2,5,3,6)]
  colnames(dt) <- c("Regione", "Popolazione", "Prima dose", "Prima dose (%)", "Ciclo vaccinale concluso", "Ciclo vaccinale concluso (%)")
  dt
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

min_max_norm <- function(x, mult = 100) {
  (x - min(x)) / (max(x) - min(x)) * mult
}
