# Load packages
library(shiny)
library(shinythemes)
library(highcharter)
library(KFAS)
library(xts)
library(dygraphs)
library(DT)
library(tidyverse)

# Kalman Filter ----------------------------------------------------
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
    rates[, i] <- (levels[, i] - lag(levels[, i])) / lag(levels[, i]) #as.numeric(kfs$alphahat[, 2])
  }
  list(
    livello = as_tibble(data = x[[1]], levels),
    crescita = as_tibble(data = x[[1]], rates * 100)
  )
}


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


# Define UI --------------------------------------------------------------
ui <- fluidPage(

  theme = shinytheme("united"),
  navbarPage(
    "Report dati Covid-19 in Italia",
    # Tabpanel ----------------------------------------
    tabPanel(
      "Epidemia",
      # UI Epidemia ---------------------------------------
      sidebarLayout(
        sidebarPanel(

          # Select type of trend to plot
          selectInput(
            inputId = "datatype", label = strong("Seleziona livello di aggregazione"),
            choices = c("Nazionale", "Regionale", "Provinciale"),
            selected = "Nazionale"
          ),

          # Select date range to be plotted
          dateRangeInput("date", strong("Intervallo temporale"),
            language = "it", start = max_date - 180, end = max_date,
            min = min_date, max = max_date
          ),
          conditionalPanel(
            condition = "input.datatype != 'Provinciale'",
            selectInput(
              inputId = "type", label = strong("Seleziona tipologia di dati"),
              choices = c("Nuovi positivi", "Decessi", "Pazienti in terapia intensiva"),
              selected = "Nuovi positivi"
            )
          ),

          # Select type of trend to plot
          conditionalPanel(
            condition = "input.datatype == 'Regionale'",
            selectInput(
              inputId = "region", label = strong("Seleziona regione"),
              choices = unique(data_reg$regione), multiple = T,
              selected = c("Lombardia", "Veneto")
            )
          ),
          conditionalPanel(
            condition = "input.datatype == 'Provinciale'",
            selectInput(
              inputId = "prov", label = strong("Seleziona provincia"),
              choices = sort(unique(data_prov$provincia)), multiple = T,
              selected = c("Milano", "Venezia")
            ),
          ),
          HTML("<hr> <b> I dati</b>. I dati originali sono resi disponibili dal Dipartimento della protezione civile a questo link: <a href='https://github.com/pcm-dpc/COVID-19'>https://github.com/pcm-dpc/COVID-19</a>. I dati relativi alla popolazione residente di regioni e province fanno riferimento al 1 Gennaio 2020 e sono disponibili sul <a href='http://dati.istat.it/Index.aspx?QueryId=42869'> portale ISTAT</a>. <hr>
<b>Il metodo</b>. Il <i>trend</i> è ottenuto tramite Kalman filter, stimato sulla scala logaritmica. Il ternd rappresenta una versione dei dati depurata dalle oscillazioni casuali e quindi è di più facile interpretazione. Il <i>tasso di crescita</i> invece rappresenta la variazione percentuale giornaliera del trend. Ad esempio, il tasso di crescita di oggi si calcola tramite la formula: 100*(trend di oggi - trend di ieri) / (trend di ieri). 
      <hr> L'autore di questa applicazione è <a href='https://tommasorigon.github.io'><b>Tommaso Rigon</b></a>, a cui è possibile scrivere per eventuali segnalazioni e richieste di chiarimenti.")
        ),

        # Output: Description, lineplot, and reference
        mainPanel(
          tabsetPanel(
            tabPanel(
              "Grafici",
              dygraphOutput("casi"),
              hr(),
              dygraphOutput("tassi"),
              hr()
            ),
            tabPanel(
              "Tabelle",
              hr(),
              textOutput("tab_testo"),
              hr(),
              DTOutput("tabella")
            )
          )
        )
      )
    ),




    # UI Vaccinazione -------------------------------------------------
    tabPanel(
      "Vaccinazione",
      sidebarLayout(
        sidebarPanel(

          # Select type of trend to plot
          selectInput(
            inputId = "datatype2", label = strong("Seleziona livello di aggregazione"),
            choices = c("Nazionale", "Regionale"),
            selected = "Nazionale"
          ),
          selectInput(
            inputId = "vaccine", label = strong("Seleziona vaccino"),
            choices = c("Tutti", unique(data_vacc_reg$fornitore)),
            selected = "Tutti"
          ),

          # Select type of trend to plot
          conditionalPanel(
            condition = "input.datatype2 == 'Regionale'",
            selectInput(
              inputId = "region3", label = strong("Seleziona regione"),
              choices = unique(data_vacc_reg$regione),
              selected = "Lombardia"
            )
          ),
          HTML("<hr> <b>I dati</b>. I dati originali sono resi disponibili dal Commissario straordinario per l'emergenza Covid-19 a questo link: <a href='https://github.com/italia/covid19-opendata-vaccini'>https://github.com/italia/covid19-opendata-vaccini</a>. Le tabelle riportano il numero di vaccini somministrati (prima tabella) e la percentuale rispetto alla popolazione nazionale / regionale (seconda tabella). I dati relativi alla popolazione residente di regioni e province fanno riferimento al 1 Gennaio 2020 e sono disponibili sul <a href='http://dati.istat.it/Index.aspx?QueryId=42869'> portale ISTAT </a>.<hr>
          <b>Il metodo</b>. Il <i>trend</i> è ottenuto tramite Kalman filter, stimato sulla scala logaritmica. Il ternd rappresenta una versione dei dati depurata dalle oscillazioni casuali e quindi è di più facile interpretazione.
      <hr> L'autore di questa applicazione è <a href='https://tommasorigon.github.io'><b>Tommaso Rigon</b></a>, a cui è possibile scrivere per eventuali segnalazioni e richieste di chiarimenti.")
        ),

        # Output: Description, lineplot, and reference
        mainPanel(
          dygraphOutput("vaccini"),
          hr(),
          DTOutput("tbl"),
          hr(),
          DTOutput("tbl2")
        )
      )
    )
  )
)


# Define server ---------------
server <- function(input, output) {
  dati_positivi_provincia <- reactive({

    # New positive data
    data_plot <- aggregate(cbind(contagiati, Pop) ~ data + provincia, sum, data = data_prov[data_prov$provincia %in% input$prov, ])
    data_plot$contagiati <- diff(c(0, data_plot$contagiati))
    data_plot$contagiati <- pmax(data_plot$contagiati, 0) / data_plot$Pop * 100000
    data_plot <- data_plot %>% pivot_wider(id_cols = data, names_from = provincia, values_from = contagiati)

    trend <- get_cmp(data_plot)
    list(
      livello = xts(trend$livello, data_plot$data),
      crescita = xts(trend$crescita, data_plot$data)
    )
  })

  dati_positivi_regione <- reactive({

    # New positive data
    data_plot <- aggregate(cbind(contagiati, Pop) ~ data + regione, sum, data = data_reg[data_reg$regione %in% input$region, ])
    data_plot$contagiati <- pmax(data_plot$contagiati, 0) / data_plot$Pop * 100000
    data_plot <- data_plot %>% pivot_wider(id_cols = data, names_from = regione, values_from = contagiati)
    data_plot$Italia <- aggregate(contagiati ~ data, sum, data = data_reg)$contagiati / sum(pop_regioni$Pop) * 100000

    trend <- get_cmp(data_plot)
    list(
      livello = xts(trend$livello, data_plot$data),
      crescita = xts(trend$crescita, data_plot$data)
    )
  })

  dati_decessi_regione <- reactive({
    # First region
    data_plot <- aggregate(cbind(decessi, Pop) ~ data + regione, sum, data = data_reg[data_reg$regione %in% input$region, ])
    data_plot$decessi <- diff(c(0, data_plot$decessi))
    data_plot$decessi <- pmax(data_plot$decessi, 0) / data_plot$Pop * 100000
    data_plot <- data_plot %>% pivot_wider(id_cols = data, names_from = regione, values_from = decessi)
    data_plot$Italia <- aggregate(decessi ~ data, sum, data = data_reg)$decessi
    data_plot$Italia <- pmax(diff(c(0, data_plot$Italia)), 0) / sum(pop_regioni$Pop) * 100000

    trend <- get_cmp(data_plot)
    list(
      livello = xts(trend$livello, data_plot$data),
      crescita = xts(trend$crescita, data_plot$data)
    )
  })

  dati_terapia_regione <- reactive({

    # First region
    data_plot <- aggregate(cbind(terapia_intensiva, Pop) ~ data + regione, sum, data = data_reg[data_reg$regione %in% input$region, ])
    data_plot$contagiati <- pmax(data_plot$terapia_intensiva, 0) / data_plot$Pop * 100000
    data_plot <- data_plot %>% pivot_wider(id_cols = data, names_from = regione, values_from = terapia_intensiva)
    data_plot$Italia <- aggregate(terapia_intensiva ~ data, sum, data = data_reg)$terapia_intensiva / sum(pop_regioni$Pop) * 100000

    trend <- get_cmp(data_plot)
    list(
      livello = xts(trend$livello, data_plot$data),
      crescita = xts(trend$crescita, data_plot$data)
    )
  })

  dati_dosi_totali_nazionale <- reactive({
    vaccino <- input$vaccine
    if (vaccino != "Tutti") {
      id_vaccino <- data_vacc_reg$fornitore %in% vaccino
    } else {
      id_vaccino <- rep(TRUE, nrow(data_vacc_reg))
    }

    # New positive data
    data_plot <- data_vacc_reg[id_vaccino, ]
    data_plot <- aggregate(cbind(dosi_totali) ~ data, sum, data = data_plot)
    data_plot <- data_plot %>% transmute(data = data_plot$data, Italia = pmax(data_plot$dosi_totali, 0))

    trend <- get_cmp(data_plot)
    list(
      livello = xts(trend$livello, data_plot$data),
      crescita = xts(trend$crescita, data_plot$data)
    )
  })

  dati_dosi_totali_regione <- reactive({
    vaccino <- input$vaccine
    if (vaccino != "Tutti") {
      id_vaccino <- data_vacc_reg$fornitore %in% vaccino
    } else {
      id_vaccino <- rep(TRUE, nrow(data_vacc_reg))
    }

    # New positive data
    data_plot <- data_vacc_reg[data_vacc_reg$regione %in% input$region3 & id_vaccino, ]
    data_plot <- aggregate(cbind(dosi_totali) ~ data + regione, sum, data = data_plot)
    data_plot$dosi_totali <- pmax(data_plot$dosi_totali, 0)
    data_plot <- data_plot %>% pivot_wider(id_cols = data, names_from = regione, values_from = dosi_totali, values_fill = 0)

    trend <- get_cmp(data_plot)
    list(
      livello = xts(trend$livello, data_plot$data),
      crescita = xts(trend$crescita, data_plot$data)
    )
  })

  output$casi <- renderDygraph({
    if (input$datatype == "Nazionale") {
      region <- "Italia"
    } else if (input$datatype == "Regionale") {
      region <- input$region
    }

    if (input$datatype != "Provinciale") {
      if (input$type == "Nuovi positivi") {
        type <- input$type
        data_plot <- dati_positivi_regione()$livello[, region]
      } else if (input$type == "Decessi") {
        type <- input$type
        data_plot <- dati_decessi_regione()$livello[, region]
      } else {
        type <- input$type
        data_plot <- dati_terapia_regione()$livello[, region]
      }
    } else {
      type <- "Nuovi positivi"
      data_plot <- dati_positivi_provincia()$livello[, input$prov]
    }


    data_plot <- data_plot[(index(data_plot) >= input$date[1]) & (index(data_plot) <= input$date[2]), ]
    dygraph(data_plot, main = paste(type, "ogni 100.000 abitanti (trend)"), ylab = paste(type, "ogni 100.000 abitanti")) %>%
      dyOptions(colors = RColorBrewer::brewer.pal(8, "Dark2"), axisLineWidth = 1.5, fillGraph = TRUE, drawGrid = FALSE)
  })

  output$vaccini <- renderDygraph({
    if (input$datatype2 == "Nazionale") {
      data_plot <- dati_dosi_totali_nazionale()$livello
    } else if (input$datatype2 == "Regionale") {
      data_plot <- dati_dosi_totali_regione()$livello[, input$region3]
    }

    data_plot <- data_plot[(index(data_plot) >= input$date[1]) & (index(data_plot) <= input$date[2]), ]
    dygraph(data_plot, main = paste("Dosi totali giornaliere somministrate (trend)")) %>%
      dyOptions(colors = RColorBrewer::brewer.pal(8, "Dark2"), axisLineWidth = 1.5, fillGraph = TRUE, drawGrid = FALSE)
  })

  output$tassi <- renderDygraph({
    if (input$datatype == "Nazionale") {
      region <- "Italia"
    } else if (input$datatype == "Regionale") {
      region <- input$region
    }

    if (input$datatype != "Provinciale") {
      if (input$type == "Nuovi positivi") {
        type <- input$type
        data_plot <- dati_positivi_regione()$crescita[, region]
      } else if (input$type == "Decessi") {
        type <- input$type
        data_plot <- dati_decessi_regione()$crescita[, region]
      } else {
        type <- input$type
        data_plot <- dati_terapia_regione()$crescita[, region]
      }
    } else {
      type <- "Nuovi positivi"
      data_plot <- dati_positivi_provincia()$crescita[, input$prov]
    }

    data_plot <- data_plot[(index(data_plot) >= input$date[1]) & (index(data_plot) <= input$date[2])]
    dygraph(data_plot,
      main = "Tasso di crescita giornaliero (%)", ylab = "Tasso di crescita giornaliero (%)"
    ) %>%
      dyLimit(limit = 0, strokePattern = "dashed") %>%
      dyOptions(colors = RColorBrewer::brewer.pal(8, "Dark2"), axisLineWidth = 1.5, fillGraph = TRUE, drawGrid = FALSE)
  })


  # ---- Tabelle
  output$tab_testo <- renderText({
    paste("Giorno considerato per le stime:", input$date[2])
  })

  output$tabella <- renderDT({
    if (input$datatype == "Nazionale") {
      dt <- tibble(
        Regione = "Italia",
        `Casi per 100k abitanti` = as.numeric(tail(dati_positivi_regione()$livello[, "Italia"], 1)),
        `Crescita casi (%)` = as.numeric(tail(dati_positivi_regione()$crescita[, "Italia"], 1)),
        `Decessi per 100k abitanti` = as.numeric(tail(dati_decessi_regione()$livello[, "Italia"], 1)),
        `Crescita decessi (%)` = as.numeric(tail(dati_decessi_regione()$crescita[, "Italia"], 1)),
      ) %>%
        datatable(rownames = FALSE, options = list(pageLength = 22, dom = "t")) %>%
        formatStyle(
          columns = c(3, 5),
          color = styleInterval(0, c("black", "red"))
        ) %>%
        formatRound(columns = 2:5, digits = 2)
    }

    if (input$datatype == "Regionale") {
      dt <- tibble(
        Regione = colnames(dati_positivi_regione()$livello),
        `Casi per 100k abitanti` = as.numeric(tail(dati_positivi_regione()$livello, 1)),
        `Crescita casi (%)` = as.numeric(tail(dati_positivi_regione()$crescita, 1)),
        `Decessi per 100k abitanti` = as.numeric(tail(dati_decessi_regione()$livello, 1)),
        `Crescita decessi (%)` = as.numeric(tail(dati_decessi_regione()$crescita, 1)),
      ) %>%
        datatable(rownames = FALSE, options = list(pageLength = 22, dom = "t")) %>%
        formatStyle(
          columns = c(3, 5),
          color = styleInterval(0, c("black", "red"))
        ) %>%
        formatRound(columns = 2:5, digits = 2)
    }

    if (input$datatype == "Provinciale") {
      dt <- tibble(
        Provincia = colnames(dati_positivi_provincia()$livello),
        `Casi per 100k abitanti` = as.numeric(tail(dati_positivi_provincia()$livello, 1)),
        `Crescita casi (%)` = as.numeric(tail(dati_positivi_provincia()$crescita, 1)),
      ) %>%
        datatable(rownames = FALSE, options = list(pageLength = 22, dom = "t")) %>%
        formatStyle(
          columns = c(3, 3),
          color = styleInterval(0, c("black", "red"))
        ) %>%
        formatRound(columns = 2:3, digits = 2)
    }

    dt
  })

  output$tbl <- renderDT({
    regione <- input$region3

    if (input$datatype2 == "Regionale") {
      data_plot <- data_vacc_reg[data_vacc_reg$regione == regione, ]
      data_plot <- aggregate(cbind(prima_dose, seconda_dose, dosi_totali) ~ fornitore, sum, data = data_plot)
    } else if (input$datatype2 == "Nazionale") {
      regione <- "Italia"
      data_plot <- data_vacc_reg
      data_plot <- aggregate(cbind(prima_dose, seconda_dose, dosi_totali) ~ fornitore, sum, data = data_plot)
    }

    data_plot <- rbind(data_plot, c("Totale", colSums(data_plot[, -1])))
    colnames(data_plot) <- c(paste("Vaccino -", regione), "Prima dose", "Seconda dose", "Totale")
    datatable(data_plot, rownames = FALSE, options = list(pageLength = 22, dom = "t"))
  })

  output$tbl2 <- renderDT({
    regione <- input$region3

    if (input$datatype2 == "Regionale") {
      K <- pop_regioni$Pop[pop_regioni$Regione == regione]
    } else if (input$datatype2 == "Nazionale") {
      K <- sum(pop_regioni$Pop)
    }


    if (input$datatype2 == "Regionale") {
      data_plot <- data_vacc_reg[data_vacc_reg$regione == regione, ]
      data_plot <- aggregate(cbind(prima_dose, seconda_dose) / K * 100 ~ fornitore, sum, data = data_plot)
    } else if (input$datatype2 == "Nazionale") {
      regione <- "Italia"
      data_plot <- data_vacc_reg
      data_plot <- aggregate(cbind(prima_dose, seconda_dose) / K * 100 ~ fornitore, sum, data = data_plot)
    }

    data_plot <- rbind(data_plot, c("Totale", colSums(data_plot[, -1])))
    data_plot$prima_dose <- as.numeric(data_plot$prima_dose)
    data_plot$seconda_dose <- as.numeric(data_plot$seconda_dose)
    colnames(data_plot) <- c(paste("Vaccino -", regione), "Prima dose (%)", "Seconda dose (%)")
    datatable(data_plot, rownames = FALSE, options = list(pageLength = 22, dom = "t")) %>%
      formatRound(columns = 2:3, digits = 2)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
