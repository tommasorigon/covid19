# Load packages
library(shiny)
library(shinythemes)
library(KFAS)
library(xts)
library(dygraphs)
library(DT)
library(tidyverse)
library(gtrendsR)

# Carica le funzioni utili
source("kalman_f.R")
source("gtrends.R")
source("download_data.R")

# Define UI --------------------------------------------------------------
ui <- fluidPage(
  
  theme = shinytheme("united"),
  
  navbarPage(
    "Report dati Covid-19 in Italia",
    
    # UI Epidemia ---------------------------------------
    tabPanel(
      "Epidemia",
      sidebarLayout(
        sidebarPanel(

          selectInput(
            inputId = "datatype", label = strong("Seleziona livello di aggregazione"),
            choices = c("Nazionale", "Regionale", "Provinciale"),
            selected = "Nazionale"
          ),

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
          hr(),
          HTML("Gli autori di questa applicazione sono <a href='https://www.unimib.it/matteo-maria-pelagatti'><b>Matteo Pelagatti</b></a> e <a href='https://tommasorigon.github.io'><b>Tommaso Rigon</b></a>, a cui è possibile scrivere per eventuali segnalazioni e richieste di chiarimenti."),
          hr(),
          HTML("Per ulteriori informazioni, si rimanda inoltre alla documentazione presente su questo sito.")
        ),

        # Output: Description, lineplot, and reference
        mainPanel(
          tabsetPanel(
            tabPanel(
              "Evoluzione epidemia",
              hr(),
              dygraphOutput("casi"),
              hr(),
              dygraphOutput("tassi"),
              hr()
            ),
            tabPanel(
              "Ultimo dato disponibile",
              hr(),
              textOutput("tab_testo"),
              hr(),
              DTOutput("tabella"),
              hr()
            ),
            tabPanel(
              "Google trends",
              hr(),
              HTML("<b> Sperimentale</b>. I grafici di questa sezione mettono a confronto le serie storiche dei nuovi positivi (a livello nazionale) e i dati relativi a <a href = 'https://trends.google.com/trends/?geo=US'> <b> Google trends </b> </a>, utilizzando la parola chiave 'sintomi covid'. Entrambe le serie storiche sono state opportunamente depurate tramite Kalman filter e quindi riscalatate (ovvero divise per il massimo e moltiplicate per 100), per poter essere confrontabili.
                   <hr> <b> Nota</b>. Attualmente sono disponibili solamente i <b> dati nazionali </b> degli ultimi 180 giorni. "),
              hr(),
              dygraphOutput("gtrends"),
              hr(),
              dygraphOutput("tassi_gtrends"),
              hr()
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

          conditionalPanel(
            condition = "input.datatype2 == 'Regionale'",
            selectInput(
              inputId = "region3", label = strong("Seleziona regione"),
              choices = unique(data_vacc_reg$regione),
              selected = "Lombardia"
            )
          ),
          hr(),
          HTML("Gli autori di questa applicazione sono <a href='https://www.unimib.it/matteo-maria-pelagatti'><b>Matteo Pelagatti</b></a> e <a href='https://tommasorigon.github.io'><b>Tommaso Rigon</b></a>, a cui è possibile scrivere per eventuali segnalazioni e richieste di chiarimenti."),
          hr(),
          HTML("Per ulteriori informazioni, si rimanda inoltre alla documentazione presente su questo sito.")
        ),

        mainPanel(
          tabsetPanel(
            tabPanel(
              "Evoluzione",
              hr(),
              dygraphOutput("vaccini"),
              hr()
            ),
            tabPanel(
              "Tabelle riassuntive",
              hr(),
              HTML(" Le tabelle riportano il numero di vaccini somministrati (prima tabella) e la percentuale rispetto alla popolazione nazionale / regionale (seconda tabella)."),
              hr(),
              DTOutput("tbl"),
              hr(),
              DTOutput("tbl2"),
              hr()
            )
          )
        )
      )
    ),

    # UI Documentazione -----------------------------------------
    tabPanel(
      "Documentazione",
      fluidRow(
        withMathJax(includeMarkdown("doc.Rmd"))
      )
    )
  )
)



# Define SERVER ---------------
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

  dati_google_trends_nazionale <- reactive({
    gtrend_data <- tibble(get_gtrend(keyword = "sintomi covid", region = "Italia", from = max_date - 180, to = max_date, output = "data.frame")) %>% transmute(date = date, Google = hits)
    trend <- get_cmp(gtrend_data)
    trend$livello <- trend$livello / max(trend$livello) * 100

    list(
      livello = xts(trend$livello, gtrend_data$date),
      crescita = xts(trend$crescita, gtrend_data$date)
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

  output$gtrends <- renderDygraph({
    data_plot <- dati_positivi_regione()$livello[, "Italia"]
    data_plot <- data_plot[(index(data_plot) >= max_date - 180) & (index(data_plot) <= max_date), ]
    data_plot$Italia <- data_plot$Italia / max(data_plot$Italia) * 100
    gtrend <- dati_google_trends_nazionale()$livello
    data_plot <- merge(gtrend, data_plot, all = TRUE)

    dygraph(data_plot, main = paste("Nuovi positivi vs Google trend"), ylab = "Scala arbitraria (max 100)") %>%
      dyOptions(colors = RColorBrewer::brewer.pal(8, "Dark2"), axisLineWidth = 1.5, fillGraph = TRUE, drawGrid = FALSE)
  })

  output$tassi_gtrends <- renderDygraph({
    data_plot <- dati_positivi_regione()$crescita[, "Italia"]
    data_plot <- data_plot[(index(data_plot) >= max_date - 180) & (index(data_plot) <= max_date), ]
    gtrend <- dati_google_trends_nazionale()$crescita
    data_plot <- merge(gtrend, data_plot, all = TRUE)

    dygraph(data_plot, main = paste("Tasso di crescita (%) - Nuovi positivi vs Google trend"), ylab = "Tasso di crescita (%)") %>%
      dyLimit(limit = 0, strokePattern = "dashed") %>%
      dyOptions(colors = RColorBrewer::brewer.pal(8, "Dark2"), axisLineWidth = 1.5, fillGraph = TRUE, drawGrid = FALSE)
  })

  output$tab_testo <- renderText({
    paste("Giorno considerato per le stime: ", input$date[2], ". <b> Nota</b>. La seguente tabella riporta il numero di casi / decessi stimati tramite Kalman filter e non il valore osservato.", sep = "")
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
    colnames(data_plot) <- c(paste("Vaccino -", regione), "Prima dose (% della popolazione)", "Seconda dose (% della popolazione)")
    datatable(data_plot, rownames = FALSE, options = list(pageLength = 22, dom = "t")) %>%
      formatRound(columns = 2:3, digits = 2)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
