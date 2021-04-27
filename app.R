library(shiny)
library(shinythemes)
library(KFAS)
library(xts)
library(dygraphs)
library(DT)
library(tidyverse)
library(gtrendsR)
library(plotly)

# Carica le funzioni utili
source("funzioni.R")

# Carica dati di popolazione
pop_province <- read_csv("https://raw.githubusercontent.com/tommasorigon/covid19/main/province.csv")
pop_regioni  <- read_csv("https://raw.githubusercontent.com/tommasorigon/covid19/main/regioni.csv")

# Dati vaccini
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


# Carica dati regionali e provinciali
dt_reg <- load_regione()
dt_pro <- load_provincia()

# dati di riferimento
max_date <- max(dt_reg$positivi$data)
min_date <- min(dt_reg$positivi$data)

# normalizza rispetto a popolazione
dt_reg_pop <- lapply(dt_reg, divide_by_pop, pop = pop_regioni)
dt_pro_pop <- divide_by_pop(dt_pro, pop_province)

# Carica modelli state-space
load("modelli.Rdata") # mod_reg: lista con positivi, decessi, terapie_intensive con liste di modelli
                      # mod_pro: lista di modelli

# Passa lo smoother sui dati
smo_reg_pos <- smoother(dt_reg_pop$positivi, mod_reg$positivi)
smo_reg_dec <- smoother(dt_reg_pop$decessi, mod_reg$decessi)
smo_reg_icu <- smoother(dt_reg_pop$terapie_intensive, mod_reg$terapie_intensive)
smo_pro_pos <- smoother(dt_pro_pop, mod_pro)


# Define UI --------------------------------------------------------------
ui <- fluidPage(
  
  theme = shinytheme("journal"),
  
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
              choices = names(dt_reg$positivi)[-1], multiple = T,
              selected = c("Lombardia", "Lazio", "Campania")
            )
          ),
          
          conditionalPanel(
            condition = "input.datatype == 'Provinciale'",
            selectInput(
              inputId = "prov", label = strong("Seleziona provincia"),
              choices = sort(names(dt_pro)[-1]), multiple = T,
              selected = c("Milano", "Roma", "Napoli")
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
              HTML("I seguenti grafici fanno riferimento al <b>trend</b> ottenuto tramite <i>Kalman filter</i> e non ai valori osservati. Si consulti la documentazione per ulteriori informazioni sulla metodologia. "),
              hr(),
              dygraphOutput("casi"),
              hr(),
              dygraphOutput("tassi"),
              hr()
            ),
            tabPanel(
              "Ultimo dato disponibile (tabella)",
              hr(),
              textOutput("tab_testo"),
              hr(),
              DTOutput("tabella"),
              hr()
            ),
            tabPanel(
              "Ultimo dato disponibile (grafico)",
              hr(),
              plotlyOutput("dispersione"),
              hr()
            ),
            tabPanel(
              "Google trends",
              hr(),
              HTML("<b> Sperimentale</b>. I grafici di questa sezione mettono a confronto le serie storiche dei nuovi positivi (a livello nazionale) e i dati relativi a <a href = 'https://trends.google.com/trends/?geo=US'> <b> Google trends</b></a>, utilizzando la parola chiave 'sintomi covid'.
              <hr> Entrambe le serie storiche sono state opportunamente depurate tramite <i>Kalman filter</i>. Nel primo grafico le serie sono state riscalatate (ovvero divise per il massimo e moltiplicate per 100), per poter essere confrontabili. Attualmente sono disponibili solamente i <b> dati nazionali </b> degli ultimi 180 giorni. "),
              hr(),
              textInput("keyword", label = h4("Keyword google trends"), value = "Sintomi covid"),
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
              HTML("Il seguente grafico fa riferimento al <b>trend</b> ottenuto tramite <i>Kalman filter</i> e non ai valori osservati. Si consulti la documentazione per ulteriori informazioni sulla metodologia. "),
              hr(),
              HTML("<b> Vaccino J&J</b>. I dati a disposizione per il vaccino J&J sono ancora troppo esigui per poter stimare un trend. Il grafico corrispondente potrebbe quindi generare un errore. "),
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

  dati_dosi_totali_nazionale <- reactive({
    vaccino <- input$vaccine
    if (vaccino != "Tutti") {
      id_vaccino <- data_vacc_reg$fornitore %in% vaccino
    } else {
      id_vaccino <- rep(TRUE, nrow(data_vacc_reg))
    }

    data_plot <- data_vacc_reg[id_vaccino, ]
    data_plot <- aggregate(cbind(dosi_totali) ~ data, sum, data = data_plot)
    data_plot <- data_plot %>% transmute(data = data_plot$data, Italia = data_plot$dosi_totali)

    trend <- get_cmp(data_plot)
    list(
      livello = xts(trend$livello[, -1], trend$livello$data),
      crescita = xts(trend$crescita[, -1], trend$crescita$data)
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
      livello = xts(trend$livello[, -1], trend$livello$data),
      crescita = xts(trend$crescita[, -1], trend$crescita$data)
    )
  })

  dati_google_trends_nazionale <- reactive({
    gtrend_data <- get_gtrend(keyword = input$keyword,
                              region = "Italia",
                              from = max_date - 180,
                              to = max_date,
                              output = "data.frame") %>%
      transmute(data = date, Google = hits)
    trend <- get_cmp(gtrend_data)
    trend$livello[, -1] <- trend$livello[, -1] / max(trend$livello[, -1]) * 100

    list(
      livello  = xts(trend$livello[, -1],  trend$livello$data),
      crescita = xts(trend$crescita[, -1], trend$crescita$data)
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
        data_plot <- smo_reg_pos$livello[(smo_reg_pos$livello$data >= input$date[1]) &
                                         (smo_reg_pos$livello$data <= input$date[2]),
                                         c("data", region)]
      } else if (input$type == "Decessi") {
        type <- input$type
        data_plot <- smo_reg_dec$livello[(smo_reg_dec$livello$data >= input$date[1]) &
                                         (smo_reg_dec$livello$data <= input$date[2]),
                                         c("data", region)]
      } else {
        type <- input$type
        data_plot <- smo_reg_icu$livello[(smo_reg_icu$livello$data >= input$date[1]) &
                                         (smo_reg_icu$livello$data <= input$date[2]),
                                         c("data", region)]
      }
    } else {
      type <- "Nuovi positivi"
      data_plot <- smo_pro_pos$livello[(smo_pro_pos$livello$data >= input$date[1]) &
                                       (smo_pro_pos$livello$data <= input$date[2]),
                                       c("data", input$prov)]
    }


    data_plot <- xts(data_plot[, -1, drop = FALSE], data_plot$data)
    dygraph(data_plot, main = paste(type, "ogni 100.000 abitanti"),
            ylab = paste(type, "ogni 100.000 abitanti")) %>%
      dyOptions(colors = RColorBrewer::brewer.pal(8, "Dark2"), axisLineWidth = 1.5,
                fillGraph = TRUE, drawGrid = FALSE)
  })

  output$vaccini <- renderDygraph({
    if (input$datatype2 == "Nazionale") {
      data_plot <- dati_dosi_totali_nazionale()$livello
    } else if (input$datatype2 == "Regionale") {
      data_plot <- dati_dosi_totali_regione()$livello
    }

    dygraph(data_plot, main = paste("Dosi totali giornaliere somministrate")) %>%
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
        data_plot <- smo_reg_pos$crescita[(smo_reg_pos$crescita$data >= input$date[1]) &
                                          (smo_reg_pos$crescita$data <= input$date[2]),
                                         c("data", region)]
      } else if (input$type == "Decessi") {
        type <- input$type
        data_plot <- smo_reg_dec$crescita[(smo_reg_dec$crescita$data >= input$date[1]) &
                                          (smo_reg_dec$crescita$data <= input$date[2]),
                                         c("data", region)]
      } else {
        type <- input$type
        data_plot <- smo_reg_icu$crescita[(smo_reg_icu$crescita$data >= input$date[1]) &
                                          (smo_reg_icu$crescita$data <= input$date[2]),
                                         c("data", region)]
      }
    } else {
      type <- "Nuovi positivi"
      data_plot <- smo_pro_pos$crescita[(smo_pro_pos$crescita$data >= input$date[1]) &
                                        (smo_pro_pos$crescita$data <= input$date[2]),
                                       c("data", input$prov)]
    }
    
    data_plot <- xts(data_plot[, -1], data_plot$data)
    dygraph(data_plot,
      main = "Tasso di crescita giornaliero (%)", ylab = "Tasso di crescita giornaliero (%)"
    ) %>%
      dyLimit(limit = 0, strokePattern = "dashed") %>%
      dyOptions(colors = RColorBrewer::brewer.pal(8, "Dark2"), axisLineWidth = 1.5, fillGraph = TRUE, drawGrid = FALSE)
  })

  output$gtrends <- renderDygraph({
    data_plot <- smo_reg_pos$livello[smo_reg_pos$livello$data >= max_date - 180,
                                     c("data", "Italia")]
    data_plot$Italia <- data_plot$Italia / max(data_plot$Italia) * 100
    data_plot <- xts(data_plot$Italia, data_plot$data)
    gtrend <- dati_google_trends_nazionale()$livello
    data_plot <- cbind(Google = gtrend, Positivi = data_plot)

    dygraph(data_plot, main = paste("Nuovi positivi vs Google trend"), ylab = "Scala arbitraria (min 0, max 100)") %>%
      dyOptions(colors = RColorBrewer::brewer.pal(8, "Dark2"), axisLineWidth = 1.5, fillGraph = TRUE, drawGrid = FALSE)
  })

  output$tassi_gtrends <- renderDygraph({
    data_plot <- smo_reg_pos$crescita[smo_reg_pos$crescita$data >= max_date - 180,
                                     c("data", "Italia")]
    gtrend <- dati_google_trends_nazionale()$crescita
    data_plot <- xts(data_plot$Italia, data_plot$data)
    data_plot <- cbind(Google = gtrend, Positivi = data_plot)

    dygraph(data_plot, main = paste("Tasso di crescita (%) - Nuovi positivi vs Google trend"), ylab = "Tasso di crescita (%)") %>%
      dyLimit(limit = 0, strokePattern = "dashed") %>%
      dyOptions(colors = RColorBrewer::brewer.pal(8, "Dark2"), axisLineWidth = 1.5, fillGraph = TRUE, drawGrid = FALSE)
  })

  output$tab_testo <- renderText({
    paste("Giorno considerato per le stime: ", input$date[2], ". La seguente tabella riporta il numero di casi / decessi stimati tramite Kalman filter e non il valore osservato.", sep = "")
  })

  output$tabella <- renderDT({
    if (input$datatype == "Nazionale") {
      dt <- tibble(
        Regione = "Italia",
        `Casi per 100k abitanti` = as.numeric(tail(smo_reg_pos$livello[, "Italia"], 1)),
        `Crescita casi (%)` = as.numeric(tail(smo_reg_pos$crescita[, "Italia"], 1)),
        `Decessi per 100k abitanti` = as.numeric(tail(smo_reg_dec$livello[, "Italia"], 1)),
        `Crescita decessi (%)` = as.numeric(tail(smo_reg_dec$crescita[, "Italia"], 1)),
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
        Regione = colnames(smo_reg_pos$livello)[-1],
        `Casi per 100k abitanti` = as.numeric(tail(smo_reg_pos$livello[, -1], 1)),
        `Crescita casi (%)` = as.numeric(tail(smo_reg_pos$crescita[, -1], 1)),
        `Decessi per 100k abitanti` = as.numeric(tail(smo_reg_dec$livello[, -1], 1)),
        `Crescita decessi (%)` = as.numeric(tail(smo_reg_dec$crescita[, -1], 1)),
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
        Provincia = colnames(smo_pro_pos$livello)[-1],
        `Casi per 100k abitanti` = as.numeric(tail(smo_pro_pos$livello[, -1], 1)),
        `Crescita casi (%)` = as.numeric(tail(smo_pro_pos$crescita[, -1], 1)),
      ) %>%
        datatable(rownames = FALSE, options = list(pageLength = 110, dom = "t")) %>%
        formatStyle(
          columns = c(3, 3),
          color = styleInterval(0, c("black", "red"))
        ) %>%
        formatRound(columns = 2:3, digits = 2)
    }

    dt
  })

  output$dispersione <- renderPlotly({
    if (input$datatype == "Nazionale") return(NULL)
    if (input$datatype == "Regionale") {
      dt <- tibble(
        Regione = colnames(smo_reg_pos$livello)[-1],
        `Casi per 100k abitanti` = as.numeric(tail(smo_reg_pos$livello[, -1], 1)),
        `Crescita casi (%)` = as.numeric(tail(smo_reg_pos$crescita[, -1], 1)),
        `Decessi per 100k abitanti` = as.numeric(tail(smo_reg_dec$livello[, -1], 1)),
        `Crescita decessi (%)` = as.numeric(tail(smo_reg_dec$crescita[, -1], 1)),
        `Terapie intensive per 100k abitanti` = as.numeric(tail(smo_reg_icu$livello[, -1], 1)),
        `Crescita terapie intensive (%)` = as.numeric(tail(smo_reg_icu$crescita[, -1], 1)),
      ) %>% mutate_at(2:7, function(x) round(x, 2))
      dt$Selezione <- dt$Regione %in% input$region
      if (input$type == "Nuovi positivi") {
        plt <- ggplot(dt, aes(y = `Casi per 100k abitanti`,
                              x = `Crescita casi (%)`,
                              label = Regione,
                              color = Selezione)) + geom_text() + theme(legend.position = "none")
      } else if (input$type == "Decessi") {
        plt <- ggplot(dt, aes(y = `Decessi per 100k abitanti`,
                              x = `Crescita decessi (%)`,
                              label = Regione,
                              color = Selezione)) + geom_text() + theme(legend.position = "none")
      } else {
        plt <- ggplot(dt, aes(y = `Terapie intensive per 100k abitanti`,
                              x = `Crescita terapie intensive (%)`,
                              label = Regione,
                              color = Selezione)) + geom_text() + theme(legend.position = "none")
      }
    }
    
    if (input$datatype == "Provinciale") {
      dt <- tibble(
        Provincia = colnames(smo_pro_pos$livello)[-1],
        `Casi per 100k abitanti` = as.numeric(tail(smo_pro_pos$livello[, -1], 1)),
        `Crescita casi (%)` = as.numeric(tail(smo_pro_pos$crescita[, -1], 1)),
      ) %>% mutate_at(2:3, function(x) round(x, 1))
      dt$Selezione <- dt$Provincia %in% input$prov
      plt <- ggplot(dt, aes(y = `Casi per 100k abitanti`,
                   x = `Crescita casi (%)`,
                   label = Provincia,
                   color = Selezione)) +
        geom_text() + theme(legend.position = "none")
    }
    ggplotly(plt, dynamicTicks = TRUE)
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
    data_plot$prima_dose <- as.numeric(data_plot$prima_dose)
    data_plot$seconda_dose <- as.numeric(data_plot$seconda_dose)
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
