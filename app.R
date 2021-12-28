library(shiny)
library(shinythemes)
library(KFAS)
library(xts)
library(dygraphs)
library(DT)
library(dplyr)
library(tidyr)
library(gtrendsR)
library(plotly)
library(data.table)

# Carica le funzioni utili
source("funzioni.R")

# Carica dati di popolazione
pop_province <- fread("https://raw.githubusercontent.com/tommasorigon/covid19/main/province.csv")
pop_regioni <- fread("https://raw.githubusercontent.com/tommasorigon/covid19/main/regioni.csv")
pop_regioni_eta <- fread("https://raw.githubusercontent.com/tommasorigon/covid19/main/regioni_eta.csv")

# Carica dati regionali e provinciali
dt_reg <- load_regione()
dt_pro <- load_provincia()
dt_vacc <- load_vaccini()

# dati di riferimento
max_date <- max(dt_reg$positivi$data)
min_date <- min(dt_reg$positivi$data)

# normalizza rispetto a popolazione
dt_reg_pop <- lapply(dt_reg, divide_by_pop, pop = pop_regioni)
dt_pro_pop <- divide_by_pop(dt_pro, pop_province)

dt_vacc_eta <- vaccini_eta(dt_vacc, pop_regioni_eta) 
dt_vacc_reg <- vaccini_reg(dt_vacc, pop_regioni_eta) 

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
  theme = shinytheme("cosmo"),
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
          conditionalPanel(
            condition = "input.type == 'Nuovi positivi'",
            checkboxInput("R_t", "Visualizza la stima numero di riproduzione R_t basata sul tasso di crescita giornaliero. ", FALSE)),
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
              HTML("I seguenti grafici fanno riferimento al <b>trend</b> ottenuto tramite <i>Kalman smoother</i> e non ai valori osservati. Si consulti la documentazione per ulteriori informazioni sulla metodologia. "),
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
              conditionalPanel(
                condition = "input.datatype == 'Nazionale'",
                HTML("Non è possibile produrre il grafico quando viene selezionato il livello di aggregazione nazionale."),
                hr()),
              plotlyOutput("dispersione"),
              hr()
            ),
            tabPanel(
              "Google trends",
              hr(),
              HTML("<b>Sperimentale</b>. I grafici di questa sezione mettono a confronto le serie storiche dei nuovi positivi (a livello nazionale e regionale) e i dati relativi a <a href = 'https://trends.google.com/trends/?geo=US'> <b> Google trends</b></a>, utilizzando la parola chiave 'sintomi covid' (modificabile). Sebbene le correlazioni identificate siano molto suggestive, si raccomanda una certa cautela nella loro interpretazione; si veda ad esempio <a href ='https://science.sciencemag.org/content/343/6176/1203#aff-1'>questo articolo scientifico</a> riguardante Google Flu.
              <hr> Entrambe le serie storiche sono state opportunamente depurate tramite <i>Kalman smoother</i>. Nel primo grafico le serie sono state riscalatate (entrambe variano tra 0 e 100), per poter essere confrontabili. Sono disponibili solamente i <b> dati nazionali </b> e i <b> dati regionali </b> degli ultimi 180 giorni. Le ricerche Google sono informative solo per le regioni più grandi."),
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




    # # UI Vaccinazione -------------------------------------------------
    # tabPanel(
    #   "Vaccinazione",
    #   sidebarLayout(
    #     sidebarPanel(
    #       selectInput(
    #         inputId = "eta", label = strong("Seleziona fascia anagrafica"),
    #         choices = c("Tutte", levels(dt_vacc$fascia_anagrafica)), multiple = T,
    #         selected = c("Tutte", "70-79", "80-89")
    #       ),
    #       hr(),
    #       HTML("Gli autori di questa applicazione sono <a href='https://www.unimib.it/matteo-maria-pelagatti'><b>Matteo Pelagatti</b></a> e <a href='https://tommasorigon.github.io'><b>Tommaso Rigon</b></a>, a cui è possibile scrivere per eventuali segnalazioni e richieste di chiarimenti."),
    #       hr(),
    #       HTML("Per ulteriori informazioni, si rimanda inoltre alla documentazione presente su questo sito.")
    #     ),
    #     mainPanel(
    #       tabsetPanel(
    #         tabPanel(
    #           "Fasce d'età",
    #           hr(),
    #           HTML("Nel grafico sottostante è riportato il numero cumulato percentuale di vaccinazioni effettuate (dato grezzo). Per <b>ciclo vaccinale completo</b> si intende il numero di persone a cui è stata somministrata la seconda dose dei vaccini Pfizer, Astrazeneca, Moderna, a cui si aggiunge il numero di persone a cui è stata somministrata la prima dose del vaccino Janssen (J&J). Sono esclusi dalla <b>popolazione</b> di riferimento i residenti appartenenti alla fascia d'età 0-11."),
    #           hr(),
    #           dygraphOutput("vaccini"),
    #           hr(),
    #           DTOutput("tbl_eta"),
    #           hr()
    #         ),
    #         tabPanel(
    #           "Somministrazioni giornaliere",
    #           hr(),
    #           HTML("Nel grafico sottostante è riportato il <b>numero giornaliero di dosi somministrate</b> a livello nazionale (dato grezzo), indipendentemente dalla tipologia di vaccino, dalle fasce d'età  e considerando sia le prime dosi che le seconde dosi. L'obiettivo dichiarato dal commissario straordinario per l'emergenza Covid è di somministrare circa <b>500.000 dosi giornaliere.</b>" ),
    #           hr(),
    #           HTML("I dati relativi all'ultimo giorno disponibile vanno interpretati con una certa cautela, perchè vengono resi pubblici ad orari diversi da regione a regione. I dati tipicamente si consolidano e diventano definitivi nell'arco di 24h." ),
    #           hr(),
    #           dygraphOutput("somministrazioni"),
    #           hr()
    #         ),
    #         tabPanel(
    #           "Regioni",
    #           hr(),
    #           HTML("Per <b>ciclo vaccinale completo</b> si intende il numero di persone a cui è stata somministrata la seconda dose dei vaccini Pfizer, Astrazeneca, Moderna, a cui si aggiunge il numero di persone a cui è stata somministrata la prima dose del vaccino Janssen (J&J). Sono esclusi dalla <b>popolazione</b> di riferimento i residenti appartenenti alla fascia d'età 0-11."),
    #           hr(),
    #           DTOutput("tbl_reg"),
    #           hr()
    #         ),
    #         tabPanel(
    #           "Tipologia vaccino",
    #           hr(),
    #           HTML("Per <b>ciclo vaccinale completo</b> si intende il numero di persone a cui è stata somministrata la seconda dose dei vaccini Pfizer, Astrazeneca, Moderna, a cui si aggiunge il numero di persone a cui è stata somministrata la prima dose del vaccino Janssen (J&J). Sono esclusi dalla <b>popolazione</b> di riferimento i residenti appartenenti alla fascia d'età 0-11."),
    #           hr(),
    #           DTOutput("tbl_vacc1")
    #         )
    #       )
    #     )
    #   )
    # ),
    # 
    # # UI Documentazione -----------------------------------------
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

  dati_google_trends_nazionale <- reactive({
    gtrend_data <- get_gtrend(
      keyword = input$keyword,
      region = if (input$datatype == "Regionale") input$region[1] else "Italia",
      from = max_date - 180,
      to = max_date,
      output = "data.frame"
    )
    gtrend_data <- gtrend_data %>% select(date, Google = hits)
    trend <- get_cmp(gtrend_data)
    trend$livello <- trend$livello %>% mutate_at(-1, min_max_norm)

    list(
      livello  = xts(trend$livello[, -1,  drop = F], trend$livello[, 1]),
      crescita = xts(trend$crescita[, -1, drop = F], trend$crescita[, 1])
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
        data_plot <- smo_reg_pos$livello[
          (smo_reg_pos$livello$data >= input$date[1]) &
            (smo_reg_pos$livello$data <= input$date[2]),
          c("data", region)
        ]
      } else if (input$type == "Decessi") {
        type <- input$type
        data_plot <- smo_reg_dec$livello[
          (smo_reg_dec$livello$data >= input$date[1]) &
            (smo_reg_dec$livello$data <= input$date[2]),
          c("data", region)
        ]
      } else {
        type <- input$type
        data_plot <- smo_reg_icu$livello[
          (smo_reg_icu$livello$data >= input$date[1]) &
            (smo_reg_icu$livello$data <= input$date[2]),
          c("data", region)
        ]
      }
    } else {
      type <- "Nuovi positivi"
      data_plot <- smo_pro_pos$livello[
        (smo_pro_pos$livello$data >= input$date[1]) &
          (smo_pro_pos$livello$data <= input$date[2]),
        c("data", input$prov)
      ]
    }


    data_plot <- xts(data_plot[, -1, drop = FALSE], data_plot$data)
    dygraph(data_plot,
      main = paste(type, "ogni 100.000 abitanti"),
      ylab = paste(type, "ogni 100.000 abitanti")
    ) %>%
      dyOptions(
        colors = RColorBrewer::brewer.pal(8, "Dark2"), axisLineWidth = 1.5,
        fillGraph = TRUE, drawGrid = FALSE
      )
  })

  output$vaccini <- renderDygraph({
    

    data_plot <- dt_vacc_eta$ciclo_concluso

    data_plot <- xts(data_plot[input$eta], data_plot$data)
    dygraph(data_plot,
            main = "Popolazione vaccinata (%) - Ciclo vaccinale completo",
            ylab = paste("Popolazione vaccinata (%)")
    ) %>%
      dyOptions(
        colors = RColorBrewer::brewer.pal(8, "Dark2"), axisLineWidth = 1.5,
        fillGraph = TRUE, drawGrid = FALSE
      )
  })
  
  output$somministrazioni <- renderDygraph({
    
    data_plot <- aggregate(dosi_totali ~ data, sum, data = dt_vacc)
    data_plot <- xts(tibble(`Dosi` = data_plot$dosi_totali), data_plot$data)
    dygraph(data_plot,
            main = paste("Dosi somministrate al giorno (Italia)"),
            ylab = paste("Dosi somministrate")
    ) %>% dyLimit(limit = 500000, strokePattern = "dashed") %>%
      dyOptions(
        colors = RColorBrewer::brewer.pal(8, "Dark2"), axisLineWidth = 1.5,
        fillGraph = TRUE, drawGrid = FALSE
      )
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
        data_plot <- smo_reg_pos$crescita[
          (smo_reg_pos$crescita$data >= input$date[1]) &
            (smo_reg_pos$crescita$data <= input$date[2]),
          c("data", region)
        ]
      } else if (input$type == "Decessi") {
        type <- input$type
        data_plot <- smo_reg_dec$crescita[
          (smo_reg_dec$crescita$data >= input$date[1]) &
            (smo_reg_dec$crescita$data <= input$date[2]),
          c("data", region)
        ]
      } else {
        type <- input$type
        data_plot <- smo_reg_icu$crescita[
          (smo_reg_icu$crescita$data >= input$date[1]) &
            (smo_reg_icu$crescita$data <= input$date[2]),
          c("data", region)
        ]
      }
    } else {
      type <- "Nuovi positivi"
      data_plot <- smo_pro_pos$crescita[
        (smo_pro_pos$crescita$data >= input$date[1]) &
          (smo_pro_pos$crescita$data <= input$date[2]),
        c("data", input$prov)
      ]
    }
    
    if(input$R_t & type == "Nuovi positivi"){
      data_plot <- xts(r_t(data_plot[, -1, drop = FALSE]), data_plot$data)
      p <- dygraph(data_plot,
                   main = "Numero di riproduzione R_t", ylab = "R_t"
      ) %>%
        dyLimit(limit = 1, strokePattern = "dashed") %>%
        dyOptions(colors = RColorBrewer::brewer.pal(8, "Dark2"), axisLineWidth = 1.5, fillGraph = TRUE, drawGrid = FALSE)
    } else {
      data_plot <- xts(data_plot[, -1, drop = FALSE], data_plot$data)
      p <- dygraph(data_plot,
        main = "Tasso di crescita giornaliero (%)", ylab = "Tasso di crescita giornaliero (%)"
      ) %>%
        dyLimit(limit = 0, strokePattern = "dashed") %>%
        dyOptions(colors = RColorBrewer::brewer.pal(8, "Dark2"), axisLineWidth = 1.5, fillGraph = TRUE, drawGrid = FALSE)
    } 
    p
  })

  output$gtrends <- renderDygraph({
    territorio <- if (input$datatype == "Regionale") input$region[1] else "Italia"
    smo <- switch(input$type,
                  `Nuovi positivi` = smo_reg_pos,
                  `Decessi` = smo_reg_dec,
                  smo_reg_icu)
    
    data_plot <- smo$livello[
      smo$livello$data >= max_date - 180,
      c("data", territorio)
    ]

    data_plot <- data_plot %>% mutate_at(-1, min_max_norm)
    data_plot <- cbind(xts(data_plot[, -1, drop = FALSE], data_plot[, 1]),
                       dati_google_trends_nazionale()$livello)
    titolo <- paste(input$type, "vs Google trend")
    dygraph(data_plot, main = titolo, ylab = "Scala arbitraria (min 0, max 100)") %>%
      dyOptions(colors = RColorBrewer::brewer.pal(8, "Dark2"), axisLineWidth = 1.5, fillGraph = TRUE, drawGrid = FALSE)
  })

  output$tassi_gtrends <- renderDygraph({
    territorio <- if (input$datatype == "Regionale") input$region[1] else "Italia"
    smo <- switch(input$type,
                  `Nuovi positivi` = smo_reg_pos,
                  `Decessi` = smo_reg_dec,
                  smo_reg_icu)
    
    data_plot <- smo$crescita[
      smo$crescita$data >= max_date - 180,
      c("data", territorio)
    ]
    
    data_plot <- cbind(xts(data_plot[, -1, drop = FALSE], data_plot[, 1]),
                       dati_google_trends_nazionale()$crescita)
    titolo <- paste("Tasso di crescita (%)", input$type, "vs Google trend")
    dygraph(data_plot, main = titolo, ylab = "Tasso di crescita (%)") %>%
      dyLimit(limit = 0, strokePattern = "dashed") %>%
      dyOptions(colors = RColorBrewer::brewer.pal(8, "Dark2"), axisLineWidth = 1.5, fillGraph = TRUE, drawGrid = FALSE)
  })

  output$tab_testo <- renderText({
    paste("Giorno considerato per le stime: ", input$date[2], ". La seguente tabella riporta il numero di casi / decessi stimati tramite Kalman smoother e non il valore osservato.", sep = "")
  })

  output$tabella <- renderDT({
    if (input$datatype == "Nazionale") {
      dt <- tibble(
        Regione = "Italia",
        `Casi per 100k abitanti` = as.numeric(tail(smo_reg_pos$livello[, "Italia"], 1)),
        `Crescita casi (%)` = as.numeric(tail(smo_reg_pos$crescita[, "Italia"], 1)),
        `Indice R_t` = r_t(as.numeric(tail(smo_reg_pos$crescita[, "Italia"], 1))),
        `Decessi per 100k abitanti` = as.numeric(tail(smo_reg_dec$livello[, "Italia"], 1)),
        `Crescita decessi (%)` = as.numeric(tail(smo_reg_dec$crescita[, "Italia"], 1)),
      ) %>%
        datatable(rownames = FALSE, options = list(pageLength = 22, dom = "t")) %>%
        formatStyle(
          columns = c(3, 6),
          color = styleInterval(0, c("black", "red"))
        ) %>%
        formatStyle(
          columns = 4,
          color = styleInterval(1, c("black", "red"))
        ) %>%
        formatRound(columns = 2:6, digits = 2)
    }

    if (input$datatype == "Regionale") {
      dt <- tibble(
        Regione = colnames(smo_reg_pos$livello)[-1],
        `Casi per 100k abitanti` = as.numeric(tail(smo_reg_pos$livello[, -1], 1)),
        `Crescita casi (%)` = as.numeric(tail(smo_reg_pos$crescita[, -1], 1)),
        `Indice R_t` = r_t(as.numeric(tail(smo_reg_pos$crescita[, -1], 1))),
        `Decessi per 100k abitanti` = as.numeric(tail(smo_reg_dec$livello[, -1], 1)),
        `Crescita decessi (%)` = as.numeric(tail(smo_reg_dec$crescita[, -1], 1)),
      ) %>%
        datatable(rownames = FALSE, options = list(pageLength = 22, dom = "t")) %>%
        formatStyle(
          columns = c(3, 6),
          color = styleInterval(0, c("black", "red"))
        ) %>%
        formatStyle(
          columns = 4,
          color = styleInterval(1, c("black", "red"))
        ) %>%
        formatRound(columns = 2:6, digits = 2)
    }

    if (input$datatype == "Provinciale") {
      dt <- tibble(
        Provincia = colnames(smo_pro_pos$livello)[-1],
        `Casi per 100k abitanti` = as.numeric(tail(smo_pro_pos$livello[, -1], 1)),
        `Crescita casi (%)` = as.numeric(tail(smo_pro_pos$crescita[, -1], 1)),
        `Indice R_t` = r_t(as.numeric(tail(smo_pro_pos$crescita[, -1], 1))),
      ) %>%
        datatable(rownames = FALSE, options = list(pageLength = 110, dom = "t")) %>%
        formatStyle(
          columns = 3,
          color = styleInterval(0, c("black", "red"))
        ) %>%
        formatStyle(
          columns = 4,
          color = styleInterval(1, c("black", "red"))
        ) %>%
        formatRound(columns = 2:4, digits = 2)
    }

    dt
  })

  output$dispersione <- renderPlotly({
    if (input$datatype == "Nazionale") {
      return(NULL)
    }
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
        plt <- ggplot(dt, aes(
          y = `Casi per 100k abitanti`,
          x = `Crescita casi (%)`,
          label = Regione,
          color = Selezione
        )) +
          geom_text() + theme_light() + scale_color_brewer(palette = "Dark2") +
          theme(legend.position = "none")
      } else if (input$type == "Decessi") {
        plt <- ggplot(dt, aes(
          y = `Decessi per 100k abitanti`,
          x = `Crescita decessi (%)`,
          label = Regione,
          color = Selezione
        )) +
          geom_text() + theme_light() + scale_color_brewer(palette = "Dark2") +
          theme(legend.position = "none")
      } else {
        plt <- ggplot(dt, aes(
          y = `Terapie intensive per 100k abitanti`,
          x = `Crescita terapie intensive (%)`,
          label = Regione,
          color = Selezione
        )) +
          geom_text() + theme_light() + scale_color_brewer(palette = "Dark2") + 
          theme(legend.position = "none")
      }
    }

    if (input$datatype == "Provinciale") {
      dt <- tibble(
        Provincia = colnames(smo_pro_pos$livello)[-1],
        `Casi per 100k abitanti` = as.numeric(tail(smo_pro_pos$livello[, -1], 1)),
        `Crescita casi (%)` = as.numeric(tail(smo_pro_pos$crescita[, -1], 1)),
      ) %>% mutate_at(2:3, function(x) round(x, 1))
      dt$Selezione <- dt$Provincia %in% input$prov
      plt <- ggplot(dt, aes(
        y = `Casi per 100k abitanti`,
        x = `Crescita casi (%)`,
        label = Provincia,
        color = Selezione
      )) + 
        geom_text(size=2.5) + theme_light() + scale_color_brewer(palette = "Dark2") +
        theme(legend.position = "none")
    }
    ggplotly(plt, dynamicTicks = TRUE, tooltip = c("x", "y", "label"), height = 500) %>% config(displayModeBar = F)
  })

  output$tbl_eta <- renderDT({
    data_tbl <- rbind(tail(dt_vacc_eta$ciclo_concluso_int, 1),
                      tail(dt_vacc_eta$ciclo_concluso, 1))[, -1]
    data_tbl <- data.frame(t(data_tbl)[c(9,1:8,10),])
    colnames(data_tbl) <- c("Ciclo vaccinale completo", "Ciclo vaccinale completo (%)")
    data_tbl$Popolazione <- data_tbl$`Ciclo vaccinale completo` / data_tbl$`Ciclo vaccinale completo (%)` * 100
    data_tbl <- data_tbl[, c(3, 1:2)]
  
    datatable(data_tbl, rownames = T, options = list(pageLength = 22, dom = "t")) %>% formatRound(columns = c(1, 2), digits = 0, interval = 3, mark = ",") %>% formatRound(columns = 3, digits = 2)
  })
  
  output$tbl_reg <- renderDT({
    datatable(dt_vacc_reg, rownames = F, options = list(pageLength = 22, dom = "t")) %>% formatRound(columns = c(2,3), digits = 0, interval = 3, mark = ",") %>% formatRound(columns = 4, digits = 2)
  })

  output$tbl_vacc1 <- renderDT({

    data_plot <- aggregate(dosi_totali ~ fornitore, sum, data = dt_vacc)
    data_plot <- rbind(data_plot, c("Totale", sum(data_plot$dosi_totali)))
    colnames(data_plot) <- c("Vaccino",  "Dosi somministrate")
    datatable(data_plot, rownames = FALSE, options = list(pageLength = 22, dom = "t")) %>% formatRound(columns = 2, digits = 0, interval = 3, mark = ",")
  })

}

# Run the application
shinyApp(ui = ui, server = server)
