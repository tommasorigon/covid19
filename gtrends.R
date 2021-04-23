

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

