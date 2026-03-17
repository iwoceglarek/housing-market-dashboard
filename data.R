# Wczytywanie i konwersja danych

# Źródła danych:
# GUS: inflacja, migracje, wynagrodzenia
# NBP: ceny mieszkań
# PRG: mapy województw

# Wczytanie danych z plików csv
inflacja_rawdata <- read.csv("./data/CENY_2496_CREL_20251227113054.csv", header = TRUE, sep = ";")
ceny_mieszkan_rpierw_rawdata <- read.csv("./data/mieszkania_r_pierwotny_c_transakcyjne.csv", header = TRUE, sep = ";")
ceny_mieszkan_rwtorn_rawdata <- read.csv("./data/mieszkania_r_wtorny_c_transakcyjne.csv", header = TRUE, sep = ";")
wynagrodzenia_rawdata <- read.csv("./data/WYNA_2687_CREL_20251227130025.csv", header = TRUE, sep = ";")
migracje_rawdata <- read.csv("./data/LUDN_1350_CREL_20251227131002.csv", header = TRUE, sep = ";")

# Funkcje pomocnicze 
time_calc <- function(year, qr) {
  year+(qr-0.5)/4
}

num_pl <- function(x) {
  as.numeric(gsub(",", ".", gsub(" ", "", x)))
}

# Konwersja danych GUS (inflacja, wynagrodzenia)
convert_gus <- function(raw_data) {
  data.frame(
    year   = as.integer(raw_data$Rok),
    qr     = as.integer(substr(raw_data$Okresy, 1, 1)),
    time   = time_calc(as.integer(raw_data$Rok),
                       as.integer(substr(raw_data$Okresy, 1, 1))),
    region = tolower(raw_data$Nazwa),
    val    = num_pl(raw_data$Wartosc),
    stringsAsFactors = FALSE
  )
}

inflacja <- convert_gus(inflacja_rawdata)
wynagrodzenia <- convert_gus(wynagrodzenia_rawdata)

# Konwersja danych migracje
convert_migracje <- function(raw_data) {
  data.frame(
    year   = as.integer(raw_data$Rok),
    time   = as.integer(raw_data$Rok),
    region = tolower(raw_data$Nazwa),
    val    = num_pl(raw_data$Wartosc),
    stringsAsFactors = FALSE
  )
}

migracje <- convert_migracje(migracje_rawdata)

# Konwersja danych NBP (ceny mieszkań)
convert_mieszkania <- function(raw_data, market_type) {
  result <- data.frame(
    year = integer(),
    qr = integer(),
    time = numeric(),
    city = character(),
    val = numeric(),
    market = character(),
    stringsAsFactors = FALSE
  )
  
  cities <- names(raw_data)[3:ncol(raw_data)]
  
  for (c in cities) {
    tmp <- data.frame(
      year   = as.integer(raw_data$Rok),
      qr     = as.integer(raw_data$Okres),
      time   = time_calc(as.integer(raw_data$Rok), as.integer(raw_data$Okres)),
      city   = c,
      val    = num_pl(raw_data[[c]]),
      market = market_type,
      stringsAsFactors = FALSE
    )
    result <- rbind(result, tmp)
  }
  return(result)
}

ceny_pierwotny <- convert_mieszkania(
  ceny_mieszkan_rpierw_rawdata,
  market_type = "pierwotny"
)
ceny_wtorny <- convert_mieszkania(
  ceny_mieszkan_rwtorn_rawdata,
  market_type = "wtórny"
)

ceny_mieszkan <- rbind(ceny_pierwotny, ceny_wtorny)

# Skumulowana inflacja
calc_cumulative_inflation <- function(df) {
  
  df <- df[order(df$region, df$time), ]
  df$val_cumulative <- 1
  
  for (r in unique(df$region)) {
    times <- unique(df$time[df$region == r])
    
    for (i in seq_along(times)) {
      df$val_cumulative[df$region == r & df$time == times[i]] <-
        if (i == 1) 1
      else df$val_cumulative[df$region == r & df$time == times[i - 1]] *
        (df$val[df$region == r & df$time == times[i]] / 100)
    }
  }
  
  df
}

inflacja <- calc_cumulative_inflation(inflacja)

# Realne ceny mieszkań (skorygowane o inflację)
calc_val_inflation_adj <- function(ceny_mieszkan, inflacja){
  infl_pol <- inflacja[inflacja$region=='polska',
                   c("year", "qr", "val_cumulative")]
  names(infl_pol)[3] <- "infl_cum"
  infl_pol <- infl_pol[order(infl_pol$year, infl_pol$qr), ]
  
  start <- ceny_mieszkan[
    order(ceny_mieszkan$year, ceny_mieszkan$qr),
  ][1, ]
  
  base_inflation_val <- infl_pol$infl_cum[
    infl_pol$year == start$year &
      infl_pol$qr   == start$qr
  ]
  
  infl_pol$infl_cum <- infl_pol$infl_cum / base_inflation_val * 100
  
  ceny_mieszkan <- merge(ceny_mieszkan, infl_pol, by = c("year", "qr"), all.x = TRUE)
  
  ceny_mieszkan$val_inflation_adj <- ceny_mieszkan$val*100/ceny_mieszkan$infl_cum
  ceny_mieszkan$infl_cum <- NULL
  
  return(ceny_mieszkan)
}

ceny_mieszkan <- calc_val_inflation_adj(ceny_mieszkan, inflacja)


# Mapy
woj <- st_read("./data/maps/A01_Granice_wojewodztw.shp")
woj$region <- tolower(woj$JPT_NAZWA_)

# Dostępność mieszkań (miesiące pracy na 1m2)
calc_ceny_1m2 <- function(ceny_mieszkan, wynagrodzenia) {
  
  ceny_mieszkan <- ceny_mieszkan[!grepl("^X", ceny_mieszkan$city), ]
  
  ceny_mieszkan$city <- gsub("\\.", " ", ceny_mieszkan$city)
  
  city_region_map <- data.frame(
    city = c(
      "Białystok", "Bydgoszcz", "Gdańsk", "Gdynia", "Katowice",
      "Kielce", "Kraków", "Lublin", "Łódź", "Olsztyn",
      "Opole", "Poznań", "Rzeszów", "Szczecin",
      "Warszawa", "Wrocław", "Zielona Góra"
    ),
    region = c(
      "podlaskie", "kujawsko-pomorskie", "pomorskie", "pomorskie", "śląskie",
      "świętokrzyskie", "małopolskie", "lubelskie", "łódzkie", "warmińsko-mazurskie",
      "opolskie", "wielkopolskie", "podkarpackie", "zachodniopomorskie",
      "mazowieckie", "dolnośląskie", "lubuskie"
    ),
    stringsAsFactors = FALSE
  )
  
  ceny_region <- merge(
    ceny_mieszkan,
    city_region_map,
    by = "city",
    all.x = TRUE
  )
  
  ceny_region <- ceny_region[!is.na(ceny_region$region), ]
  
  ceny_zarobki <- merge(
    ceny_region,
    wynagrodzenia,
    by = c("region", "year", "qr"),
    suffixes = c("_cena", "_placa"),
    all.x = TRUE
  )
  
  ceny_zarobki$miesiace_pracy_na_1m2 <-
    ceny_zarobki$val_cena / ceny_zarobki$val_placa
  
  ceny_zarobki[, c(
    "city",
    "region",
    "year",
    "qr",
    "time_cena",
    "market",
    "miesiace_pracy_na_1m2"
  )]
}

ceny_1m2 <- calc_ceny_1m2(
  ceny_mieszkan = ceny_mieszkan,
  wynagrodzenia = wynagrodzenia
)

# Cleanup
rm(inflacja_rawdata, wynagrodzenia_rawdata, migracje_rawdata, 
   ceny_mieszkan_rpierw_rawdata, ceny_mieszkan_rwtorn_rawdata,
   time_calc, num_pl, convert_gus, convert_migracje, convert_mieszkania, 
   ceny_pierwotny, ceny_wtorny, calc_cumulative_inflation, calc_val_inflation_adj,
   calc_ceny_1m2)
