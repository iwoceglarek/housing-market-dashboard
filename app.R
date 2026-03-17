library(shiny)
library(bslib)
library(ggplot2)
library(sf)

source("data.R")

ui <- page_fluid(
  
  title = "Rynek mieszkaniowy w Polsce",
  
  navset_tab(
    
    # Ceny mieszkań
    nav_panel(
      "Ceny mieszkań",
      layout_sidebar(
        sidebar = sidebar(
          selectizeInput("city", "Miasto:", 
                         choices = sort(unique(ceny_mieszkan$city)), 
                         multiple = TRUE, selected = c("Warszawa", "Kraków", "Wrocław", "Katowice", "Poznań")),
          sliderInput("years", "Lata:", 
                      min = min(ceny_mieszkan$year), 
                      max = max(ceny_mieszkan$year), 
                      value = c(min(ceny_mieszkan$year), 
                                max(ceny_mieszkan$year)),
                      step = 1, sep = '', ticks = FALSE),
          selectInput("inflation_adj", "Ceny:",
                      choices = c("nominalne", "realne"),
                      "nominalne"),
          selectInput("market", "Rynek:",
                      choices = c("pierwotny", "wtórny"),
                      "wtórny")),
        plotOutput("ceny_mieszkan_plot"))),
    
    # Dostępność mieszkań
    nav_panel(
      "Dostępność mieszkań",
      layout_columns(
        col_widths = c(3, 9),
        card(sliderInput("years2", "Rok:",
                         min = min(ceny_mieszkan$year) + 1,
                         max = max(ceny_mieszkan$year),
                         value = max(ceny_mieszkan$year),
                         step = 1, sep = '', ticks = FALSE),
             selectInput("market2", "Rynek:",
                         choices = c("pierwotny", "wtórny"),
                         "wtórny"),
             hr(),
             value_box(title = "Najmniej miesięcy pracy",
                       value = textOutput("min_city"),
                       theme = "bg-success"),
             
             value_box(title = "Najwięcej miesięcy pracy",
                       value = textOutput("max_city"),
                       theme = "bg-danger")),
        card(plotOutput("dostepnosc_mieszk_plot", 
                        height = "600px")))),
    
    # Relacja cen mieszkań do zarobków
    nav_panel(
      "Relacja cen mieszkań do zarobków",
      layout_sidebar(
        sidebar = sidebar(
          selectizeInput("city2", "Miasto:", 
            choices = sort(unique(ceny_mieszkan$city)), 
            multiple = TRUE, selected = c("Warszawa", "Kraków", "Wrocław", "Katowice", "Poznań")),
          sliderInput("years4", "Lata:", 
            min = min(ceny_mieszkan$year), 
            max = max(ceny_mieszkan$year), 
            value = c(min(ceny_mieszkan$year), 
                      max(ceny_mieszkan$year)),
            step = 1, sep = '', ticks = FALSE),
          selectInput("market3", "Rynek:",
                      choices = c("pierwotny", "wtórny"),
                      "wtórny"),
          selectInput("form", "Dane:",
                      choices = c("wartości nominalne", "zmiany procentowe", "miesiące pracy na 1 m²"),
                      "zmiany procentowe"),
          conditionalPanel(condition="input.form == 'zmiany procentowe'", 
                           checkboxInput("inflacja_checkbox", "Pokaż inflację", TRUE)), 
          conditionalPanel(condition="input.form == 'zmiany procentowe'", 
                           checkboxInput("wynagrodzenia_checkbox", "Pokaż wynagrodzenia", TRUE))), 
        plotOutput("ceny_mieszkan_zarobki_plot"))),
    
    # Migracje
    nav_panel(
      "Migracje",
      layout_sidebar(
        sidebar = sidebar(
          selectizeInput("region", "Województwo:", 
                         choices = sort(unique(ceny_1m2$region)), 
                         multiple = TRUE, selected = c(unique(ceny_1m2$region))),
          sliderInput("years3", "Rok:",
                      min = min(ceny_mieszkan$year),
                      max = max(ceny_mieszkan$year)-1,
                      value = max(ceny_mieszkan$year)-1,
                      step = 1, sep = '', ticks = FALSE),
          selectInput("market2", "Rynek:",
                      choices = c("pierwotny", "wtórny"),
                      "wtórny")),
        plotOutput("migracje_plot")))),
  
  # Stopka 
  div(class = "text-muted small",
      "Źródła danych: GUS, NBP, PRG | Opracowanie własne")
)

server <- function(input, output, session) {
  
  # Wykres dla "Ceny mieszkań"
  output$ceny_mieszkan_plot <- renderPlot({
    req(input$market, input$city)
    
    cc <- ceny_mieszkan[
      ceny_mieszkan$market == input$market &
        ceny_mieszkan$city %in% input$city &
        ceny_mieszkan$year >= input$years[1] &
        ceny_mieszkan$year <= input$years[2],]
    
    req(nrow(cc) > 0)
    
    y_col <- if (input$inflation_adj == "nominalne"){
      cc$val} 
    else{
      cc$val_inflation_adj
    }
    
    y_label <- if (input$inflation_adj == "realne") {
      "Cena m² (ceny stałe z roku 2006 Q3)"} 
    else{
      "Cena m² (ceny bieżące)"
    }
    
    ggplot(cc,
           aes(x = time, y =y_col, color = city)) +
      geom_line(linewidth = 1) +
      labs(title = paste0(
        "Ceny (", input$inflation_adj, ") mieszkań – rynek ", input$market),
        x = "Rok",
        y = y_label,
        color = "Miasto") +
      scale_y_continuous(
        labels = scales::label_number(big.mark = " ", suffix = " zł")) + 
      theme_minimal() +
      theme(legend.position = "right")
  })
  
  # Wykres dla "Dostępność mieszkań"
  output$dostepnosc_mieszk_plot <- renderPlot({
    req(input$years2, input$market2)
    
    ceny_1m2_sel <- ceny_1m2[
      ceny_1m2$qr == 1 &
        ceny_1m2$year == input$years2 &
        ceny_1m2$market == input$market2,]
    
    req(nrow(ceny_1m2_sel) > 0)
    
    ceny_region <- aggregate(
      miesiace_pracy_na_1m2 ~ region,
      data = ceny_1m2_sel,
      FUN = mean,
      na.rm = TRUE
    )
    
    mapa_ceny_1m2 <- merge(
      woj,
      ceny_region,
      by = "region",
      all.x = TRUE
    )
    
    ggplot(mapa_ceny_1m2) +
      geom_sf(aes(fill = miesiace_pracy_na_1m2)) +
      scale_fill_viridis_c(
        na.value = "grey90",
        option = "C",
        direction = -1
      ) +
      labs(fill = "Miesiące pracy na 1 m²",
           title = paste0(
             "Średnia liczba miesięcy pracy na 1 m² (",
             input$years2, " Q1) – rynek ", input$market2)) +
      theme_minimal() +
      theme(
        axis.title = element_blank(),
        axis.text  = element_blank(),
        axis.ticks = element_blank())
  })
  
  # Wykres dla "Migracje"
  output$migracje_plot <- renderPlot({
    migracje_sel <- migracje[
      migracje$year == input$years3 &
        migracje$region %in% input$region,
      c("region", "val")]
    names(migracje_sel)[2] <- "saldo_migracji"
    
    ceny_region <- aggregate( miesiace_pracy_na_1m2 ~ region, 
                              data = ceny_1m2[ceny_1m2$year == input$years3 & 
                                                ceny_1m2$market == input$market2, ], 
                              FUN = mean, 
                              na.rm = TRUE)
    
    migracje_ceny <- merge(
      migracje_sel,
      ceny_region,
      by = "region",
      all = FALSE
    )
    
    mapa_migracje_ceny <- merge(
      woj,
      migracje_ceny,
      by = "region",
      all.x = TRUE
    )
    
    ggplot(migracje_ceny,
           aes(x = miesiace_pracy_na_1m2,
               y = saldo_migracji,
               label = region)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
      geom_point(size = 3, color = "#2166ac") +
      geom_text(vjust = -0.7, size = 3) +
      labs(title = paste0(
        "Zależność migracji od dostępności mieszkań (", input$years3, ")"),
        x = paste0("Średnia liczba miesięcy pracy na 1 m² (rynek ", input$market2, ")"),
        y = "Saldo migracji") +
      theme_minimal()
  })

  # Wykres dla "Relacja cen mieszkań do zarobków"
  output$ceny_mieszkan_zarobki_plot <- renderPlot({
    req(input$city2, input$years4, input$market3, input$form)

    index_series <- function(x) {
      x / x[which(!is.na(x))[1]] * 100}
    
    ceny_sel <- ceny_mieszkan[
      ceny_mieszkan$city %in% input$city2 &
        ceny_mieszkan$market == input$market3 &
        ceny_mieszkan$year >= input$years4[1] &
        ceny_mieszkan$year <= input$years4[2],]
    req(nrow(ceny_sel) > 0)
    
    df_plot <- data.frame(
      time   = ceny_sel$time,
      value  = ceny_sel$val,
      series = ceny_sel$city,
      stringsAsFactors = FALSE
    )
    
    if (input$form == "zmiany procentowe") {
      df_plot$value <- ave(
        df_plot$value,
        df_plot$series,
        FUN = index_series)}
    
    if (input$form == "zmiany procentowe" && input$wynagrodzenia_checkbox) {
      
      place_sel <- wynagrodzenia[
        wynagrodzenia$region == "polska" &
          wynagrodzenia$year >= input$years4[1] &
          wynagrodzenia$year <= input$years4[2],]
      
      if (nrow(place_sel) > 1) {
        
        df_place <- data.frame(
          time   = place_sel$time,
          value  = place_sel$val,
          series = "Wynagrodzenia (Polska)",
          stringsAsFactors = FALSE
        )
        
        
        df_place$value <- index_series(df_place$value)
        df_plot <- rbind(df_plot, df_place)
      }
    }
    
    if (input$form == "zmiany procentowe" && input$inflacja_checkbox) {
      
      infl_sel <- inflacja[
        inflacja$region == "polska" &
          inflacja$year >= input$years4[1] &
          inflacja$year <= input$years4[2],]
      
      if (nrow(infl_sel) > 1) {
        
        df_infl <- data.frame(
          time   = infl_sel$time,
          value  = infl_sel$val_cumulative,
          series = "Inflacja (Polska)",
          stringsAsFactors = FALSE
        )
        
        
        df_infl$value <- index_series(df_infl$value)
        df_plot <- rbind(df_plot, df_infl)
      }
    }
    
    if (input$form == "miesiące pracy na 1 m²") {
      
      ggplot(
        ceny_1m2[
          ceny_1m2$city %in% input$city2 &
            ceny_1m2$market == input$market3 &
            ceny_1m2$year >= input$years4[1] &
            ceny_1m2$year <= input$years4[2],],
        aes(x = time_cena,
            y = miesiace_pracy_na_1m2,
            color = city,
            group = city)) +
        geom_line(linewidth = 1) +
        geom_point(size = 2) +
        labs(title = "Relacja cen mieszkań do zarobków",
             subtitle = "Liczba miesięcy pracy potrzebna na zakup 1 m²",
             x = "Rok",
             y = "Miesiące pracy na 1 m²",
             color = "Miasto") +
        theme_minimal()
      
    } else {
      
      df_plot <- df_plot[order(df_plot$series, df_plot$time), ]
      
      ggplot(
        df_plot,
        aes(x = time,
            y = value,
            color = series,
            group = interaction(series))) +
        geom_line(linewidth = 1) +
        geom_point(size = 2) +
        labs(title = "Relacja cen mieszkań, zarobków i inflacji",
             subtitle = ifelse(
               input$form == "zmiany procentowe",
               "Indeks (początek każdej serii = 100)",
               "Wartości nominalne"),
             x = "Rok",
             y = ifelse(
               input$form == "zmiany procentowe",
               "Indeks (%)",
               "Wartość"),
             color = "") +
        theme_minimal()
    }
  })
  
  # Reaktywny filtr danych ceny_1m2 dla value_box (min / max)
  ceny_sel <- reactive({
    ceny_1m2[
      ceny_1m2$year == input$years2 &
        ceny_1m2$qr == 1 &
        ceny_1m2$market == input$market2,]
  })
  
  # Generowanie tekstu dla value_box (min)
  output$min_city <- renderText({
    df <- ceny_sel()
    req(nrow(df) > 0)
    
    r <- df[which.min(df$miesiace_pracy_na_1m2), ]
    
    paste0(
      r$city, "\n",
      round(r$miesiace_pracy_na_1m2, 1), " mies."
    )
  })
  
  # Generowanie tekstu dla value_box (max)
  output$max_city <- renderText({
    df <- ceny_sel()
    req(nrow(df) > 0)
    
    r <- df[which.max(df$miesiace_pracy_na_1m2), ]
    
    paste0(
      r$city, "\n",
      round(r$miesiace_pracy_na_1m2, 1), " mies."
    )
  })
}

shinyApp(ui, server)



