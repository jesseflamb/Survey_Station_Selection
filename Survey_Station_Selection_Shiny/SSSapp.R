library(shiny)
library(leaflet)
library(dplyr)
library(geosphere)
library(lubridate)
library(DT)

# --- UI SECTION ---
ui <- fluidPage(
  titlePanel("WGOA Survey Planner"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "1. Upload Station CSV", accept = ".csv"),
      numericInput("ship_speed", "2. Ship Speed (knots):", value = 11.75, step = 0.25),
      textInput("dep_time", "3. Departure (YYYY-MM-DD HH:MM):", 
                value = format(Sys.time(), "%Y-%m-%d %H:%M")),
      
      hr(),
      div(style="display:inline-block; width:48%;", 
          actionButton("undo", "Undo Last", class = "btn-warning", style="width:100%")),
      div(style="display:inline-block; width:48%;", 
          actionButton("reset", "Clear All", class = "btn-danger", style="width:100%")),
      
      br(), br(),
      downloadButton("download_log", "Download Ship's Log", class = "btn-success", style="width:100%"),
      hr(),
      helpText("Click stations on the map in order. The path and log will update automatically.")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Map", 
                 leafletOutput("map", height = "700px")),
        tabPanel("Ship's Log / Schedule", 
                 br(),
                 uiOutput("summary_stats"), # New Summary Section
                 hr(),
                 DTOutput("log_table"))
      )
    )
  )
)

# --- SERVER SECTION ---
server <- function(input, output, session) {
  
  selected_data <- reactiveValues(ids = c())
  
  raw_data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  # Initial Map Render (The static base layer)
  output$map <- renderLeaflet({
    req(raw_data())
    leaflet(raw_data()) %>%
      addTiles() %>%
      addCircleMarkers(lng = ~LON, lat = ~LAT, layerId = ~Station, 
                       label = ~paste("Sta:", Station, "| Grid:", Grid.ID), 
                       radius = 5, color = "blue", fillOpacity = 0.5, weight = 1)
  })
  
  # Handle Map Clicks
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    selected_data$ids <- c(selected_data$ids, click$id)
  })
  
  # UNDO Logic
  observeEvent(input$undo, {
    if (length(selected_data$ids) > 0) {
      selected_data$ids <- selected_data$ids[-length(selected_data$ids)]
    }
  })
  
  # RESET Logic
  observeEvent(input$reset, {
    selected_data$ids <- c()
  })
  
  # Map Updates (The dynamic "Selection" layer)
  observe({
    req(raw_data())
    proxy <- leafletProxy("map")
    
    # CRITICAL FIX: Clear the specific group "selection" 
    # This removes both the red lines and the red dots
    proxy %>% clearGroup("selection")
    
    if (length(selected_data$ids) > 0) {
      df_path <- raw_data() %>%
        filter(Station %in% selected_data$ids) %>%
        slice(match(selected_data$ids, Station))
      
      # Add Red Lines to "selection" group
      if (nrow(df_path) > 1) {
        proxy %>% addPolylines(lng = df_path$LON, lat = df_path$LAT, 
                               group = "selection", color = "red", weight = 3)
      }
      
      # Add Red Dots to "selection" group
      proxy %>% addCircleMarkers(lng = df_path$LON, lat = df_path$LAT, 
                                 group = "selection", color = "red", 
                                 radius = 6, fillOpacity = 0.8)
    }
  })
  
  # Calculation Logic
  processed_log <- reactive({
    req(length(selected_data$ids) > 0)
    start_time <- ymd_hm(input$dep_time, quiet = TRUE)
    if(is.na(start_time)) start_time <- Sys.time() 
    
    df_calc <- raw_data() %>%
      filter(Station %in% selected_data$ids) %>%
      slice(match(selected_data$ids, Station)) %>%
      mutate(
        Stn_Order = row_number(),
        Dist_NM = ifelse(row_number() == 1, 0, 
                         distGeo(cbind(LON, LAT), cbind(lag(LON), lag(LAT))) / 1852),
        Steam_H = Dist_NM / input$ship_speed,
        Ops_H = as.numeric(Total_Gear_Time)
      )
    
    arrival_times <- as.POSIXct(rep(NA, nrow(df_calc)))
    departure_times <- as.POSIXct(rep(NA, nrow(df_calc)))
    current_time <- start_time
    
    for(i in 1:nrow(df_calc)) {
      current_time <- current_time + dhours(df_calc$Steam_H[i])
      arrival_times[i] <- current_time
      current_time <- current_time + dhours(df_calc$Ops_H[i])
      departure_times[i] <- current_time
    }
    
    df_calc$Est_Arrival <- format(arrival_times, "%Y-%m-%d %H:%M")
    df_calc$Est_Departure <- format(departure_times, "%Y-%m-%d %H:%M")
    df_calc$Ops_Min <- round(df_calc$Ops_H * 60, 1)
    df_calc$Travel_Min <- round(df_calc$Steam_H * 60, 1)
    
    return(df_calc)
  })
  
  # NEW: Summary Stats UI
  output$summary_stats <- renderUI({
    req(processed_log())
    df <- processed_log()
    total_dist <- round(sum(df$Dist_NM), 1)
    final_eta <- tail(df$Est_Departure, 1)
    
    wellPanel(
      fluidRow(
        column(6, h4(strong("Total Distance:")), h3(total_dist, "NM")),
        column(6, h4(strong("Final Completion:")), h3(final_eta))
      )
    )
  })
  
  # Table Render
  output$log_table <- renderDT({
    validate(need(length(selected_data$ids) > 0, "Select stations to begin."))
    datatable(processed_log() %>% 
                select(Stn_Order, Station, Grid.ID, Travel_Min, Est_Arrival, Ops_Min, Est_Departure),
              colnames = c("Order", "Sta #", "Grid ID", "Travel (min)", "Arrive", "Ops (min)", "Depart"),
              options = list(pageLength = 50, scrollX = TRUE), rownames = FALSE)
  })
  
  output$download_log <- downloadHandler(
    filename = function() { paste0("Survey_Schedule_", Sys.Date(), ".csv") },
    content = function(file) { write.csv(processed_log(), file, row.names = FALSE) }
  )
}

shinyApp(ui, server)