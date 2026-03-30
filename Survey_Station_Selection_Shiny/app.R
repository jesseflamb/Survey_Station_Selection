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
      fileInput("file", "Upload Station CSV", accept = ".csv"),
      numericInput("ship_speed", "Ship Speed (knots):", value = 11.75, step = 0.25),
      dateTimeInput("dep_time", "Departure Time:", value = Sys.time()),
      hr(),
      actionButton("reset", "Reset Selection", class = "btn-danger"),
      br(), br(),
      helpText("Click stations on the map in the order you plan to visit them.")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Map", leafletOutput("map", height = "600px")),
        tabPanel("Ship's Log / Schedule", DTOutput("log_table"))
      )
    )
  )
)

# --- SERVER SECTION ---
server <- function(input, output, session) {
  
  # 1. Reactive Value to store selected stations in order
  selected_data <- reactiveValues(ids = c())
  
  # 2. Load Data
  raw_data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
    # Ensure column names match your script: Station, LAT, LON, Total_Gear_Time, Grid.ID, Gear.Sampled
  })
  
  # 3. Initial Map Render
  output$map <- renderLeaflet({
    req(raw_data())
    df <- raw_data()
    
    leaflet(df) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~LON, lat = ~LAT, 
        layerId = ~Station, # Use Station ID as the identifier
        label = ~as.character(Station),
        radius = 5, color = "blue", fillOpacity = 0.7
      )
  })
  
  # 4. Handle Map Clicks
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    # Append the clicked station ID to our ordered list
    selected_data$ids <- c(selected_data$ids, click$id)
    
    # Update map to show selection path
    df_ordered <- raw_data() %>%
      filter(Station %in% selected_data$ids) %>%
      slice(match(selected_data$ids, Station))
    
    proxy <- leafletProxy("map")
    
    if(nrow(df_ordered) > 1) {
      proxy %>%
        addPolylines(
          lng = df_ordered$LON, lat = df_ordered$LAT, 
          layerId = "path", color = "red", weight = 2
        )
    }
    
    # Highlight selected markers
    proxy %>%
      addCircleMarkers(
        lng = click$lng, lat = click$lat,
        color = "red", radius = 7, fillOpacity = 1
      )
  })
  
  # 5. Reset Logic
  observeEvent(input$reset, {
    selected_data$ids <- c()
    leafletProxy("map") %>% 
      clearShapes() %>% 
      removeShape("path")
  })
  
  # 6. Calculations (The "Ship's Log")
  output$log_table <- renderDT({
    req(length(selected_data$ids) > 0)
    
    # Get selected stations in order
    df_selected <- raw_data() %>%
      filter(Station %in% selected_data$ids) %>%
      slice(match(selected_data$ids, Station)) %>%
      mutate(Stn_Order = row_number())
    
    # Math logic from your RMD
    df_calc <- df_selected %>%
      mutate(
        # Distance calculation (using geosphere)
        Dist_NM = ifelse(row_number() == 1, 0, 
                         distGeo(cbind(LON, LAT), 
                                 cbind(lag(LON), lag(LAT))) / 1852),
        # Steam time in hours
        Steam_H = Dist_NM / input$ship_speed,
        # Operation time from your CSV (Total_Gear_Time)
        Ops_H = Total_Gear_Time
      )
    
    # Cumulative Time Logic
    times <- c()
    current_time <- as.POSIXct(input$dep_time)
    
    for(i in 1:nrow(df_calc)) {
      # Add steam time to arrive
      current_time <- current_time + hours(df_calc$Steam_H[i])
      # This is arrival time; now add ops time
      current_time <- current_time + hours(df_calc$Ops_H[i])
      times[i] <- as.character(current_time)
    }
    
    df_calc$Est_Completion <- times
    
    datatable(df_calc %>% select(Stn_Order, Station, Grid.ID, Dist_NM, Est_Completion),
              options = list(pageLength = 25))
  })
}

shinyApp(ui, server)