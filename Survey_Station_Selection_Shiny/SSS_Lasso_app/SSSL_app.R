# Load necessary libraries
library(shiny)
library(leaflet)
library(leaflet.extras) # For the drawing tools
library(dplyr)
library(geosphere)
library(lubridate)
library(DT)
library(TSP)     # For the optimization engine
library(sf)      # For "Point-in-Polygon" selection

# --- UI SECTION ---
ui <- fluidPage(
  titlePanel("WGOA Survey Optimizer (v2.0)"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "1. Upload Station CSV", accept = ".csv"),
      numericInput("ship_speed", "2. Ship Speed (knots):", value = 11.75, step = 0.25),
      textInput("dep_time", "3. Departure (YYYY-MM-DD HH:MM):", 
                value = format(Sys.time(), "%Y-%m-%d %H:%M")),
      
      hr(),
      h4("Route Controls"),
      actionButton("optimize", "Auto-Optimize Route", class = "btn-primary", style="width:100%"),
      br(), br(),
      div(style="display:inline-block; width:48%;", 
          actionButton("undo", "Undo Last", class = "btn-warning", style="width:100%")),
      div(style="display:inline-block; width:48%;", 
          actionButton("reset", "Clear All", class = "btn-danger", style="width:100%")),
      
      br(), br(),
      downloadButton("download_log", "Download Optimized Log", class = "btn-success", style="width:100%"),
      hr(),
      helpText("HOW TO SWEEP: Use the polygon tool (square or pentagon icon) on the map to draw a shape around multiple stations. They will all be added at once.")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Map & Selection", 
                 leafletOutput("map", height = "700px")),
        tabPanel("Optimized Ship's Log", 
                 br(),
                 uiOutput("summary_stats"),
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
  
  # --- MAP RENDER WITH DRAWING TOOLS ---
  output$map <- renderLeaflet({
    req(raw_data())
    leaflet(raw_data()) %>%
      addTiles() %>%
      addCircleMarkers(lng = ~LON, lat = ~LAT, layerId = ~Station, 
                       label = ~paste("Sta:", Station, "| Grid:", Grid.ID), 
                       radius = 5, color = "blue", fillOpacity = 0.5, weight = 1) %>%
      # Add the toolbar for sweeping/lasso
      addDrawToolbar(
        targetGroup = "draw",
        polylineOptions = FALSE, circleOptions = FALSE, markerOptions = FALSE,
        circleMarkerOptions = FALSE, editOptions = editToolbarOptions()
      )
  })
  
  # 1. Handle Single Clicks
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    selected_data$ids <- c(selected_data$ids, click$id)
  })
  
  # 2. Handle "Sweep" (Polygon Selection)
  observeEvent(input$map_draw_new_feature, {
    feature <- input$map_draw_new_feature
    polygon_coords <- matrix(unlist(feature$geometry$coordinates), ncol = 2, byrow = TRUE)
    
    # Convert points and polygon to sf objects to check intersections
    poly_sf <- st_polygon(list(polygon_coords)) %>% st_sfc(crs = 4326)
    points_sf <- st_as_sf(raw_data(), coords = c("LON", "LAT"), crs = 4326)
    
    # Find stations inside the polygon
    inside_indices <- which(st_within(points_sf, poly_sf, sparse = FALSE))
    new_ids <- raw_data()$Station[inside_indices]
    
    # Add new unique IDs to the list
    selected_data$ids <- unique(c(selected_data$ids, new_ids))
    
    # Remove the drawn shape from map to keep it clean
    leafletProxy("map") %>% clearShapes() %>% clearGroup("draw")
  })
  
  # 3. --- OPTIMIZATION ENGINE (TSP) ---
  observeEvent(input$optimize, {
    req(length(selected_data$ids) > 2)
    
    # Get coordinates for currently selected stations
    df_to_opt <- raw_data() %>%
      filter(Station %in% selected_data$ids) %>%
      slice(match(selected_data$ids, Station))
    
    # Create distance matrix (Meters to NM)
    coords <- cbind(df_to_opt$LON, df_to_opt$LAT)
    d_mat <- distm(coords, fun = distGeo) / 1852
    
    # Solve the Traveling Salesperson Problem
    # We use "insert" or "two_opt" for a quick, efficient route
    tsp_prob <- TSP(d_mat)
    # We find a path starting at the FIRST station selected
    tour <- solve_TSP(tsp_prob, method = "nearest_neighbor", control = list(start = 1))
    
    # Reorder IDs based on the optimized tour
    optimized_order <- as.integer(tour)
    selected_data$ids <- df_to_opt$Station[optimized_order]
  })
  
  # Undo/Reset Logic
  observeEvent(input$undo, {
    if (length(selected_data$ids) > 0) selected_data$ids <- selected_data$ids[-length(selected_data$ids)]
  })
  observeEvent(input$reset, { selected_data$ids <- c() })
  
  # Map Visual Updates
  observe({
    req(raw_data())
    proxy <- leafletProxy("map")
    proxy %>% clearGroup("selection")
    
    if (length(selected_data$ids) > 0) {
      df_path <- raw_data() %>%
        filter(Station %in% selected_data$ids) %>%
        slice(match(selected_data$ids, Station))
      
      if (nrow(df_path) > 1) {
        proxy %>% addPolylines(lng = df_path$LON, lat = df_path$LAT, 
                               group = "selection", color = "red", weight = 3)
      }
      proxy %>% addCircleMarkers(lng = df_path$LON, lat = df_path$LAT, 
                                 group = "selection", color = "red", radius = 6)
    }
  })
  
  # Calculation Logic (Same as v1 with dhours fix)
  processed_log <- reactive({
    req(length(selected_data$ids) > 0)
    start_time <- ymd_hm(input$dep_time, quiet = TRUE)
    if(is.na(start_time)) start_time <- Sys.time() 
    
    df_calc <- raw_data() %>%
      filter(Station %in% selected_data$ids) %>%
      slice(match(selected_data$ids, Station)) %>%
      mutate(
        Stn_Order = row_number(),
        Dist_NM = ifelse(row_number() == 1, 0, distGeo(cbind(LON, LAT), cbind(lag(LON), lag(LAT))) / 1852),
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
  
  # Render Summary and Table
  output$summary_stats <- renderUI({
    req(processed_log())
    df <- processed_log()
    wellPanel(fluidRow(
      column(6, h4(strong("Total Dist:")), h3(round(sum(df$Dist_NM), 1), "NM")),
      column(6, h4(strong("Final ETA:")), h3(tail(df$Est_Departure, 1)))
    ))
  })
  
  output$log_table <- renderDT({
    validate(need(length(selected_data$ids) > 0, "Select or Sweep stations on the map."))
    datatable(processed_log() %>% 
                select(Stn_Order, Station, Grid.ID, Travel_Min, Est_Arrival, Ops_Min, Est_Departure),
              colnames = c("Order", "Sta #", "Grid ID", "Travel (min)", "Arrive", "Ops (min)", "Depart"),
              options = list(pageLength = 50, scrollX = TRUE), rownames = FALSE)
  })
  
  output$download_log <- downloadHandler(
    filename = function() { paste0("Optimized_Survey_", Sys.Date(), ".csv") },
    content = function(file) { write.csv(processed_log(), file, row.names = FALSE) }
  )
}

shinyApp(ui, server)