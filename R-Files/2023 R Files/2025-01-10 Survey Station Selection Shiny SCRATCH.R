## OBJECTIVE: Make shiny app to select station order at sea

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("tidyr",  "dplyr","lubridate","hms", "ggplot2", "ggmap", "lattice", 
              "stringr", "data.table","tibble","readxl", "sf","maps", "mapdata",
              "mapplots", "mapview", "marmap", "Cairo", "ncdf4","oce", "here","reshape2",
              "viridis", "export", "rnaturalearth", "kableExtra",
              "rnaturalearthdata", "forcats","sf", "geosphere", "leaflet","shiny")
ipak(packages)



# Load the required packages
library(shiny)
library(leaflet)
library(geosphere)

# Define the coordinates of the six stations in the Bering Sea
stations <- data.frame(
  name = paste("Station", 1:6),
  lat = c(57.0, 56.0, 55.5, 55.2, 56.5, 57.8),
  lng = c(-170.0, -171.0, -172.0, -172.5, -170.5, -169.8)
)

# Define the UI
ui <- fluidPage(
  titlePanel("Survey Stations in the Bering Sea"),
  sidebarLayout(
    sidebarPanel(
      h4("Click on the stations in order on the map."),
      actionButton("calculate", "Calculate"),
      downloadButton("downloadData", "Download CSV")
    ),
    mainPanel(
      leafletOutput("map"),
      tableOutput("timeTable")
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  # Render the map
  output$map <- renderLeaflet({
    leaflet(stations) %>%
      addTiles() %>%
      addMarkers(~lng, ~lat, label = ~name, layerId = ~name)
  })
  
  # Reactive value to store clicked stations
  clicked_stations <- reactiveValues(data = data.frame(name = character(), lat = numeric(), lng = numeric()))
  
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    if (nrow(clicked_stations$data) < 6) {  # Ensure only up to 6 stations are added
      clicked_stations$data <- rbind(clicked_stations$data, stations[stations$name == click$id,])
    }
    print(clicked_stations$data)  # Debugging line to print clicked stations
  })
  
  # Calculate time to travel between stations when the "Calculate" button is pressed
  travel_times <- reactiveVal(data.frame())
  
  observeEvent(input$calculate, {
    print("Calculate button pressed")  # Debugging line
    if (nrow(clicked_stations$data) < 2) {
      print("Not enough stations selected")  # Debugging line
      return()
    }
    
    travel_times_df <- data.frame(
      From = character(),
      To = character(),
      Distance = numeric(),
      Time = numeric(),
      stringsAsFactors = FALSE
    )
    
    for (i in 1:(nrow(clicked_stations$data) - 1)) {
      from <- clicked_stations$data[i, c("lng", "lat")]
      to <- clicked_stations$data[i + 1, c("lng", "lat")]
      distance <- distHaversine(from, to) / 1852  # Convert meters to nautical miles
      travel_times_df <- rbind(travel_times_df, data.frame(
        From = clicked_stations$data$name[i],
        To = clicked_stations$data$name[i + 1],
        Distance = distance,
        Time = distance / 10  # Assuming speed is 10 knots
      ))
    }
    
    # Set names for the columns
    travel_times_df <- setNames(travel_times_df, c("From", "To", "Distance (Nautical Miles)", "Time (Hours)"))
    
    travel_times(travel_times_df)
    
    output$timeTable <- renderTable({
      travel_times()
    })
    
    print(travel_times())  # Debugging line to print the travel times table
  })
  
  # Provide download handler for the CSV file
  output$downloadData <- downloadHandler(
    filename = function() { "travel_times.csv" },
    content = function(file) {
      write.csv(travel_times(), file, row.names = FALSE)
    }
  )
}

# Run the Shiny app
shinyApp(ui, server)



