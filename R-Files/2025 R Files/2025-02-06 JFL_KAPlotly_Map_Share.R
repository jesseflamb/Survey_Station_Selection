#--------------------------------#
# Survey Station Planning code ####
#--------------------------------#
# Attempt 2 ####

#library(shiny)
#library(plotly)
#library(Cairo)
#library(clipr)
#library(here)

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("tidyr",  "dplyr", "ggplot2", "ggmap", "lattice", "here",
              "rnaturalearth","marmap","readxl", "data.table", "geosphere",
             "lubridate","hms", "stringr","tibble","readxl", "sf","maps", 
             "mapdata","mapplots", "mapview","Cairo", "ncdf4","oce", "here",
             "reshape2", "viridis", "export", "rnaturalearth",
              "rnaturalearthdata", "forcats", "geosphere", "shiny",
             "plotly","clipr")

ipak(packages) 


ui <- fluidPage(
  plotlyOutput("plot"),
  verbatimTextOutput("click"),
  verbatimTextOutput("brush"),
  actionButton("copy", "Copy")
)

# Bering Sea
EBSdata <- read.csv(here("Data","UpdatedEBSdata_DY2407_StationList.csv"))
str(EBSdata)

# WGOA 
#Station Location Data
#WGOAdata <- read.csv(here("Data","WGOA station_list_dougherty_2019_ProjInstrutions.csv")) %>% 
#  rename("LAT" = "Latitude.N.") %>% rename("LON"="Longitude..W.") %>% mutate(Grid.ID = tolower(Grid.ID))
#WGOAdata$LON <- WGOAdata$LON*-1
#GridStaData <- read.csv(here("Data","2023-10-18 WGOA Grid Sta Depth Data.csv"))
#GridMeanZ <- GridStaData %>% dplyr::rename("Grid.ID" = "FOCI_GRID") %>% 
#  mutate(Grid.ID = tolower(Grid.ID)) %>% 
#  group_by(Grid.ID) %>% summarise(MeanZ = mean(BOTTOM_DEPTH))

#WGOAdata_z <- dplyr::left_join(WGOAdata,GridMeanZ, by = "Grid.ID")

#w1 <- ggplot2::map_data("world", xlim = c(-180,180), ylim = c(20, 80))


# get bathymetry data
b = marmap::getNOAA.bathy(lon1 = -180, lon2 = -140, lat1 = 50, lat2 = 70, resolution = 10)
# convert bathymetry to data frame
bf = fortify.bathy(b)

server <- function(input, output, session) {
  
  output$plot <- renderPlotly({
    p <- ggplot()+
      coord_map("albers", parameters=c(37, 59),xlim=c(-174,-158),ylim=c(53.5,60))+
      # add 50m contour
      geom_contour(data = bf,
                   aes(x=x, y=y, z=z),
                   breaks=c(-50),
                   linewidth=c(0.5),
                   linetype='dashed',
                   colour="grey90")+
      geom_text(data=EBSdata %>% filter(Grid.ID=="AX61"),aes(x=LON, y=LAT, label="50m"),colour="grey90", size=4,
                fontface="bold",nudge_x=.1, nudge_y=.3)+
      # add 100m contour
      geom_contour(data = bf,
                   aes(x=x, y=y, z=z),
                   breaks=c(-100),
                   linewidth=c(0.5),
                   linetype='dashed',
                   colour="grey80")+
      geom_text(data=EBSdata %>% filter(Grid.ID=="BV61"),aes(x=LON, y=LAT, label="100m"),colour="grey80", size=4,
                fontface="bold",nudge_x=0, nudge_y=0.3)+
      # add 200m contour
      geom_contour(data = bf,
                   aes(x=x, y=y, z=z),
                   breaks=c(-200),
                   linewidth=c(0.5),
                   linetype='dashed',
                   colour="grey70")+
      geom_text(data=EBSdata %>% filter(Grid.ID=="CB61"),aes(x=LON, y=LAT, label="200m"),colour="grey70", size=4,
                fontface="bold",nudge_x=-0.75, nudge_y=0)+
      geom_polygon(data = w1, aes(x=long, y = lat, group = group),colour="black", fill="darkgrey")+
      geom_path(data=EBSdata %>% filter(TYPE=="Core") %>% arrange(Grid.ID),aes(x=LON, y=LAT, color=Grid.ID), colour="black")+
      geom_point(data = EBSdata %>% filter(TYPE=="Core"), aes(x=LON, y=LAT, key=Grid.ID, fill=Sta_Pri), color="black",size=4, shape=21)+
      # scale_fill_manual(values=c("blue","red"))+
      geom_text(data=EBSdata %>% filter(TYPE=="Core"),aes(x=LON, y=LAT, label=Grid.ID), size=4, colour="black",
                fontface="bold",nudge_x=-0.09, nudge_y=-0.09)+
      scale_x_continuous(name="Longitude",expand = c(0,0))+
      scale_y_continuous(name="Latitude",expand = c(0,0))+
      theme_bw()+
      theme(
        plot.title=element_text(size=20, color="black"),
        axis.text.x=element_text(size=20, color = "black"), 
        axis.text.y = element_text(size=20, color = "black"),
        axis.title.y = element_text(size=20, color = "black"),
        axis.title.x = element_text(size=20, color = "black"),
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_line(color="gray85"),
        panel.grid.minor = element_line(color="gray85"),
        legend.background = element_rect(fill='transparent'),
        # legend.box.background = element_rect(fill='transparent'),
        legend.position = c(0.1,0.2),
        # legend.position = "right",
        legend.title = element_text(size=20, color="black"),
        legend.text = element_text(size=18, color="black"))+
      ggtitle("DY24-07 EcoFOCI Spring Ichthyoplankton Survey")
    ggplotly(p) %>% 
      layout(dragmode = "lasso")
  })
  
  output$click <- renderPrint({
    d <- event_data("plotly_click")
    if (!is.null(d)) d
  })
  
  output$brush <- renderPrint({
    d <- event_data("plotly_selected")
    if (!is.null(d)) d
  })
  
  observeEvent(input$copy, {
    write_clip(brushedPoints(EBSdata, input$plotly_selected), object_type = "table")
  })
}

shinyApp(ui, server)

results<-read.table(text = read_clip(), header = TRUE, sep = "\t", fill=TRUE)
results






#-----------------------------------------------------------------------------------#
# Attempt 2 ####
#--------------------------------#
# This retrieves all "brushed points" with metadata but I can't figure out how to select point after point

library(shiny)
library(ggplot2)
library(Cairo)   # For nicer ggplot2 output when deployed on Linux
library(clipr)


ui <- fluidPage(
  fluidRow(
    column(width = 12,
           plotOutput("plot1", height = 800,
                      click = "plot1_click",
                      brush = brushOpts(
                        id = "plot1_brush"
                      )
           )
    )
  ),
  fluidRow(
    column(width = 12,
           h4("Points near click"),
           verbatimTextOutput("click_info")
    ),
    column(width = 12,
           h4("Brushed points"),
           verbatimTextOutput("brush_info"),
           br(),
           actionButton("copy", "Copy")
    )
  )
)

server <- function(input, output) {
  output$plot1 <- renderPlot({
    ggplot()+
      coord_map("albers", parameters=c(37, 59),xlim=c(-174,-158),ylim=c(53.5,60))+
      # add 50m contour
      geom_contour(data = bf%>% rename(LON=x, LAT=y), 
                   aes(x=LON, y=LAT, z=z),
                   breaks=c(-50),
                   linewidth=c(0.5),
                   linetype='dashed',
                   colour="grey90")+
      geom_text(data=EBSdata %>% filter(Grid.ID=="AX61"),aes(x=LON, y=LAT, label="50m"),colour="grey90", size=4, 
                fontface="bold",nudge_x=.1, nudge_y=.3)+
      # add 100m contour
      geom_contour(data = bf%>% rename(LON=x, LAT=y), 
                   aes(x=LON, y=LAT, z=z),
                   breaks=c(-100),
                   linewidth=c(0.5),
                   linetype='dashed',
                   colour="grey80")+
      geom_text(data=EBSdata %>% filter(Grid.ID=="BV61"),aes(x=LON, y=LAT, label="100m"),colour="grey80", size=4, 
                fontface="bold",nudge_x=0, nudge_y=0.3)+
      # add 200m contour
      geom_contour(data = bf%>% rename(LON=x, LAT=y), 
                   aes(x=LON, y=LAT, z=z),
                   breaks=c(-200),
                   linewidth=c(0.5),
                   linetype='dashed',
                   colour="grey70")+
      geom_text(data=EBSdata %>% filter(Grid.ID=="CB61"),aes(x=LON, y=LAT, label="200m"),colour="grey70", size=4, 
                fontface="bold",nudge_x=-0.75, nudge_y=0)+
      geom_polygon(data = w1 %>% rename(LON=long, LAT=lat), aes(x=LON, y = LAT, group = group),colour="black", fill="darkgrey")+
      geom_path(data=EBSdata %>% filter(TYPE=="Core") %>% arrange(Station),aes(x=LON, y=LAT, color=Station), colour="black")+
      geom_point(data = EBSdata %>% filter(TYPE=="Core"), aes(x=LON, y=LAT, key=Grid.ID, fill=Sta_Pri), color="black",size=4, shape=21)+
      # scale_fill_manual(values=c("blue","red"))+
      geom_text(data=EBSdata %>% filter(TYPE=="Core"),aes(x=LON, y=LAT, label=Grid.ID), size=4, colour="black",
                fontface="bold",nudge_x=-0.09, nudge_y=-0.09)+
      scale_x_continuous(name="Longitude",expand = c(0,0))+
      scale_y_continuous(name="Latitude",expand = c(0,0))+
      theme_bw()
    
  })
  
  
  output$click_info <- renderPrint({
    # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
    # were a base graphics plot, we'd need those.
    nearPoints(EBSdata, input$plot1_click, xvar="LON", yvar="LAT", addDist = TRUE)
  })
  
  
  output$brush_info <- renderPrint({
    brushedPoints(EBSdata, input$plot1_brush, xvar="LON", yvar="LAT")
  })
  
  observeEvent(input$copy, {
    write_clip(brushedPoints(EBSdata, input$plot1_brush), object_type = "table")
  })
}

shinyApp(ui, server)


results<-read.table(text = read_clip(), header = TRUE, sep = "\t", fill=T)
results

