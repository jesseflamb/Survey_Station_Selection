##Recent Surveys Stations Completed
#Sta_Count_13_Present <- read.csv(here("Data","2023_10_12-All_Zooplankton.csv")) %>% dplyr::select(YEAR,HAUL_ID) %>% 
#  dplyr::mutate(YEAR = as.factor(YEAR)) %>% 
#  tidyr::separate_wider_delim(HAUL_ID," ",names = c("CRUISE","STATION","HAUL","GEAR","NET"), names_repair = "unique") %>% 
#  dplyr::filter(CRUISE %in% c("DY13-06","DY15-05","DY17-05","DY19-05")) %>% dplyr::select(CRUISE, STATION)  %>% unique() %>%  
#  group_by(CRUISE) %>% summarise(count = n())

#Sta_DATE_13_Present <- read.csv(here("Data","2023_10_12-All_Zooplankton.csv")) %>% dplyr::select(CRUISE,GMT_DATE_TIME_TXT) %>% 
#  dplyr::filter(CRUISE %in% c("DY13-06","DY15-05","DY17-05","DY19-05")) %>% mutate(DATE = as.Date(GMT_DATE_TIME_TXT)) %>% 
# group_by(CRUISE) %>% summarise(START_DATE = min(DATE), END_DATE = max(DATE), 
#                                 Days_Count = as.integer(difftime(max(DATE), min(DATE), units = "days")) + 1)
#Sta_Sampled <- left_join(Sta_Count_13_Present, Sta_DATE_13_Present, by = "CRUISE") %>% dplyr::rename("Station Count" = "count") %>% 
#  mutate(Sta_per_Day = (`Station Count`/Days_Count))
#write.csv(Sta_Sampled,file = here("Data","2025-02-06 Spring Larval Stations Sampled 13-19.csv"))


# Sample data frame with station locations (longitude, latitude)
library(shiny)
library(plotly)
library(Cairo)
library(sf)
library(dplyr)
library(geosphere)

# Sample data frame with station locations (longitude, latitude)
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
