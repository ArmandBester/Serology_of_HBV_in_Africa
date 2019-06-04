library(shiny)
library(leaflet)
library(dplyr)
library(readr)
library(htmltools)
library(htmlwidgets)


newDF <- read_delim('oxHBVappData.csv', delim = ",", col_names = TRUE)
newDF$Size_of_cohort <- as.numeric(newDF$Size_of_cohort)
newDF$long <- as.numeric(newDF$long)

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Visualisation of HBV sero-epidemiology data for Africa"),
  
  tags$br(),
  
  htmlOutput("Meta"),
  
  tags$br(),
  
  navlistPanel(
    
    tabPanel("Estimated susceptible population",
      leafletOutput("HBVmap",  width = "80%", height = 700)
    ),
    
    tabPanel("Anti-HBc prevalence",
             leafletOutput("HBVmap2",  width = "80%", height = 700)
    ),
    
    tabPanel("HBsAg prevalence",
             leafletOutput("HBVmap3",  width = "80%", height = 700)
    ),
    
    tabPanel("Anti-HBc HBsAg ratio",
             leafletOutput("HBVmap4",  width = "80%", height = 700)
    ), widths = c(2, 10)
  ),  style='width: 85%'
)


## <---------------------------------0------------------------>

server <- function(input, output, session) {
  
  output$Meta <- renderText("These data were collected through a systematic literature review of HBV serology data for adult populations<p> in Africa,  
                            from January 1995 to June 2018; metadata are available on-line at the following link: <p>
                            <a href='https://figsharecom/s/4414fce1d474bc8a6198'>https://figsharecom/s/4414fce1d474bc8a6198 </a> <p>
                            and source code is available here: <p>
                            <a href='https://github.com/ArmandBester/Serology_of_HBV_in_Africa'>https://github.com/ArmandBester/Serology_of_HBV_in_Africa </a> <p>
                            Click on the menu bar on the left of the page to choose which data you would like to visualise. Hover over each cohort to <p>
                            see details of the location, population sampled, cohort size and HBV serology results. The circle in each case is proportional <p>
                            to the size of the cohort.  You can zoom in and out using the mouse wheel.")

  # create a color palette
  pal <- colorNumeric(
    palette = "viridis",
    domain = newDF$Estimated_susceptible_population, 
    reverse = TRUE
  )
  
  pal2 <- colorNumeric(
    palette = "viridis",
    domain = newDF$'Anti-Hbc_prevalence', 
    reverse = TRUE
  )
  
  pal3 <- colorNumeric(
    palette = "viridis",
    domain = newDF$HBsAg_prevalence, 
    reverse = TRUE
  )
  
  pal4 <- colorNumeric(
    palette = "viridis",
    domain = newDF$'Anti-HBc_HBsAg_ratio', 
    reverse = TRUE
  )
  
  # create labels
  labs <- lapply(seq(nrow(newDF)), function(i) {
    paste0( 'Country: ', newDF[i, "Country"], '<p></p>',
            "City: ", newDF[i, "City"], '</p><p>',
            "Cohort char: ",  newDF[i, "CohortCharacteristics"],'</p><p>',
            "Cohort Size: ", newDF[i, "Size_of_cohort"],'</p><p>',
            "Anti-HBc: ", newDF[i, "Anti-Hbc_prevalence"],'</p><p>',
            "HBsAg: ", newDF[i, "HBsAg_prevalence"],'</p><p>',
            "Anti-HBc HBsAg ratio: ", newDF[i, "Anti-HBc_HBsAg_ratio"], "</p><p>",
            "Estimated susceptible population: ", newDF[i, "Estimated_susceptible_population"], "</p><p>",
            newDF[i, "long"], ", ",
            newDF[i, "lat"],'</p><p>')
  })
  
  
    
  # Map1
  output$HBVmap <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(newDF) %>% addTiles() %>%
      fitBounds(min(newDF$long), min(newDF$lat), max(newDF$long), max(newDF$lat)) %>% 
      addCircleMarkers(lng = ~long, lat = ~lat, 
                       popup = lapply(labs, HTML),
                       label = lapply(labs, HTML),
                       stroke = TRUE, fillOpacity = 0.4,
                       color = ~pal(Estimated_susceptible_population),
                       radius = ~log2(Size_of_cohort)) %>% 
      
      addLegend("bottomright", pal = pal, values = ~Estimated_susceptible_population,
                title = "Estimated susceptible <p> population",
                labFormat = labelFormat(),
                opacity = 1)
    })
  
  
  # Map2 
  output$HBVmap2 <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(newDF) %>% addTiles() %>%
      fitBounds(min(newDF$long), min(newDF$lat), max(newDF$long), max(newDF$lat)) %>% 
      addCircleMarkers(lng = ~long, lat = ~lat, 
                       popup = lapply(labs, HTML),
                       label = lapply(labs, HTML),
                       stroke = TRUE, fillOpacity = 0.4,
                       color = pal2(newDF$'Anti-Hbc_prevalence'),
                       radius = ~log2(Size_of_cohort)) %>% 
      
      addLegend("bottomright", pal = pal2, values = newDF$'Anti-Hbc_prevalence',
                title = "Anti-HBc prevalence",
                labFormat = labelFormat(),
                opacity = 1)
  })
  
  
  # Map3 
  output$HBVmap3 <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(newDF) %>% addTiles() %>%
      fitBounds(min(newDF$long), min(newDF$lat), max(newDF$long), max(newDF$lat)) %>% 
      addCircleMarkers(lng = ~long, lat = ~lat, 
                       popup = lapply(labs, HTML),
                       label = lapply(labs, HTML),
                       stroke = TRUE, fillOpacity = 0.4,
                       color = ~pal3(HBsAg_prevalence),
                       radius = ~log2(Size_of_cohort)) %>% 
      
      addLegend("bottomright", pal = pal3, values = newDF$HBsAg_prevalence,
                title = "HBsAg prevalence",
                labFormat = labelFormat(),
                opacity = 1)
  })
  
  # Map4
  output$HBVmap4 <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(newDF) %>% addTiles() %>%
      fitBounds(min(newDF$long), min(newDF$lat), max(newDF$long), max(newDF$lat)) %>% 
      addCircleMarkers(lng = ~long, lat = ~lat, 
                       popup = lapply(labs, HTML),
                       label = lapply(labs, HTML),
                       stroke = TRUE, fillOpacity = 0.4,
                       color = pal4(newDF$'Anti-HBc_HBsAg_ratio'),
                       radius = ~log2(Size_of_cohort)) %>% 
      
      addLegend("bottomright", pal = pal4, values = newDF$'Anti-HBc_HBsAg_ratio',
                title = "Anti-HBc/HBsAg",
                labFormat = labelFormat(),
                opacity = 1)
  })
  
}
   


# Run the application 
shinyApp(ui = ui, server = server)

