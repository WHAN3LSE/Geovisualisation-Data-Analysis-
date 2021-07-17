library(shiny)
library(leaflet)
library(geojsonio)
library(RColorBrewer)
library(sp)

eu <- geojson_read ("map.geojson", what = "sp")

ui <- bootstrapPage(
  # Styling applied to the UI
  tags$style(type = "text/css", "html, body {width: 100%; height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  # Placement of the navigation panel
  absolutePanel(top = 10, right = 10,
  sliderInput("range", "Year of Joining European Union", min(eu$year), max(eu$year),
              value = range(eu$year), step = 1),
  selectInput("colors", "Color Scheme",
              rownames(subset(brewer.pal.info, category %in% c("seq", "div")))),
  checkboxInput("legend", "Show Legend", TRUE)
  )
)

server <- function(input, output, session) {
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
  eu [eu$year >= input$range[1] & eu$year <= input$range[2],]
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(input$colors, eu$year)
  })
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    
      leaflet(eu)%>% 
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      setView(10, 10, zoom = 2.0)
  })
  
  # Incremental changes to the map (in this case, replacing the
  # polygon when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    pal <- colorpal()
    leafletProxy("map", data = filteredData())%>%
      clearShapes()%>%
      addPolygons(stroke = FALSE, smoothFactor = 0.3, fillColor = ~pal(year), fillOpacity = 0.7, popup = ~paste(name_long, ": ", year))
    
  })
  
  # Use a separate observer to recreate the legend as needed once changes are made
  observe({
    proxy <- leafletProxy("map", data = eu)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy%>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright", pal = pal, values = eu$year)
    }
  })
}

shinyApp(ui, server)