

shinyUI({
  navbarPage('Malawi LIMS data',
    tabPanel('Leaflet Map',
            hr(),
    dropdownButton(
                     label = "Select Lab", status = "default", width = 200, circle = F,
                     checkboxGroupInput(inputId = "lab", label = "Choose", sort(unique(df2$lab)), selected=df2$lab[1])),
    hr(),
    sliderInput("per_adjust", label = "Percentage adjustment:", min = 0, max = 40, value = 0, step = 0.25),
    hr(),
    hr(),
    box(
      title = "Lab catchment areas",
      # collapsible = TRUE,
      # width = "100%",
      # height = "1000px",
      # leafletOutput('mymap',width="100%",height="1000px"))
      tags$style(type = "text/css", ".box-body {height:80vh}"),
      leafletOutput('mymap', width = "200%", height = "100%")
    )),
    
    tabPanel('Network Analysis',
             visNetworkOutput("network")
             ),
  
    tabPanel('Hulls', 
             sliderInput("per_adjust2", label = "Percentage adjustment:", min = 0, max = 40, value = 0, step = 0.25),
             hr(),
             plotOutput('hulls'))
  )
})
  
  