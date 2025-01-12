
library(shiny)
library(colourpicker)

source("mapdrawing.R")


ui <- fluidPage(

    titlePanel("Draw map by writing the location name"),

    sidebarLayout(
        sidebarPanel(
          textInput(inputId="place", label = "1. Choose location", value = "Finland, Jämsä", width = "400px",
                    placeholder = "Name of location"),

          selectInput(
            'street', '2. Choose street tier', choices = c(1,2,3),
            selectize = FALSE
          ),
          colourInput(inputId = "col", "Select colour of roads", value = "red"),
          colourInput(inputId = "background", "Select background colour", value = "white"),
          colourInput(inputId = "water", "Select water colour", value = "lightgray"),
          sliderInput("sizeplot",
                      "Size of picture:",
                      min = 0,
                      max = 2,
                      value = 0.01,
                      step = 0.01
          ),
          sliderInput("vshift",
                      "Shift vertically:",
                      min = -1,
                      max = 1,
                      value = 0,
                      step = 0.01
          ),
          sliderInput("hshift",
                      "Shift horizontally:",
                      min = -1,
                      max = 1,
                      value = 0,
                      step = 0.01
          )
        ),
        mainPanel(
           plotOutput("map"),
           textOutput('ex_out'),
           textOutput("sliderValue")
           
        )
    )
)

server <- function(input, output) {
  debouncedSizeplot <- debounce(reactive(input$sizeplot), 500)
  
  output$value <- renderText({ 
    input$place
  })
  output$sliderValue <- renderText({
    paste("Valittu koko:", debouncedSizeplot())
  })
  output$map <- renderPlot({
    drawmap(place_name = input$place, v.shift = input$vshift, h.shift = input$hshift, mapsize = debouncedSizeplot(), water.col = input$water, bg.col = input$background, street.col = c(input$col,input$col,input$col),street.tier = input$street)
  })
}

shinyApp(ui = ui, server = server)

