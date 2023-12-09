library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(readr)
library(shinyjs)
library(shinyWidgets)  

ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = ("Análisis de Spotify")),
                    
                    dashboardSidebar(                   
                      sidebarMenu(br(), br(),
                                  selectInput("anio", "Seleccione el año:", choices = NULL), br(),
                                  selectInput("genero", "Seleccione el género:", choices = NULL),
                                  
                                  fluidRow(
                                    column(width = 12, offset = 2, br(), br(), # para centrar el botón de descarga
                                           downloadButton("descarga", "Descargar Datos"),
                                           useShinyjs()
                                    )
                                  )
                      )
                    ),
                    
                    dashboardBody(
                      
                      column(5, plotlyOutput("grafico")),
                      column(7, dataTableOutput("tabla"))
                    )
)

server <- function(input, output, session) {
  
  spotify_data <- reactive({ read.csv2("datos/spotify_2000_2023.csv") })
  
  observe({
    updateSelectInput(session, "anio", choices = unique(spotify_data()$year))
    updateSelectInput(session, "genero", choices = unique(spotify_data()$top.genre))
  })
  
  seleccion_spotify <- reactive({
    spotify_data() |>
      filter(top.genre == input$genero, year == input$anio)
  })
  
  output$grafico <- renderPlotly({
    ggplot(seleccion_spotify(), aes(x = energy, y = valence))+
      geom_point() +
      labs(title = paste("Valence vs Energy"),
           x = "Energy",
           y = "Valence")
    
  })
  output$tabla <- renderDataTable({
    seleccion_spotify()
  })
  
  observe({
    if (nrow(seleccion_spotify()) == 0) {
      shinyjs::disable("descarga")
    } else {
      shinyjs::enable("descarga")
    }
  })
  
  output$descarga <- downloadHandler(
    filename = function() {
      paste("data_spotify_", input$genero,"_",  input$anio, ".csv", sep = "")
    },
    content = function(file) {
      write_csv(seleccion_spotify(), file)
    } )
}

shinyApp(ui = ui, server = server)