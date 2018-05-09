#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(magrittr)
library(ggplot2)
library(readr)
library(dplyr)

data <- readr::read_rds("eleitorado.rds") %>% 
  filter(ano == "2016")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Perfil do Eleitorado Brasileiro"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    selectInput("uf", label = "Selecione uma UF", 
                choices = list("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA", "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO", "ZZ"), 
                selected = "AC"),
    uiOutput("city_ui")
  ),
  mainPanel(
    plotOutput("bar")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  cities <- reactive({
    mun <- read_rds("mun_tse.rds") 
    
    names_city <- mun[mun$UF == input$uf,][["NOME_MUNICIPIO"]]
    cod_tse    <- mun[mun$UF == input$uf,][["COD_MUN_TSE"]]
    
    cities <- as.list(cod_tse)
    names(cities) <- names_city
    return(cities)
  })

  output$city_ui <- renderUI({
    selectInput("city",
                label = "Selecione uma Cidade",
                choices = cities())
  })

  output$bar <- renderPlot({
    data %>% 
      filter(tsecod == input$city) %>% 
      ggplot(mapping = aes(estado_civil)) +
      geom_bar() +
      theme_minimal()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

