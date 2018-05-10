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
library(knitr)

data <- readr::read_rds("eleitorado.rds") %>% 
  filter(ano == "2016")

# Define UI for application that draws a histogram
ui <- navbarPage("Eleitorado Brasileiro",
  tabPanel("Dados",
    # Sidebar with a slider input for number of bins 
    sidebarPanel(
      img(src = "logoCepespData.png", height = "100%", width = "100%"),
      titlePanel("Perfil do Eleitorado Brasileiro"),
      selectInput("uf", label = "Selecione uma UF", 
                  choices = list("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA", "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO", "ZZ"), 
                  selected = "AC"),
      uiOutput("city_ui"),
      selectInput("var",
                  label = "Selecione uma Variável",
                  choices = list("Estado Civil" = 1, "Faixa Etária" = 2, "Escolaridade" = 3, "Sexo" = 4)),
      tableOutput("sum_est")
    ),
    mainPanel(
      plotOutput("bar")
    )
  ),
  tabPanel("Sobre",
           mainPanel(
             h2("Créditos"),
             p("Aplicativo criado pela equipeo do CEPESPData da FGV."),
             tags$li("Guilherme"),
             tags$li("Mauricio Izumi"),
             tags$li("Rafael Silva")
           )
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
  
  output$sum_est <- renderTable({
    data %>% 
      filter(tsecod == 1120) %>% 
      summarise(`Eleitorado` = n(),
                `Proporção de Mulheres` = sum(sexo == 2)/sum(sexo %in% c(2, 4)))
  })

  output$bar <- renderPlot({
    data %>% 
      filter(tsecod == input$city) %>%
      ggplot(mapping = aes(as.factor(sexo), fill = as.factor(sexo))) +
      geom_bar() +
      theme_minimal() +
      scale_fill_discrete(label = c("Masculino", "Feminino")) +
      theme(title = element_text(face = "bold", size = 16),
            axis.title = element_text(face = "bold", size = 14),
            axis.text  = element_text(size = 14),
            legend.position = "bottom") +
      labs(x = "Sexo", 
           y = "Quantidade",
           title = "Proporção de Sexo")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

