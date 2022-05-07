library(shiny)
library(shinythemes)
library(data.table)
library(plotly)
library(tidyverse) #ggplot2 , dplyr


# Base de Dados
dados <- fread("vacina_aracaju.csv", encoding = 'UTF-8')


# UI
shinyUI(fluidPage(
  # Tema
  theme = shinytheme("flatly"),
  
  # Título                
  titlePanel(p("Vacinação de COVID-19 em Sergipe", style="text-align:center" )),
  br(),
  br(),
  
  # Layout Sidebar
  sidebarLayout(
    
    # Painel lateral
    sidebarPanel(
      h3(p("Filtros", style="text-align:center")),
      br(),
      
      # Filtro 1
      selectizeInput(
        inputId = "dose",
        label =  "Tipo da dose",
        choices = c('Todas',sort(unique(dados$vacina_descricao_dose)))
      ),
      
      # Filtro 2
      selectizeInput(
        inputId = "gender",
        label =  "Gênero",
        choices = c('Ambos',sort(unique(dados$paciente_enumsexobiologico)))
      ),
      
      # Filtro 3
      dateRangeInput(
        inputId = "date",
        label = "Data",
        start = min(as.Date(dados$vacina_dataaplicacao)),
        end = max(as.Date(dados$vacina_dataaplicacao))
      ),
      br(),
      br()
    ),
    
    # Painel Principal         
    mainPanel(
      
      fluidRow(
        
        # Painel de tabs
        tabsetPanel(
          
          # Tab 1
          tabPanel(
            # Nome da Tab
            p("Vacinação Diária"),
            # Plot
            column(
              width = 12, # Largura
              plotlyOutput('vacdiaria', height = 380) # Altura do gráfico
            )
          ),
          
          # Tab 2
          tabPanel(
            # Nome da Tab
            p("Outros Gráficos"),
            # Subttitulo
            h4(p("Quantidade de doses aplicadas", 
                 style="color:black;text-align:center"
                )
              ),
            hr(), #Linha
            # Plot 1
            column(
              width = 6,
              h4(p("Por tipo de vacina", style="text-align:center;font-size:15px")),
              plotlyOutput('tipovac', height = 340)
            ),
            # Plot 2
            column(
              width = 6,
              h4(p("Por raça", style="text-align:center;font-size:15px")),
              plotlyOutput('raca', height = 340)
            )
          )
        )
      )
    )
    )
  )
)



