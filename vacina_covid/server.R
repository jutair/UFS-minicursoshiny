library(shiny)
library(data.table)
library(tidyverse)
library(plotly)


# Base de Dados
dados <- fread("vacina_aracaju.csv", encoding = 'UTF-8')


# Server
shinyServer(function(input, output) {
  
  # Filtros (reatividade)
  df <- reactive({
    
    # Filtro por tipo da dose
    if (!'Todas' %in% input$dose){
        dados <- dados %>% filter(
          vacina_descricao_dose %in% input$dose
        )
    }
    
    # Filtro por genero
    if(!'Ambos' %in% input$gender){
        dados <- dados %>% filter(
          paciente_enumsexobiologico %in% input$gender
        )
    }
    
    # Filtro de Dara
    dados <- dados %>% filter(
      as.Date(vacina_dataaplicacao) >= input$date[1] 
      & as.Date(vacina_dataaplicacao) <= input$date[2]
    )

    dados
  })

  
  # Plot vacinação diária
  output$vacdiaria <- renderPlotly({
    
    # Transformação
    dados <- df()
    dados_data<- dados %>%
      group_by(
        vacina_dataaplicacao
      ) %>%
      summarise(
        Vacinas =n()
      )
    
    # Gráfico
    ggplotly(
      ggplot(data = dados_data) +
        geom_col(
          aes(x = as.Date(vacina_dataaplicacao),
              y = Vacinas,
              text = paste("Doses Aplicadas:",
                           Vacinas,
                           "\nData:",
                           format(as.Date(vacina_dataaplicacao),'%d-%m-%Y'))
              ), 
              width = 0.5,
              fill = "#18BC9C"
        ) +
        scale_x_date(
          breaks = '1 month', date_labels = "%b"
        ) +
        scale_y_continuous(expand = expansion(mult = c(0,0.05))) +
        theme_classic() +
        theme(
          panel.background = element_rect(fill = "transparent", colour = NA),
          axis.title = element_blank(),
          plot.title = element_text(),
          axis.text = element_text(color = "#555555"),
          axis.line.y = element_blank(),
          axis.line.x = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.ticks = element_blank()
        ),
      tooltip = "text")
  })

  # Plot Tipo de Vacina
  output$tipovac <- renderPlotly({
    
    # Transformação
    dados <- df()
    dados_tipo <- dados %>%
      group_by(
        vacina_nome
      ) %>%
      summarise(
        Vacinas = n()
      )%>%
      mutate(
        vacina_nome = fct_reorder(vacina_nome,Vacinas, .desc = TRUE)
      )
    
    # Gráfico
    ggplotly(
      ggplot(dados_tipo) +
        geom_col(
          aes(x = (vacina_nome),
              y = Vacinas,
              text = paste("Doses Aplicadas:",Vacinas)
          ), 
          width = 0.7, 
          stat = "identity",
          fill = '#25D498'
        ) +
        scale_y_continuous(expand = expansion(mult = c(0,0.05))) +
        scale_x_discrete(labels = c(
          "Vacina Covid-19 - Covishield"="Covishield", 
          "Covid-19-Coronavac-Sinovac/Butantan"="Coronavac",
          "Vacina covid-19 - BNT162b2 - BioNTech/Fosun Pharma/Pfizer"="Pfizer",
          "Vacina covid-19 - Ad26.COV2.S - Janssen-Cilag" ="Janssen",
          "Covid-19-AstraZeneca"="Astrazeneca"
        )) +
        theme_classic() +
        theme(
          panel.background = element_rect(fill = "transparent", colour = NA),
          axis.title = element_blank(),
          plot.title = element_text(),
          axis.text = element_text(color = "#555555"),
          axis.line.y = element_blank(),
          axis.line.x = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)
        ),
      tooltip = "text" ,
      textposition = 'outside')
  })

  # Plot por raça
  output$raca <- renderPlotly({

    # Transformação
    dados <- df()
    dados_tipo <- dados %>%
      group_by(
        paciente_racacor_valor
      ) %>%
      summarise(
        Vacinas = n()
      )%>%
      mutate(
        paciente_racacor_valor = fct_reorder(
          paciente_racacor_valor, Vacinas, .desc = TRUE
          )
      )

    # Gráfico
    ggplotly(
      ggplot(dados_tipo) +
        geom_col(
          aes(x = (paciente_racacor_valor),
              y = Vacinas,
              text = paste("Doses Aplicadas:",Vacinas)
          ), 
          width = 0.7, 
          stat = "identity",
          fill = '#524EEE'
        ) +
        scale_y_continuous(expand = expansion(mult = c(0,0.05))) +
        scale_x_discrete(labels = c(
          "PARDA"="Parda", 
          "AMARELA"="Amarela",
          "BRANCA"="Branca",
          "PRETA" ="Preta",
          "INDIGENA"="Indigena",
          "SEM INFORMACAO"="Sem Info"
        )) +
        theme_classic() +
        theme(
          panel.background = element_rect(fill = "transparent", colour = NA),
          axis.title = element_blank(),
          plot.title = element_text(),
          axis.text = element_text(color = "#555555"),
          axis.line.y = element_blank(),
          axis.line.x = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_text()
        ),
      tooltip = "text")
  })
  
})
