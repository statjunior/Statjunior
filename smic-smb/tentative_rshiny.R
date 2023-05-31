library(shiny)
library(ggplot2)


# Interface utilisateur
ui <- fluidPage(
  plotOutput("plot_salaire_reel")
)

# Serveur
server <- function(input, output) {
  output$plot_salaire_reel <- renderPlot({
    ggplot(data = base_sal_reel,
           aes(x = DATE)) +
      geom_line(aes(y = smic, color = "SMIC"), size = 1.3) + 
      geom_line(aes(y = smbs, color = "SMB"), size = 1.3) + 
      geom_line(aes(y = smbo, color = "SMB Ouvriers"), size = 1) + 
      geom_line(aes(y = smbe, color = "SMB Employés"), size = 1) + 
      geom_line(aes(y = smbpi, color = "SMB Professions intermédiaires"), size = 1) + 
      geom_line(aes(y = smbc, color = "SMB Cadres"), size = 1) +
      labs(title = "Evolution du pouvoir d'achat du SMIC et du Salaire mensuel de base depuis la crise du Covid", 
           subtitle = "Lecture : Au 1er trimestre 2023, le pouvoir d'achat du Salaire mensuel de base recule de 1,4 % par rapport au 2e trimestre 2017. \nSource : Dares (Salaires de base), Insee (Smic mensuel brut, Indice des prix à la consommation) \nCalculs et graphiques : @statjunior", 
           y = "Variation par rapport au 2e trimestre 2017 (%)", x = "Date") +
      scale_x_date(limits = as.Date(c("2017-06-01", NA))) + 
      scale_color_manual(values = c("SMB"="red", "SMB Cadres" = "orange", "SMIC" = "#C77CFF", "SMB Employés" ="#00BA38", "SMB Ouvriers" = "#00B8E7", "SMB Professions intermédiaires" = "#D39200"))+
      theme_grey()+
      theme(legend.position="bottom",
            legend.title = element_text(size=9), 
            legend.text = element_text(size=12),
            plot.title = element_text(color = "#333333", size = 18, face = "bold"),
            plot.subtitle = element_text(color = "#333333", size = 14,face = "italic")) + 
      ylim(-5,5)
  })
}

# Application Shiny
shinyApp(ui, server)




library(shiny)
library(ggplot2)
library(dplyr)

# Chargement des données
base_sal_reel$DATE <- as.Date(base_sal_reel$DATE)

# Soustraire la première valeur de chaque série par la valeur de la première année affichée
base_sal_reel_adj <- base_sal_reel %>%
  mutate_at(vars(2:10), ~ . - .[which.min(DATE) & !is.na(.)])

# Définition de l'interface utilisateur
ui <- fluidPage(
  titlePanel("Evolution du pouvoir d'achat du salaire mensuel de base par secteur d'activité"),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("date_range", label = "Sélectionnez une période :", 
                     start = "2017-06-01", end = "2023-03-01",
                     min = "2017-06-01", max = "2023-03-01",
                     separator = " - "),
      selectInput("secteur", label = "Sélectionnez un secteur :", 
                  choices = c("Industrie", "Construction", "Tertiaire"))
    ),
    mainPanel(
      plotOutput("graph")
    )
  )
)

# Définition du serveur
server <- function(input, output) {
  
  # Filtrage des données selon les sélections de l'utilisateur
  data <- reactive({
    base_sal_reel_adj %>%
      filter(DATE >= input$date_range[1] & DATE <= input$date_range[2])
  })
  
  # Création du graphique
  output$graph <- renderPlot({
    ggplot(data = data(), aes(x = DATE)) +
      geom_line(aes_string(y = paste0("smb_", tolower(input$secteur)), color = paste0("SMB ", input$secteur)), size = 1.3) + 
      labs(title = "Evolution du pouvoir d'achat du salaire mensuel de base par secteur d'activité", 
           subtitle = paste("Lecture : Au", format(input$date_range[2], "%B %Y"), 
                            ", le pouvoir d'achat du Salaire mensuel de base", input$secteur, 
                            "est en variation de", round(data() %>% 
                                                           filter(DATE == input$date_range[2]) %>% 
                                                           pull(!!sym(paste0("smb_", tolower(input$secteur)))), 2), 
                            "par rapport à", format(input$date_range[1], "%B %Y"), ".\n", 
                            "Source : Dares (Salaires de base), Insee (Indice des prix à la consommation)\n", 
                            "Calculs et graphiques : @statjunior"), 
           y = "Variation par rapport à", x = "Date") +
      scale_x_date(limits = as.Date(c(input$date_range[1], NA))) + 
      scale_color_manual(values = c("SMB Industrie" = "brown", "SMB Construction" = "#C77CFF", "SMB Tertiaire" ="#00BA38"))+
      theme_grey()+
      theme(legend.position="bottom",
            legend.title = element_text(size=9), 
            legend.text = element_text(size=12),
            plot.title = element_text(color = "#333333", size = 18, face = "bold"),
            plot.subtitle = element_text(color = "#333333", face = "italic")) + 
      ylim(-5,5)
  })
  
  output$info <- renderText({
    point <- nearPoints(base_sal_reel, input$plot_click$x, input$plot_click$y, maxpoints = 1)
    if (nrow(point) == 0) return()
    paste("Date : ", as.character(point$DATE), "\n",
          "SMB Industrie : ", as.character(point$smb_industrie), "\n",
          "SMB Construction : ", as.character(point$smb_construction), "\n",
          "SMB Tertiaire : ", as.character(point$smb_tertiaire))
  })
}

shinyApp(ui = ui, server = server)