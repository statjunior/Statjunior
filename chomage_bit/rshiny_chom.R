library(shiny)
library(ggplot2)

library(shiny)
library(ggplot2)

library(shiny)
library(ggplot2)
library(scales)

library(shiny)
library(ggplot2)
library(plotly)


# Interface utilisateur
# Interface utilisateur
ui <- fluidPage(
  titlePanel("Exemple de graphique interactif"),
  sidebarPanel(
    selectInput("variable", "Variable:", 
                choices = unique(tx_emploi_sexe$sexe)),
    sliderInput("range", "Range:", 
                min = min(tx_emploi_sexe$DATE), max = max(tx_emploi_sexe$DATE), value = c(min(tx_emploi_sexe$DATE), max(tx_emploi_sexe$DATE))),
    radioButtons("gender", "Genre:", choices = c("Toutes"="all", "Hommes"="M", "Femmes"="F")),
    textInput("search", "Recherche:", ""),
    actionButton("clear", "Effacer")
  ),
  mainPanel(
    plotOutput("plot", hover = "plot_hover"),
    verbatimTextOutput("info")
  )
)

# Serveur
server <- function(input, output, session) {
  
  data <- reactive({
    df <- tx_emploi_sexe
    df <- subset(df, df$DATE >= input$range[1] & df$DATE <= input$range[2])
    if (input$search != "") {
      df <- subset(df, grepl(input$search, df$OBS_VALUE, ignore.case = TRUE))
    }
    if (input$gender != "all") {
      df <- subset(df, df$sexe == input$gender)
    }
    df
  })
  
  output$plot <- renderPlot({
    ggplot(data(), aes(x = DATE, y = OBS_VALUE, color = sexe)) +
      geom_line(size = 1.1) +
      scale_x_date(date_labels = "%Y") +
      labs(x = "Année", y = input$variable) +
      theme(legend.position = "bottom")
  })
  
  output$info <- renderPrint({
    if (is.null(input$plot_hover)) {
      return(NULL)
    } else {
      paste0("Valeur de ", input$variable, " en ", input$plot_hover$disp, ": ", 
             round(as.numeric(input$plot_hover[[input$variable]]), 2))
    }
  })
  
  observeEvent(input$clear, {
    updateTextInput(session, "search", value = "")
  })
  
}

# Lancement de l'application Shiny
shinyApp(ui, server)

library(ggplot2)
library(plotly)

library(ggplot2)
library(plotly)

# UI
ui <- fluidPage(
  titlePanel("Exemple de graphique interactif"),
  sidebarPanel(
    selectInput("variable", "Variable:", 
                choices = unique(tx_emploi_sexe$sexe)),
    sliderInput("range", "Range:", 
                min = min(tx_emploi_sexe$DATE), max = max(tx_emploi_sexe$DATE), 
                value = c(min(tx_emploi_sexe$DATE), max(tx_emploi_sexe$DATE))),
    radioButtons("gender", "Genre:", 
                 choices = c("Toutes"="all", "Hommes"="M", "Femmes"="F")),
    actionButton("reset", "Réinitialiser")
  ),
  mainPanel(
    plotlyOutput("plot"),
    verbatimTextOutput("info")
  )
)

# Serveur
server <- function(input, output, session) {
  
  data <- reactive({
    df <- tx_emploi_sexe
    df <- subset(df, df$DATE >= input$range[1] & df$DATE <= input$range[2])
    if (input$gender != "all") {
      df <- subset(df, df$sexe == input$gender)
    }
    df
  })
  
  output$plot <- renderPlotly({
    gg <- ggplot(data(), aes(x = DATE, y = OBS_VALUE, color = sexe)) +
      geom_line(size = 1.1) +
      scale_x_date(date_labels = "%Y") +
      labs(x = "Année", y = input$variable) +
      theme(legend.position = "bottom")
    ggplotly(gg, tooltip = c("x", "y"))
  })
  
  output$info <- renderPrint({
    if (is.null(event_data("plotly_hover"))) {
      return(NULL)
    } else {
      paste0("Valeur de ", input$variable, " en ", event_data("plotly_hover")[["x"]], ": ", 
             round(event_data("plotly_hover")[["y"]], 2))
    }
  })
  
  observeEvent(input$reset, {
    updateSliderInput(session, "range", value = c(min(tx_emploi_sexe$DATE), max(tx_emploi_sexe$DATE)))
    updateRadioButtons(session, "gender", selected = "all")
  })
  
}

# Lancement de l'application Shiny
shinyApp(ui, server)