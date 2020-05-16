library(shiny)
wine = read.csv(file = "wine.quality.csv")



ui <- fluidPage(
  pageWithSidebar(
    
    headerPanel("Predicting wine quality"),
    
    sidebarPanel(
      sliderInput("fixed.acidity", "Fixed acidity",
                  min = 0, max = 2,
                  value = 0.7, step = 0.1),
      sliderInput("volatile.acidity", "Volatile acidity",
                  min = 0, max = 2,
                  value = 0.7, step = 0.1),
      sliderInput("citric.acid", "Citric acid",
                  min = 0, max = 2,
                  value = 0.7, step = 0.1),   
      sliderInput(inputId = "residual.sugar",
                   label = "Residual sugar",
                   min = 0, max = 100, value = 40),
      sliderInput("chlorides", "Chlorides",
                  min = 0, max = 2,
                  value = 0.7 , step = 0.1),
      sliderInput(inputId = "free.sulfur.dioxide",
                  label = "Free sulfur dioxide",
                  min = 0, max = 300, value = 150),
      sliderInput(inputId = "density",
                  label = "Density",
                  min = 0.5, max = 1.5, value = 1, step = 0.1),
      sliderInput(inputId = "pH",
                  label = "pH",
                  min = 1, max = 4, value = 2, step = 0.1),
      sliderInput(inputId = "sulphates",
                  label = "Sulphates",
                  min = 0, max = 2, value = 1, step = 0.1),
      sliderInput(inputId = "alcohol",
                  label = "Alcohol",
                  min = 0, max = 40, value = 12)

    ),
    
    
    mainPanel(

      h3("Age appears best in four things: old wood to burn, old wine to drink, old friends to trust and old authors to read. - Francis Bacon - "),
      h4("Overview"),
      h5("As I cannot burn old wood in my townhouse and old friends are not accessible at this time due  to social distancing, I thought it would be fun to learn more about good wine to enjoy with my books. The goal of the project is to predict wine quality based on physicochemical data. The data comes from Portuguese Vinho Verde wine, including quality score, which was used as label. Linear regression with lasso was performed to  shrink the coefficients of the variables. The model results in RMSE of 0.76. The linear regression formula was used in this Shiny app to predict the wine's quality score."),
      h4("Instruction"),
      h5("Use the sliders to ajust the values of wine's physicochemical properies to calcualate the wine's quality score. Higher score means objectively better wine."),
      h4("Github"),
      h5(tags$a("https://github.com/vy-p-tran/Predicting-wine-quality")),
      h4("Data source"),
      h5("Paulo Cortez, University of Minho, Guimaraes, Portugal"),
      h5(tags$a("http://www3.dsi.uminho.pt/pcortez")),
      h2("The predicted quality score for this wine is:"),
      h2(textOutput("value"))
  
      )
  )
)

server <- function(input, output) {

  output$value <- renderPrint({ 
    76.13340844 - 0.01000893*input$fixed.acidity - 1.68114115 * input$volatile.acidity + 0.05803395* input$citric.acid + 
      0.04764393 * input$residual.sugar -0.84030287*input$chlorides + 0.00368378 * input$free.sulfur.dioxide -74.85531765 * input$density + 0.35165708*input$pH + 
      0.38901146*input$sulphates +  0.28111621 * input$alcohol
  })
}

shinyApp(ui, server)