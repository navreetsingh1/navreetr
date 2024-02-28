install.packages("shiny")

install.packages("ggplot2")
# Load necessary libraries
library(shiny)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel("Multiple Linear Regression App"),
  sidebarLayout(
    sidebarPanel(
      numericInput("input_x1", "Enter X1:", value = 1),
      numericInput("input_x2", "Enter X2:", value = 1),
      actionButton("submit_button", "Submit")
    ),
    mainPanel(
      plotOutput("regression_plot")
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Function to perform multiple linear regression
  perform_linear_regression <- function(x1, x2) {
    # Assuming a multiple linear regression equation y = 2*x1 + 3*x2 + error
    y <- 2 * x1 + 3 * x2 + rnorm(1) * 3
    return(data.frame(x1 = x1, x2 = x2, y = y))
  }
  
  # Reactive expression for the regression data
  regression_data <- reactive({
    perform_linear_regression(input$input_x1, input$input_x2)
  })
  
  # Render the scatter plot
  output$regression_plot <- renderPlot({
    ggplot(regression_data(), aes(x = x1, y = y, color = x2)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = "Multiple Linear Regression Plot", x = "X1", y = "Y")
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
