library(shiny)

# Define UI for application that draws a histogram
# ui <- fluidPage(
#   titlePanel("censusVis"),
#   
#   sidebarLayout(
#     sidebarPanel(
#       helpText("Create demographic maps with 
#                information from the 2010 US Census."),
#       
#       selectInput("var", 
#                   label = "Choose a variable to display",
#                   choices = c("Percent White", 
#                               "Percent Black",
#                               "Percent Hispanic", 
#                               "Percent Asian"),
#                   selected = "Percent White"),
#       
#       sliderInput("range", 
#                   label = "Range of interest:",
#                   min = 0, max = 100, value = c(0, 100))
#       ),
#     
#     mainPanel(
#       textOutput("selected_var")
#     )
#   )
#   )
# library(readr)
# spc_data <- read_csv("s_univariate/data/diagJ5_loose1.txt")
library(readxl)
book1 <- read_excel("data/book1.xlsx", col_names = FALSE)


ui <- fluidPage(
  titlePanel("Doing simulatioin"),
  
  sidebarLayout(
    sidebarPanel(
      helpText(strong("univariate spc:"),
               br(),
               "xbar chart",
               br(),
               "R chart",
               br(),
               strong("multivariate spc:"),
               br(),
               "Hotelling chart"
               ),
      
      selectInput("var", 
                  label = "Choose a variable to display",
                  choices = c("Voltage", 
                              "Torque",
                              "Angle"
                              ),
                  selected = "Voltage"),
      # tableOutput(),
      sliderInput("range", 
                  label = "Range of interest:",
                  min = 0, max = 1000, value = c(0, 100))
    ),
    
    mainPanel(
      textOutput("selected_var"),
      textOutput("min_max"),
      plotOutput(outputId = "distPlot")
    )
  )
)


server <- function(input, output) {
  
  output$selected_var <- renderText({ 
    paste("You have selected", input$var)
  })
  
  output$min_max <- renderText({
    paste("You have chosen a range that goes from",
          input$range[1], "to", input$range[2])
  })
  
  output$distPlot <- renderPlot ({
    qcc(data = book1,type = "R")
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
## Only run this example in interactive R sessions
# if (interactive()) {
#   # table example
#   shinyApp(
#     ui = fluidPage(
#       fluidRow(
#         column(12,
#                tableOutput('table')
#         )
#       )
#     ),
#     server = function(input, output) {
#       output$table <- renderTable(iris)
#     }
#   )
#   
#   
#   # DataTables example
#   shinyApp(
#     ui = fluidPage(
#       fluidRow(
#         column(12,
#                dataTableOutput('table')
#         )
#       )
#     ),
#     server = function(input, output) {
#       output$table <- renderDataTable(iris)
#     }
#   )
# }
