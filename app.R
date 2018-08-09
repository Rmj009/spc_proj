# # 練習 ----------------------------------------------------------------------
# 
# 
# 
# ui <- fluidPage(
#   titlePanel("Monitoring Process"),
#   fluidRow(
#     
#     column(2,
#            h3("Buttons"),
#            actionButton("action", "Action"),
#            br(),
#            br(), 
#            submitButton("Submit")),
#     
#     
#     column(3, 
#            checkboxGroupInput("checkGroup", 
#                               h3("Checkbox group"), 
#                               choices = list("Choice 1" = 1, 
#                                              "Choice 2" = 2, 
#                                              "Choice 3" = 3),
#                               selected = 1)),
#     column(3,
#            selectInput("select", h3("Select box"), 
#                        choices = list("Choice 1" = 1, "Choice 2" = 2,
#                                       "Choice 3" = 3), selected = 1)),
#     column(3, 
#            sliderInput("slider1", h3("Sliders"),
#                        min = 0, max = 1000, value = 10),
#            sliderInput("slider2",h3("Range"),
#                        min = 0, max = 1000, value = c(100, 200))),
#     
#     column(3, 
#            numericInput("num", 
#                         h3("Numeric input"), 
#                         value = 1))
#   ),
#   sidebarLayout(
#     sidebarPanel(
#       h2("Installation"),
#       p("Shiny is available on CRAN, so you can install it in the usual way from your R console:"),
#       code('install.packages("shiny")'),
#       br(),
#       br(),
#       br(),
#       br(),
#       # img(src = "", height = 500, width = 720), #img must be in a folder named www
#       img(src = "Mainframe1.png", height = 70, width = 200),
#       br(),
#       "Shiny is a product of ", 
#       span("RStudio", style = "color:blue")
#     ),
#     mainPanel(
#       # h6("Episode IV", align = "center"),
#       # h6("A NEW HOPE", align = "center"),
#       # h5("It is a period of civil war.", align = "center"),
#       # h4("Rebel spaceships, striking", align = "center"),
#       # h3("from a hidden base, have won", align = "center"),
#       # h2("their first victory against the", align = "center"),
#       # h1("evil Galactic Empire."), # 要+逗號
#       # 
#       # p("p creates a paragraph of text."),
#       # p("A new p() command starts a new paragraph. Supply a style attribute to change the format of the entire paragraph.", style = "font-family: 'times'; font-si16pt"),
#       # strong("strong() makes bold text."),
#       # em("em() creates italicized (i.e, emphasized) text."),
#       # br(),
#       # code("code displays your text similar to computer code"),
#       # div("div creates segments of text with a similar style. This division of text is all blue because I passed the argument 'style = color:blue' to div", style = "color:blue"),
#       # br(),
#       # p("span does the same thing as div, but it works with",
#       #   span("groups of words", style = "color:blue"),
#       #   "that appear inside a paragraph.")
#       # 
#     )
#   )
# )
# 
# # Define server logic required to draw a histogram
# server <- function(input, output) {
#    
#    output$distPlot <- renderPlot({
#       # generate bins based on input$bins from ui.R
#       x    <- faithful[, 2] 
#       bins <- seq(min(x), max(x), length.out = input$bins + 1)
#       
#       # draw the histogram with the specified number of bins
#       hist(x, breaks = bins, col = 'darkgray', border = 'white')
#    })
# }
# 
# # Run the application 
# shinyApp(ui = ui, server = server)
# 
# # source("s_uni2/data/book1.xlsx")
# 
# 


# Data Maniuplation -------------------------------------------------------
library(shiny)
library(readxl)
library(mapproj)
library(maps)


# counties <- readRDS("s_uni2/data/counties.rds")
# book1 <- read_excel("s_uni2/data/book1.xlsx", col_names = FALSE)
# data(pistonrings)
data("diamonds")
taje = with(pistonrings, qcc.groups(diameter, sample))
# load()
# normaldata<-source("s_uni2/data/normal01_3Pnormal_1.txt")
# library(readr)
# normaldata <- read_csv(sample(c("s_uni2/data/normal01_3Pnormal_1.txt","s_uni2/data/normal01_3Pnormal_2.txt","s_uni2/data/normal01_3Pnormal_3.txt"),1), 
                                # col_names = FALSE)
# normdat<-replicate(3,sample(normaldata[1:30],1,replace = FALSE))
# sample(normaldata[normaldata>1],1,replace=TRUE)

# UI Design ---------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Doing simulatioin"),
  
  sidebarLayout(
    
    sidebarPanel(
      fluidRow(helpText(strong("univariate spc:"),
                        br(),
                        "xbar chart",
                        br(),
                        "R chart",
                        br(),
                        strong("multivariate spc:"),
                        br(),
                        "Hotelling chart")
      ),
      
      selectInput("selt", h1("Charts"), 
        choices = c("Histogram",
                    "R",
                    "xbar" ,
                    "xbar.one",
                    "CUSUM",
                    "EWMA",
                    "Hotelling chart" = 3), selected = "R"),
      selectInput("var",
                  label = "Variables Correlation",
                  choices = c("Voltage", 
                              "Torque",
                              "Angle",
                              "CPI"
                              ),
                  selected = "Voltage"),
   
      sliderInput("range", 
                  label = "Range of interest:",
                  min = 0, max = 1000, value = c(7,77)),
      numericInput("num", 
                   h3("Numeric input"), 
                   value = 1)
      
    ),
    
    mainPanel(
      textOutput("selected_var"),
      textOutput("min_max"),
      # tableOutput("SummaryTable"),
      # checkboxGroupInput(), 
      plotOutput("SpcPlot1"),
      # plotOutput("SpcPlot2"),
      plotOutput("distPlot")
    )
  )
)

## source(目錄裡的R檔案OPEN) open working directory 
## source("s_uni-bind/T2stats.R")
# stats.T2(pistonrings)






# Server Design -----------------------------------------------------------
if (interactive()) {}



server <- function(input, output, session) {
  
  output$selected_var <- renderText({ 
    paste("You have selected", input$var)
    # paste("You have selected", input$var2)
    # paste("You have selected", input$var3)

    
  })
  output$min_max <- renderText({
    paste("You have chosen a range that goes from",
          input$range[1], "to", input$range[2])
  })
  output$SummaryTable<-renderTable({

    input$add<-summary(cusum.thick)
    print(input$add)
    
  })
  
  output$distPlot <- renderPlot ({
    
    
    
    spcdata<-switch(input$var,
                    "Voltage"= qcc(data = torF,type = "R"),
                    "Torque" = diamonds$price,
                    "Angle" =diamonds$depth,
                    "CPI" = cp(x= data.frame(taje),lsl=73.99,usl=74.03,target=74.02) # composite Cp Indcies
                    # process.capability(qcc(data = taje,type = "xbar"),spec.limit =c(input$range[1],input$range[2])) # xbar CPI , assign specification value
                    # ss.study.ca(pistonrings$diameter,LSL=73.9,USL=74.1,Target=74) # composite Cpk/Cp/Pk
                      
                    )
    # qcc(data = book1,type = "xbar.one")
    # qcc(data = book1,type = "xbar")
    plot(spcdata)
  })
  output$SpcPlot1<-renderPlot({
    
    d1<- sample(diamonds$color,30)
    d2<- sample(diamonds$depth,30)
    d3<- sample(diamonds$price,30)
    Frdp<-diamonds$depth
    torF<-data.frame(d1,d2,d3)
    uspc<-switch(input$selt,
           "Histogram"=hist(Frdp[seq_len(input$range)]),
           "xbar.one"=qcc(data = torF,type = "xbar.one"),
           "R"=qcc(data = torF,type = "R"),
           "xbar"=qcc(torF[1:20,], type="xbar", newdata=torF[21:30,]),
           "CUSUM" = cusum(data= torF),#print(summary(cusum.thick)),
           "EWMA" = ewma(data = torF)
           
           
            # "Hotelling chart"=
           
    )
    plot(uspc)
  })
  
  # output$SpcPlot2<-renderPlot({
  #   effect<-"Electrical hazards"
  #   causes.gr<-c("nonstationary voltage","high voltage","power supply")
  #   causes<-vector(mode="list",length=length(causes.gr))
  #   causes[1]<-list(c("atmosphere", "wires"))
  #   causes[2]<-list(c("electricty unstable"))
  #   causes[3]<-list(c("electricty unbearable","twinCAT abnormal", "Short circuit"))
  #   plt<-switch(input$range,
  #               ss.ceDiag(effect, causes.gr, causes,sub="Electrial hazard Problem")
  #               
  #               )
  #   plot(plt)
  # })
  # 
  
  #     ## economicExample ---------------------------------------------------------
  # 
  # 
  # data <- getSymbols(input$symb,
  #                    from = input$dates[1],
  #                    to = input$dates[2],
  #                    auto.assign = FALSE)
  # 
  # 
  # dataInput <- reactive({  
  #   getSymbols(input$symb, src = "google",
  #              from = input$dates[1],
  #              to = input$dates[2],
  #              auto.assign = FALSE)
  # })
  # 
  # finalInput <- reactive({
  #   if (!input$adjust) return(dataInput())
  #   adjust(dataInput())
  # })
  # 
  # output$plot <- renderPlot({
  #   chartSeries(finalInput(), theme = chartTheme("white"),
  #               type = "line", log.scale = input$log, TA = NULL)})
  # 
  # 

  }


# Run the application 
shinyApp(ui = ui, server = server)

# runApp(list(ui = ui, server = server))



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

