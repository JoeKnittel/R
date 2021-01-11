library(shiny)

# Define UI for data upload app ----
ui <- fluidPage(
    # App title ----
    titlePanel("Discrete Distributions:"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # Input: Select a file ----
            fileInput("file1", "Upload CSV File:",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),

            # Dropdown to choose column ----
            uiOutput("dropdown"),
            
            uiOutput("hr1"),
            
            uiOutput("header"),
            
            uiOutput("sep"),
            
            uiOutput("hr2")
                                  
            
        ),
        
        
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Data file ----
#            tableOutput("contents")
            
        )
        
    )
)

library(ggplot2)

# Define server logic to read selected file ----
server <- function(input, output) {

    output$header <- renderUI({
        req(input$file1)
        checkboxInput("header", "Has Header Row?", TRUE)
    })
    
    output$sep <- renderUI({
        
        req(input$file1)

        radioButtons("sep", "Delimiter",
                     choices = c(Comma = ",",
                                 Semicolon = ";",
                                 Tab = "\t"),
                     selected = ",")
        
    })

    output$hr1 <- renderUI({
        req(input$file1)    
        # Horizontal line ----
        tags$style(HTML("hr {border-top: 1px solid #CCCCCC;}"))
    })    
    
    output$hr2 <- renderUI({
        req(input$file1)    
        # Horizontal line ----
        tags$hr()
    })    
    
        
    output$dropdown <- renderUI({

        req(input$file1)

        tryCatch(
            {
                df <- read.csv(input$file1$datapath,
                               header = input$header,
                               sep = input$sep,
                               quote = input$quote)
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )

        selectInput("column", "Choose Variable:", choices = colnames(df))

    })

    
        
    # data <- reactive({read.csv(input$file1$datapath,header = input$header, sep=input$sep)})
    # 
    #     
    # 
    # output$dropdown <- renderUI({
    #     selectInput("column", "Test", choices = c("a","b"))
    # })
}

# Create Shiny app ----
shinyApp(ui, server)