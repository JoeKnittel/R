library(shiny)
library(shinyjs)
library(ggplot2)
library(fitdistrplus)

hidden(span(id = "a"), div(id = "b"))

# Define UI for data upload app ----
ui <- navbarPage(id = "nav", title = "Discrete Random Variable Analysis", 

           tabPanel(value = "input", title = "Input",
              
              fluidPage(
                  
                  useShinyjs(),
                       
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
                         
                         uiOutput("hr2"),
                         
                         uiOutput("button")                      
                         
                     ),
                     mainPanel(
                         
                            # Output: Data file ----
                            # tableOutput("contents")
                      )
                 )
              )
           ),
           tabPanel(value = "output", title = "Output",
                    fluidPage(
                        tags$div(class = "load", id = "loadtxt",
                                 tags$h4("Please load some data to model...")          
                        ),
                        tabsetPanel(id = "distributions",
                            tabPanel(value = "pois", title = "Poisson", br(),
                                sidebarLayout(
                                    sidebarPanel(
                                        tableOutput("poissonTable"),
                                        verbatimTextOutput("poissonSummary")
                                    ),
                                    mainPanel(
                                        plotOutput("poissonPlot")         
                                    )
                                )
                            ),
                            tabPanel(value = "nb", title = "Negative Binomial", br(),
                                     sidebarLayout(
                                         sidebarPanel(
                                             tableOutput("negbinTable"),
                                             verbatimTextOutput("negbinSummary")
                                         ),
                                         mainPanel(
                                             plotOutput("negbinPlot")         
                                         )
                                     )
                            ),
                            tabPanel(value = "bin", title = "Binomial", br(),
                                     sidebarLayout(
                                         sidebarPanel(
                                             tableOutput("binTable"),
                                             uiOutput("binSizeSlider"),
                                             uiOutput("binProbSlider"),
                                             verbatimTextOutput("binSummary")
                                         ),
                                         mainPanel(
                                             plotOutput("binPlot")         
                                         )
                                     )
                            )
                        )
                    )
           ),
           tabPanel(value = "about", title = "About",
                    fluidPage(
     #                   tags$h4("about page test")        
                        tags$p("This app was designed to help facilitate the process of fitting a distribution to a discrete random variable. 
                        To try it, just go to the 'Input' tab and upload a CSV file. Make sure to select the appropriate settings, then click the 'Run Analysis' 
                        button at the bottom. This will take you to the 'Output' tab, where you can observe some possible distributions. If the dispersion is not
                        significantly different than 1, the best choice is regular Poisson model, but if either under- or overdispersion is present, alternative models 
                        are suggested: for instance, a Binomial (underdispersion) or a Negative Binomial model (overdispersion) might be a better choice. It's also 
                        possible to modify the Poisson model to better fit the data; this is called a Zero-Modified (ZM) Poisson model, and it is presented as an option, 
                        if dispersion is present. When choosing a final model, it's a good idea to compare the AIC and BIC values, as well as the p-value of the Chi-square 
                        test in each case. All of this information is presented.")
                    )
           )
    )


# Define server logic to read selected file ----
server <- function(input, output, session) {

    hideTab(inputId = "distributions", target = "pois")
    hideTab(inputId = "distributions", target = "nb")
    hideTab(inputId = "distributions", target = "bin")
    
    output$header <- renderUI({
        req(input$file1)
        checkboxInput("header", "File Contains Header?", TRUE)
    })

    # function to access inputted data, if present
    getData <- reactive({
        req(input$file1)
        req(input$sep)
        
        tryCatch(
            {
                df <- read.csv(input$file1$datapath,
                               header = input$header,
                               sep = input$sep,
                               quote = input$quote,
                               check.names = FALSE)
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )
        
        df
    })
        
    observeEvent(input$button,{
        shinyjs::hide("loadtxt")
        showTab(inputId = "distributions", target = "pois")
        showTab(inputId = "distributions", target = "nb")
        showTab(inputId = "distributions", target = "bin")
        updateTabsetPanel(session, "distributions", selected = "pois")
        updateNavbarPage(session, "nav", selected = "output")
    })
    
    output$sep <- renderUI({
        
        req(input$file1)
        
        radioButtons("sep", "Choose Delimiter:",
                     choices = c(Comma = ",",
                                 Semicolon = ";",
                                 Tab = "\t"),
                     selected = ",")
        
    })

    output$hr1 <- renderUI({
        req(input$file1)    
        tags$style(HTML("hr {border-top: 1px solid #CCCCCC;}"))
    })    
    
    output$hr2 <- renderUI({
        req(input$file1)    
        # Horizontal line ----
        tags$hr()
    })    
    
        
    output$dropdown <- renderUI({
        df <- getData()
        selectInput("column", "Choose Discrete Variable:", choices = colnames(df))
    })

    output$button <- renderUI({
        req(input$file1)
        actionButton("button", "Run Analysis", style = "width:100%")
    })
    
    output$poissonTable <- renderTable({
    #    df <- getData()
    #    return (head(df[,input$column]))
    })
    
    output$poissonSummary <- renderPrint({
        df <- getData()
        fitp <- fitdistrplus::fitdist(df[,input$column], "pois")
        return (summary(fitp))
    })
    
    output$poissonPlot <- renderPlot({
        df <- getData()
        fitp <- fitdistrplus::fitdist(df[,input$column], "pois")
       # windows.options(width=10, height=10)
        return (
            plot(fitp)
        )
    })
    
    output$negbinTable <- renderTable({
        
    })
    
    output$negbinSummary <- renderPrint({
        df <- getData()
        fitnb <- fitdistrplus::fitdist(df[,input$column], "nbinom")
        return(summary(fitnb))
    })
    
    output$negbinPlot <- renderPlot({
        df <- getData()
        fitnb <- fitdistrplus::fitdist(df[,input$column], "nbinom")
        return(plot(fitnb))
    })
    
    output$binTable <- renderTable({
        
    })
    
    output$binSizeSlider <- renderUI({
        df <- getData()
        sliderInput(inputId = "binSizeSlider", label = "m", min = max(df[,input$column]), max = 4*max(df[,input$column]), 
                    value = 2*max(df[,input$column]))
    })

    output$binProbSlider <- renderUI({
        df <- getData()
        sliderInput(inputId = "binProbSlider", label = "p", min = 0, max = 1, 
                    value = 0.5)
    })
    
    output$binSummary <- renderPrint({
        df <- getData()
        fitbin <- fitdistrplus::fitdist(df[,input$column], dist = "binom", start = list(prob = input$binProbSlider),
                                        fix.arg = list(size = input$binSizeSlider))
        return(summary(fitbin))
    })
    
    output$binPlot <- renderPlot({
        df <- getData()
        fitbin <- fitdistrplus::fitdist(df[,input$column], dist = "binom", start = list(prob = input$binProbSlider),
                                        fix.arg = list(size = input$binSizeSlider))
        return(plot(fitbin))
    })
    
}

# Create Shiny app ----
shinyApp(ui, server)