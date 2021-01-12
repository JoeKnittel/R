library(shiny)
library(shinyjs)
library(ggplot2)
library(fitdistrplus)
library(gamlss.dist)

#hidden(span(id = "a"), div(id = "b"))

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
                            ),
                            tabPanel(value = "sum", title = "Summary", br(),
                                sidebarLayout(
                                    sidebarPanel(
                                        verbatimTextOutput("summaryGoF")
                                    ),
                                    mainPanel(
                                        plotOutput("summaryPlot")
                                    )
                                )             
                            )
                        )
                    )
           ),
           tabPanel(value = "about", title = "About",
                    fluidPage(
                        
                        withMathJax(),

                        tags$div(HTML("<script type='text/x-mathjax-config'>
                            MathJax.Hub.Config({
                            tex2jax: {inlineMath: [['$','$']]}
                            });
                            </script>
                        ")),
                        
                        tags$style(HTML("hr {border-top: 1px solid #dddddd;}")),
                        
          #              helpText('An irrational number $\\sqrt{2}$
          # and a fraction $1-\\frac{1}{2}$'),
                        
                        p("This app was designed to facilitate the process of fitting a distribution to a discrete random variable."),
                        
                        HTML("<p>To try it, just click on the <i>Input</i> tab and upload a CSV file. Make sure to select the appropriate settings for the given file, then click the <i>Run Analysis</i> button at the bottom. This will take you to the <i>Output</i> tab, where you can observe some distributions whose parameters are optimized based on their MLE.</p>"),
                        
                        h3("Distributions"),
                        
                        hr(),
                        
                        h4("Poisson Model"),
                        
                        p("The simplest of the models presented is the Poisson model. This model is commonly used when modeling discrete random variables, but in its definition is an assumption that sometimes leads to a suboptimal fit: $\\text{E}[\\text{N}] = \\text{Var}[\\text{N}]$. If the data show clear evidence of under- or overdispersion, then one of the other models is probably a better choice."),
                        
                        hr(),
                        
                        h4("Negative Binomial Model"),
                        
                        p("If the variable exhibits overdispersion (i.e., $\\text{Var}[\\text{N}] > \\text{E}[\\text{N}]$), the Negative Binomial distribution is probably a good choice. Essentially, the Negative Binomial model is an extension of the Poisson model; it treats the random variable $N$ as a mixture of Poisson and Gamma processes: $(N|\\lambda) \\sim Pois(\\lambda)$, where $\\lambda \\sim Gamma(\\alpha, \\theta)$, resulting in an unconditional distribution $N \\sim NB(\\alpha, \\theta)$."),
                        
                        hr(),
                        
                        h4("Binomial Model"),
                        
                        p("If the variable exhibits clear underdispersion (i.e., $\\text{Var}[\\text{N}] < \\text{E}[\\text{N}]$), the Binomial distribution is probably a good choice. This is because, by definition, $\\text{E}[\\text{N}] = mq$ and $\\text{Var}[\\text{N}]=mq(1-q)$. Hence, $\\text{Var}[\\text{N}]=(1-q) \\cdot \\text{E}[\\text{N}]<\\text{E}[\\text{N}]$."),
                        
                        hr(),
                        
                        h4("Zero-Modified Models"),
                        
                        HTML("<p>Other options exist beyond the distributions presented here. For instance, the Zero-inflated Poisson (ZI Poisson) distribution has become increasingly popular among statistical practitioners as a way to capture overdispersion in a random variable and/or as a way to better fit a model to a sample exhibiting an excess of 0-counts. It does so by setting $Pr(N=0) = p^M_0 = p_0 + \\epsilon$, and defining $Pr(N=k) = p_k^M = (\\frac{1-p_0^M}{1-p_0})p_k$ for $k\\in $ &#8469;.</p>"),
                        
                        HTML("<p>We can even create an Extended Truncated Negative Binomial (ETNB) model by modifying the Negative Binomial model in two ways: setting $p_0 = 0$ (i.e., truncating the distribution's support) and allowing the parameter $r$ to be negative.</p>"),
                        
                        HTML("<p>With that said, there are plenty of people (see <a href = 'https://statisticalhorizons.com/zero-inflated-models' target='_blank'> here</a>) who believe that the distributions presented in this app should suit almost all occassions, when fitting a distribution to a given discrete random variable."),
                        
                        hr(),
                        
                        h3("Goodness-of-Fit"),
                        
                        hr(),
                        
                        h4("Summary Statistics"),
                        
                        HTML("<p>The <i>Summary</i> panel  on the <i>Output</i> page contains a lot of additional information that can help determine which of the choices is best for a given scenario, including the $\\chi^2$ test statistic, Akaike's Information Criterion (AIC), and Bayesian Information Criterion (BIC); for each of these selection criteria, the distribtution that maximizes the value is best.</p>"),
                        
                        hr(),
                        
                        h4("Summary Plots"),
                        
                        HTML("<p>The <i>Summary</i> panel also displays several plots that compare each of the best-fit distributions to the empirical distribution. These plots can be a valuable tool in discerning not just which distribution is best, but also in identifying specific features of the data that the certain distributions fail to represent.</p>"),
                        
                        hr(),
                        
                        HTML("<p>Copyright &#169; 2021 by Joe Knittel</p>")
                        )
                   )
           )


# Define server logic to read selected file ----
server <- function(input, output, session) {

    hideTab(inputId = "distributions", target = "pois")
    hideTab(inputId = "distributions", target = "nb")
    hideTab(inputId = "distributions", target = "bin")
    hideTab(inputId = "distributions", target = "sum")
    
    
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
    
    gofPlot <- reactive({
        
        
    })
        
    observeEvent(input$button,{
        shinyjs::hide("loadtxt")
        showTab(inputId = "distributions", target = "pois")
        showTab(inputId = "distributions", target = "nb")
        showTab(inputId = "distributions", target = "bin")
        showTab(inputId = "distributions", target = "sum")
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

#    output$binProbSlider <- renderUI({
#        df <- getData()
#        sliderInput(inputId = "binProbSlider", label = "prob", min = 0, max = 1, 
#                    value = 0.5)
#    })
    
    output$binSummary <- renderPrint({
        df <- getData()
        fitbin <- fitdistrplus::fitdist(df[,input$column], dist = "binom", start = list(prob = 0.5),
                                        fix.arg = list(size = input$binSizeSlider))
        return(summary(fitbin))
    })
    
    output$binPlot <- renderPlot({
        df <- getData()
        fitbin <- fitdistrplus::fitdist(df[,input$column], dist = "binom", start = list(prob = 0.5),
                                        fix.arg = list(size = input$binSizeSlider))
        return(plot(fitbin))
    })
    
    output$summaryPlot <- renderPlot({
        df <- getData()
        fitp <- fitdistrplus::fitdist(df[,input$column], "pois")
        fitnb <- fitdistrplus::fitdist(df[,input$column], "nbinom")
        fitbin <- fitdistrplus::fitdist(df[,input$column], dist = "binom", start = list(prob = 0.5),
                                        fix.arg = list(size = input$binSizeSlider))
        par(mfrow = c(1, 4))
        plot.legend <- c("Pois", "NB", "B")
        denscomp(list(fitp, fitnb, fitbin), legendtext = plot.legend)
        qqcomp(list(fitp, fitnb, fitbin), legendtext = plot.legend)
        cdfcomp(list(fitp, fitnb, fitbin), legendtext = plot.legend)
        ppcomp(list(fitp, fitnb, fitbin), legendtext = plot.legend)
        
    #       return(qqcomp(list(fitp, fitnb, fitbin)))
    })

    output$summaryGoF <- renderPrint({
        df <- getData()
        fitp <- fitdistrplus::fitdist(df[,input$column], "pois")
        fitnb <- fitdistrplus::fitdist(df[,input$column], "nbinom")
        fitbin <- fitdistrplus::fitdist(df[,input$column], dist = "binom", start = list(prob = 0.5),
                                        fix.arg = list(size = input$binSizeSlider))
        return(gofstat(list(fitp,fitnb, fitbin),fitnames=c("Pois", "NB", "B")))
    })
    
}


# Create Shiny app ----
shinyApp(ui, server)