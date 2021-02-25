library(shiny)
library(shinyjs)
library(fitdistrplus)

# defines the app's ui
ui <- navbarPage(id = "nav", title = "Discrete Random Variable Analysis", selected = "about", 

        # input page ui   
        tabPanel(value = "input", title = "Input",
              
            fluidPage(
                  
                # provides additional javascript functionality within the app
                useShinyjs(),
                       
                # sidebar of input page
                sidebarLayout(
                     
                    sidebarPanel(
                         
                        # upload file
                        fileInput("file1", "Upload CSV File:",
                        
                            multiple = FALSE,
                            accept = c("text/csv",
                                       "text/comma-separated-values,text/plain",
                                       ".csv")),
                         
                        uiOutput("dropdown"),
                         
                        uiOutput("hr1"),
                         
                        uiOutput("header"),
                         
                        uiOutput("sep"),
                         
                        uiOutput("hr2"),
                         
                        uiOutput("button")                      
                         
                     ),

                    mainPanel(
                    )
                )
           )
        ),
        
        # output page ui 
        tabPanel(value = "output", title = "Output",
                    
            fluidPage(
                
                # text to display if no data is loaded
                tags$div(class = "load", id = "loadtxt",
                                 tags$h4("Please load some data to model...")          
                ),
                
                # a set of tabs for the various distributions
                tabsetPanel(id = "distributions",
                    
                    tabPanel(value = "pois", title = "Poisson", br(),
                        
                        sidebarLayout(
                            
                            sidebarPanel(
                                
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
                                
                                uiOutput("binSizeSlider"),
                                
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
                            
                                verbatimTextOutput("summaryGoF"),
                                
                                hr(),
                                
                                selectInput("exportChoice", "Choose Distribution:", choices = c("Poisson", "Negative Binomial", "Binomial")),
                                
                                downloadButton("exportButton", label = "Export Model", style = "width:100%")
                            
                            ),
                            
                            mainPanel(
                            
                                plotOutput("summaryPlot")
                            )
                        )             
                    )
                )
            )
        ),
        
        # about page of ui
        tabPanel(value = "about", title = "About",
        
            fluidPage(
                        
                withMathJax(),

                tags$div(HTML("<script type='text/x-mathjax-config'>
                               MathJax.Hub.Config({
                               tex2jax: {inlineMath: [['$','$']]}
                               });</script>
                ")),
                        
                tags$style(HTML("hr {border-top: 1px solid #dddddd;}")),
                        

                p("This app was designed to facilitate the process of fitting a distribution to a discrete random variable."),
                        
                HTML("<p>To try it, just click on the <font style = 'background-color: #d4ebf2'>Input</font> page and upload a CSV file. 
                     Make sure to select the appropriate settings for the given file, then click the <font style = 'background-color: #f2dbd4'>Run Analysis</font> button at the bottom. 
                     This will take you to the <font style = 'background-color: #d4ebf2'>Output</font> page, where you can observe some distributions whose parameters are estimated using 
                     maximum likelihood estimation (MLE).</p>"),
                        
                h3("Distributions"),
                        
                hr(),
                        
                h4("Poisson Model"),
                        
                p("The simplest of the models presented is the Poisson model. This distribution is commonly used when modeling discrete random variables, but in its definition is an assumption 
                  that sometimes leads to a sub-optimal fit: $\\text{E}[\\text{N}] = \\text{Var}[\\text{N}]$."),
                        
                p("If the data show clear evidence of under- or overdispersion, then one of the other models is probably a better choice."),
                        
                hr(),
                        
                h4("Negative Binomial Model"),
                        
                p("If the variable exhibits overdispersion (i.e., $\\text{Var}[\\text{N}] > \\text{E}[\\text{N}]$), the Negative Binomial distribution is probably a good choice, since 
                  $\\text{E}[\\text{N}] = r\\beta$ and $\\text{Var}[\\text{N}]=r\\beta(1+\\beta) \\rightarrow \\text{Var}[\\text{N}]=\\text{E}[\\text{N}] \\cdot (1+\\beta) > \\text{E}[\\text{N}]$."),
                        
                p("Essentially, the Negative Binomial model is an extension of the Poisson model; it treats the random variable $\\text{N}$ as a mixture of Poisson and Gamma processes: 
                  $(\\text{N}|\\lambda) \\sim \\text{Pois}(\\lambda)$, where $\\lambda \\sim \\text{Gamma}(\\alpha, \\theta)$, resulting in an unconditional distribution $\\text{N} 
                  \\sim \\text{NB}(r=\\alpha, \\beta=\\theta)$."),
                        
                hr(),
                        
                h4("Binomial Model"),
                        
                p("If the variable exhibits clear underdispersion (i.e., $\\text{Var}[\\text{N}] < \\text{E}[\\text{N}]$), the Binomial distribution is probably a good choice. This is because, 
                  by definition, $\\text{E}[\\text{N}] = mq$ and $\\text{Var}[\\text{N}]=mq(1-q)$. Hence, $\\text{Var}[\\text{N}]=(1-q) \\cdot \\text{E}[\\text{N}]<\\text{E}[\\text{N}]$."),
                        
                hr(),
                        
                h4("Zero-Modified Models"),
                        
                HTML("<p>Other options exist beyond the distributions presented here. For instance, the Zero-inflated Poisson (ZI Poisson) distribution has become increasingly popular among statistical 
                     practitioners as a way to capture overdispersion in a random variable and/or as a way to better fit a model to a sample exhibiting an excess of 0-counts. It does so by setting 
                     $\\text{Pr}(\\text{N}=0) = p^M_0 > p_0$, and defining $\\text{Pr}(\\text{N}=k) = p_k^M = \\left(\\frac{1-p_0^M}{1-p_0}\\right) \\cdot p_k$ for $k\\in \\mathbb{N}$.</p>"),
                        
                HTML("<p>We can even create an Extended Truncated Negative Binomial (ETNB) model by modifying the Negative Binomial model in two ways: setting $p_0 = 0$ (i.e., truncating the distribution's 
                     support) and allowing the parameter $r$ to be negative.</p>"),
                        
                HTML("<p>With that said, there are plenty of people (see <a href = 'https://statisticalhorizons.com/zero-inflated-models' target='_blank'> here</a>) who believe that the distributions 
                     presented in this app should suit almost all occasions, when fitting a distribution to a given discrete random variable."),
                        
                hr(),
                        
                h3("Goodness-of-Fit"),
                        
                hr(),
                        
                h4("Summary Statistics"),
                        
                HTML("<p>The <font style = 'background-color: #d4f2db'>Summary</font> tab  on the <font style = 'background-color: #d4ebf2'>Output</font> page contains a lot of additional information that 
                     can help determine which of the choices is best for a given scenario, including the $\\chi^2$ test statistic, Akaike's Information Criterion (AIC), and Bayesian Information Criterion 
                     (BIC); for each of these selection criteria, the distribution that maximizes the statistic is best.</p>"),
                        
                hr(),
                        
                h4("Summary Plots"),
                        
                HTML("<p>The <font style = 'background-color: #d4f2db'>Summary</font> tab also displays several plots that compare each of the best-fit distributions to the empirical distribution. 
                     These plots can be a valuable tool in discerning not just which distribution is best, but also in identifying specific features of the data that the certain distributions fail to 
                     represent.</p>"),
                        
                hr(),
          
                h3("Model Usage"), 
          
                hr(),
          
                h4("Selecting Model"),
          
                hr(),
          
                HTML("<p>Once you've determined the best model based on your selection criteria of choice, choose your favorite model with the dropdown menu at the bottom of the 
                     <font style = 'background-color: #d4f2db'>Summary</font> tab.</p>"),
          
                HTML("<p>Then, click on the <font style = 'background-color: #f2dbd4'>Export Model</font> button right below it. This will store the model in a 
                     <font style = 'background-color:#dbd4f2'>.rds file</font> for later usage.</p>"),
 
                hr(),

                h4("Loading Model into R"),

                hr(),

                HTML("<p>After you've downloaded your model of choice, you can load it into R using the following code: <font style = 'background-color: #dbd4f2'>readRDS(path)</font>, 
                     where path is the path to your downloaded model file.</p>"),

                hr(),
          
                HTML("<p>Copyright &#169; 2021 by Joe Knittel</p>")
            )
        )
    )


# defines the app's server logic
server <- function(input, output, session) {
    
    # hide distribution tabs if no data has been uploaded
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
    
    # show distribution tabs once data has been loaded
    observeEvent(input$button,{
    
        shinyjs::hide("loadtxt")
        
        showTab(inputId = "distributions", target = "pois")
        
        showTab(inputId = "distributions", target = "nb")
        
        showTab(inputId = "distributions", target = "bin")
        
        showTab(inputId = "distributions", target = "sum")
        
        updateTabsetPanel(session, "distributions", selected = "pois")
        
        updateNavbarPage(session, "nav", selected = "output")
    })
    
    # radio buttons to select delimiter of input file
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
        
        tags$hr()
    })    
    
    # dropdown menu to select variable from input file    
    output$dropdown <- renderUI({
    
        df <- getData()

        selectInput("column", "Choose Discrete Variable:", choices = colnames(df))
    
    })

    # button to fit models to data
    output$button <- renderUI({
    
        req(input$file1)
        
        actionButton("button", "Run Analysis", style = "width:100%")
    
    })
    
    # summary of fitted poisson model
    output$poissonSummary <- renderPrint({
        df <- getData()
        fitp <- fitdistrplus::fitdist(df[,input$column], "pois")
        return (summary(fitp))
    })
    
    # plot of fitted poisson model 
    output$poissonPlot <- renderPlot({
        
        df <- getData()

        fitp <- fitdistrplus::fitdist(df[,input$column], "pois")
      
        return (plot(fitp))
    })
    
    # summary of fitted negative binomial model
    output$negbinSummary <- renderPrint({
    
        df <- getData()
    
        fitnb <- fitdistrplus::fitdist(df[,input$column], "nbinom")
        
        return(summary(fitnb))
    })
    
    # plot of fitted negative binomial model 
    output$negbinPlot <- renderPlot({
    
        df <- getData()
        
        fitnb <- fitdistrplus::fitdist(df[,input$column], "nbinom")

        return(plot(fitnb))
    })
    
    # slider for fixing binomial model's size parameter (required for fitting the distribution)
    output$binSizeSlider <- renderUI({
    
        df <- getData()
        
        sliderInput(inputId = "binSizeSlider", label = "m", 
                    
                    min = max(df[,input$column]), max = 4*max(df[,input$column]), 

                    value = 2*max(df[,input$column]))
    })

    # summary of fitted binomial model
    output$binSummary <- renderPrint({
    
        df <- getData()

        req(input$column)
        req(input$binSizeSlider)
        
        fitbin <- fitdistrplus::fitdist(df[,input$column], dist = "binom", start = list(prob = 0.5),
                                        fix.arg = list(size = input$binSizeSlider))
        return(summary(fitbin))
    })

    # plot of fitted binomial model 
    output$binPlot <- renderPlot({

        df <- getData()
        
        req(input$column)
        req(input$binSizeSlider)
        
        fitbin <- fitdistrplus::fitdist(df[,input$column], dist = "binom", start = list(prob = 0.5),
                                        fix.arg = list(size = input$binSizeSlider))
        return(plot(fitbin))
    })
    
    # generates an ensemble of summary plots 
    output$summaryPlot <- renderPlot({

        df <- getData()
        
        req(input$column)
        req(input$binSizeSlider)

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
        
    })

    # generates goodness-of-fit statistics for the collection of models
    output$summaryGoF <- renderPrint({

        df <- getData()
        
        fitp <- fitdistrplus::fitdist(df[,input$column], "pois")
        
        fitnb <- fitdistrplus::fitdist(df[,input$column], "nbinom")
        
        fitbin <- fitdistrplus::fitdist(df[,input$column], dist = "binom", start = list(prob = 0.5),
                                        fix.arg = list(size = input$binSizeSlider))

        return(gofstat(list(fitp,fitnb, fitbin),fitnames=c("Pois", "NB", "B")))
    })
    
    # export button to save selected model to .rds file
    output$exportButton <- downloadHandler(
        
        # default filename of the downloaded file
        filename = function() {
        
            choice <- input$exportChoice
            
            if (choice == "Poisson"){
            
                name = "Poisson"
            
            } else if (choice == "Negative Binomial") {
            
                name = "NB"
            
            } else {
            
                name = "B"
            
            }
             
            paste(name, "_Model", ".rds", sep="")

        },
        
        # downloads the selected model 
        content = function(file) {
        
            req(input$file1)
            
            df <- getData()
            
            choice <- input$exportChoice
             
            if(choice == "Poisson"){
            
                model <- fitdistrplus::fitdist(df[,input$column], "pois")
            
            } else if(choice == "Negative Binomial"){
                
                model <- fitdistrplus::fitdist(df[,input$column], "nbinom")
            
            } else {
            
                model <- fitbin <- fitdistrplus::fitdist(df[,input$column], dist = "binom", start = list(prob = 0.5),
                                                          fix.arg = list(size = input$binSizeSlider))
            }
             
            saveRDS(summary(model), file)
        }    
            
    )
       
}

# creates the shiny app
shinyApp(ui, server)