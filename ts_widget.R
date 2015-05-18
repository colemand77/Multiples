tsExplore <- function(dataset, gmin = NULL, gmax = NULL){
  require(shiny)
  require(xts)
  require(ggvis)
  require(dplyr)
  require(dygraphs)
  require(data.table)
  
  shinyApp(
      ui = fluidPage(
        fluidRow(style = "padding-bottom: 10px;",
                 column(4,selectInput("tickers","Choose Series:",
                                      choices = names(dataset)[-1],
                                      selected = names(dataset)[1])) # code from user interface
                 ),
        fluidRow(
          column(9,
            dygraphOutput("chart")),
          column(3,
            plotOutput("bplot"))),
        
        fluidRow(
          ggvisOutput("h"))
        ),
      server = function(input, output){
        #code from server
        #create the raw data.frame
        #need some kind of helper information to say that
          #a) first column must be a date
          #b) must be passed in as data.frame
          #c) columns must be individual series; not "tidy" data
        raw <- dataset
        
        ###Add this back in!
        #raw$Date <- as.Date(dataset$Date, format = "%m/%d/%Y")
        #convert to time series
        if(is.data.table(raw)){
          TSraw <- as.xts(raw[, -1, with = FALSE], order.by = raw$Date)[-1]
        }
        else{
          TSraw <- as.xts(raw[-1], order.by = raw$Date)
        }
        
        useData <- reactive({TSraw[,input$tickers]})
        
        from <- reactive({if (!is.null(input$chart_date_window))
          strftime(input$chart_date_window[1], "%Y-%m-%d")      
        })
        
        to <- reactive({if (!is.null(input$chart_date_window))
          strftime(input$chart_date_window[2], "%Y-%m-%d")
        })
        
        dateRange <- reactive({paste(from(), to(), sep = "/")})
        
        
        #output$from <- renderText({from()})
        #output$to <- renderText({to()})
        #output$range <- renderText({dateRange()})
        
        useDataDF <- reactive({  
          d <- data.frame(coredata = useData()[dateRange()])
          names(d) <- "multiples"
          d <- d %>% filter(!is.na(multiples))
          return(d)
        })
        
        useData_Mult <- reactive({
          d <- data.frame(coredata(TSraw[dateRange()])) %>%
            gather(Ticker, Multiple, na.rm = TRUE)
          return(d)
        })
        
        output$chart <- renderDygraph({
          dygraph(useData()) %>%
            dyRangeSelector(heigh = 40) %>%
            dyAxis("y", label = "NTM EBITDA Multiple", valueRange = c(gmin,gmax)) %>%
            dyAxis("x", pixelsPerLabel = 45, drawGrid = TRUE) %>%
            dyRoller(rollPeriod = 1)
        })
        
        reactive({useDataDF() %>%
                    ggvis(~multiples, fill := "lightblue", stroke := "white") %>%
                    #     layer_densities()}) %>%
                    layer_histograms()}) %>%
          bind_shiny("h")
        
        output$bplot <- renderPlot({
          par(mar = c(3,5,0,0))
          f <- boxplot(useDataDF(), ylim = c(gmin,gmax))
          f
          text(1.4, f$stats[3], paste(format(f$stats[3], trim = TRUE, digits = 3)), col = "blue")
          text(.6, f$stats[2], paste(format(f$stats[2], trim = TRUE, digits = 3))) 
          text(.6, f$stats[4], paste(format(f$stats[4], trim = TRUE, digits = 3)))
          
        })
        
        
      },#end of the server function

      options = list(height = 1000)
      
      )#end of the shinyApp

}#end of the widget function