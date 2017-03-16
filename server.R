

library(shiny)
library(metafor)
#library(MetaBiasR)
library(ggplot2)
library(plotly)
library(scales)

source("packages.R")
source("functions.R")

# Define server logic required to summarize and view the selected
# dataset
shinyServer(function(input, output) {

  # By declaring datasetInput as a reactive expression we ensure 
  # that:
  #
  #  1) It is only called when the inputs it depends on changes
  #  2) The computation and result are shared by all the callers 
  #	  (it only executes a single time)
  #
if(FALSE){#comment block
  datasetInput <- reactive({
    switch(input$dataset,
           "bangertdrowns2004" = get(data(dat.bangertdrowns2004)),
           "hackshaw1998" = get(data(dat.hackshaw1998)),
           "konstantopoulos2011" = get(data(dat.konstantopoulos2011)),
           "raudenbush1985" = get(data(dat.raudenbush1985))
	)
  })
}


  datasetInput <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) return(get(data(dat.bangertdrowns2004)))
    data <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    data
  })


  
  # The output$caption is computed based on a reactive expression
  # that returns input$caption. When the user changes the
  # "caption" field:
  #
  #  1) This function is automatically called to recompute the 
  #     output 
  #  2) The new caption is pushed back to the browser for 
  #     re-display
  # 
  # Note that because the data-oriented reactive expressions
  # below don't depend on input$caption, those expressions are
  # NOT called when input$caption changes.
  output$caption <- renderText({
    input$caption
  })
  
  # The output$summary depends on the datasetInput reactive
  # expression, so will be re-executed whenever datasetInput is
  # invalidated
  # (i.e. whenever the input$dataset changes)
  output$summary <- renderPrint({
    dataset <- datasetInput()
#    ans <- BFbias(dataset$yi, sqrt(dataset$vi), alpha=input$Alpha, beta=input$Beta); names(ans) <- "Bayes Factor"
    ans <- BFbias(dataset$yi, sqrt(dataset$vi)); names(ans) <- "Bayes Factor"
    ans
  })

  output$plot1 <- renderPlotly({
    dataset <- datasetInput()
#    p <- ggplotly(plotBFbias(y=dataset$yi, sig=sqrt(dataset$vi), alpha=input$Alpha, beta=input$Beta))
    p <- ggplotly(plotBFbias(y=dataset$yi, sig=sqrt(dataset$vi)))
#    print(p)  
	})


if(FALSE){
  output$plot1 <- renderPlot({
    dataset <- datasetInput()
    p <- plotBFbias(dataset$yi, sqrt(dataset$vi))
	print(p)  })
}
  
  # The output$view depends on both the databaseInput reactive
  # expression and input$obs, so will be re-executed whenever
  # input$dataset or input$obs is changed. 

if(FALSE){
  output$plot2 <- renderPlotly({
    dataset <- datasetInput()
    pl <- ggplotly(plotL(y=dataset$yi, sig=sqrt(dataset$vi), alpha=input$Alpha, beta=input$Beta))
#    pl  
	})
}

  output$plot2 <- renderPlotly({
    dataset <- datasetInput()
#    pl <- (plotL(y=dataset$yi, sig=sqrt(dataset$vi), alpha=input$Alpha, beta=input$Beta))
    pl <- ggplotly(plotL(y=dataset$yi, sig=sqrt(dataset$vi)))
#    print(pl)  
#    pl
	})

  output$plot3 <- renderPlot({
    dataset <- datasetInput()
	TWOmassPlot(y=dataset$yi, sig=sqrt(dataset$vi))
	})



  output$view <- renderTable({
    datasetInput()#head(datasetInput(), n = 5)#input$obs)
  })
})


