

library(shiny)
library(metafor)
#library(MetaBiasR)
library(ggplot2)
library(plotly)
library(scales)

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
  datasetInput <- reactive({
    switch(input$dataset,
           "bangertdrowns2004" = get(data(dat.bangertdrowns2004)),
           "hackshaw1998" = get(data(dat.hackshaw1998)),
           "konstantopoulos2011" = get(data(dat.konstantopoulos2011)),
           "raudenbush1985" = get(data(dat.raudenbush1985))
	)
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
    ans <- BFbias(dataset$yi, sqrt(dataset$vi)); names(ans) <- "Bayes Factor"
    ans
  })

  output$plot1 <- renderPlotly({
    dataset <- datasetInput()
    p <- ggplotly(plotBFbias(dataset$yi, sqrt(dataset$vi)))
#    p  
	})

#  output$plot1 <- renderPlot({
#    dataset <- datasetInput()
#    p <- plotBFbias(dataset$yi, sqrt(dataset$vi))
#	print(p)  })

  
  # The output$view depends on both the databaseInput reactive
  # expression and input$obs, so will be re-executed whenever
  # input$dataset or input$obs is changed. 


  output$plot2 <- renderPlotly({
    dataset <- datasetInput()
    pl <- ggplotly(plotL(dataset$yi, sqrt(dataset$vi)))
#    pl  
	})


  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
})

