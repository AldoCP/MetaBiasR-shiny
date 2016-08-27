library(shiny)
library(plotly)

# Define UI for dataset viewer application
shinyUI(pageWithSidebar(

  # Application title
  headerPanel("Publication bias analysis"),

  # Sidebar with controls to provide a caption, select a dataset, and 
  # specify the number of observations to view. Note that changes made
  # to the caption in the textInput control are updated in the output
  # area immediately as you type
  sidebarPanel(
    textInput("caption", "Caption:", "[Custom name]"),

    selectInput("dataset", "Choose a metafor dataset:", 
                choices = c("raudenbush1985", "bangertdrowns2004", "hackshaw1998", "konstantopoulos2011")),

    numericInput("obs", "Max. num. of observations displayed in table:", 10)
  ),


  # Show the caption, a summary of the dataset and an HTML table with
  # the requested number of observations
  mainPanel(
    h3(textOutput("caption")), 

    verbatimTextOutput("summary"), 

    plotlyOutput("plot1"), 
#    plotOutput("plot1"), 

#    tabsetPanel(tabPanel("Plot1", plotlyOutput("plot1"))),
#    tabsetPanel(tabPanel("Plot1", plotOutput("plot1"))),

    plotlyOutput("plot2"), 


    tableOutput("view")
  )
))

