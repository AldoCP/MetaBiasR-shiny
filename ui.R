library(shiny)
library(plotly)

# Define UI for dataset viewer application
shinyUI(pageWithSidebar(

  # Application title
  headerPanel("MetaBias"),

if(FALSE){ #comment this section
  sidebarPanel(
    textInput("caption", "Caption:", "[Custom name]"),
    selectInput("dataset", "...Or choose an example dataset from metafor:", 
                choices = c("raudenbush1985", "bangertdrowns2004", "hackshaw1998", "konstantopoulos2011")),
    numericInput("obs", "Max. num. of observations displayed in table:", 10),
      sliderInput("Alpha", 
        label = "Alpha (Beta distribution)",
        min = 0, max = 5, value = 1, step=0.01),
      sliderInput("Beta", 
        label = "Beta (Beta distribution)",
        min = 0, max = 5, value = 1, step=0.01)
             )
},

  sidebarLayout(
    sidebarPanel(

      tags$hr(),
      p('Sample dataset loaded automatically:',
        a(href = 'raudenbush1985.csv', 'raudenbush1985.csv'),
        '\n', 'You can load your own data below.'),

      tags$hr(),
      fileInput('file1', 'Choose file to upload',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  'text/tab-separated-values',
                  'text/plain',
                  '.csv',
                  '.tsv'
                )
      ),
      tags$hr(),
      checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   '\t'),
      radioButtons('quote', 'Quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   ''),
      tags$hr(),
      p('If you want a sample .csv or .tsv file to upload,',
        'you can first download the sample file',
        a(href = 'raudenbush1985.csv', 'raudenbush1985.csv'),
        ', and then try uploading them.')#,

#      tags$hr(),
#      sliderInput("Alpha", 
#        label = "Alpha (Beta distribution)",
#        min = 0, max = 5, value = 1, step=0.01),
#      sliderInput("Beta", 
#        label = "Beta (Beta distribution)",
#        min = 0, max = 5, value = 1, step=0.01)

    ),


  # Show the caption, a summary of the dataset and an HTML table with
  # the requested number of observations
  mainPanel(

##    h3(textOutput("caption")), 
##    verbatimTextOutput("summary"), 
##    plotlyOutput("plot1"), 
###    plotOutput("plot1"), 
###    tabsetPanel(tabPanel("Plot1", plotlyOutput("plot1"))),
###    tabsetPanel(tabPanel("Plot1", plotOutput("plot1"))),
##    plotlyOutput("plot2"), 
##    tableOutput("view")

      tabsetPanel(
#        tabPanel("Caption", h3(textOutput("caption"))), 
        tabPanel("Bayes factor", verbatimTextOutput("summary")), 
        tabPanel("BF vs sigma prior", plotlyOutput("plot1")),
        tabPanel("Likelihood", plotlyOutput("plot2")),
#        tabPanel("BF vs sigma prior", plotOutput("plot1")),
#        tabPanel("Likelihood", plotOutput("plot2")),
        tabPanel("Data", tableOutput("view"))
                 )

  )
))
)

