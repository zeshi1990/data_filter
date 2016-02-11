shinyUI(pageWithSidebar(
  
  headerPanel("Wireless-Sensor Networks Snow-Depth Data Cleaning Tool"),
  
  sidebarPanel(
    helpText("This app helps users in Roger & Steve's groups to clean the
             wireless-sensor networks data with semi-automatic guidance.
             Please follow the format as in the sample file that could 
             be downloaded from the link below. (You do not need to have
             all the fields shown as the sample file. But at least you have
             to include snow-depth data and please keep the header the same
             as in the sample."),
    a("Snow depth sample", href="https://www.dropbox.com/s/2y8faw9wrlim84b/real_test.csv?dl=1"),
    tags$hr(),
    fileInput('file1', 'Choose CSV File from local drive, adjusting parameters if necessary',
              accept=c('text/csv', 'text/comma-separated-values,text/plain')),
    
    checkboxGroupInput("variable", "Variable:",
                       c("Snow depth" = "snowdepth",
                         "Temperature" = "temperature",
                         "Relative humidity" = "relative_humidity")),
    checkboxInput('selectall', 'Select ALL', TRUE),
    h4("File upload options"),
    checkboxInput('header', 'Header', TRUE),
    radioButtons('sep', 'Separator',
                 c(Comma=',',
                   Semicolon=';',
                   Tab='\t'),
                 'Comma'),
    radioButtons('quote', 'Quote',
                 c(None='',
                   'Double Quote'='"',
                   'Single Quote'="'"),
                 'Double Quote'),
    tags$head(tags$style(type="text/css",
                         "label.radio { display: inline-block; margin:0 10 0 0;  }",
                         ".radio input[type=\"radio\"] { float: none; }"))
    
    ),
  mainPanel(
    tags$head(
      tags$style(HTML("
        /* Smaller font for preformatted text */
        pre, table.table {
          font-size: smaller;
        }

        body {
          min-height: 2000px;
        }

        .option-group {
          border: 1px solid #ccc;
          border-radius: 6px;
          padding: 0px 5px;
          margin: 5px -10px;
          background-color: #f5f5f5;
        }

        .option-header {
          color: #79d;
          text-transform: uppercase;
          margin-bottom: 5px;
        }
      "))
    ),
    tabsetPanel(
      tabPanel("Raw data",
               h4(textOutput("caption1")),
               checkboxInput(inputId = "pageable", label = "Pageable"),
               conditionalPanel("input.pageable==true",
                                numericInput(inputId = "pagesize",
                                             label = "Rows per page",value=100,min=1,max=25)),
               
               htmlOutput("raw"),
               value = 1),
      tabPanel("Time-series figure",
               h4(textOutput("caption2")),
               fluidRow(
                 column(width = 4,
                  div(class = "option-group",
                      div(class = "option-header", "Brush"),
                      radioButtons("brush_dir", "Direction(s)",
                                   c("x", "y", "xy"), inline = TRUE),
                      radioButtons("brush_policy", "Input rate policy",
                                   c("debounce", "throttle"), inline = TRUE),
                      sliderInput("brush_delay", "Delay", min=100, max=1000, value=200,
                                  step=100),
                      checkboxInput("brush_reset", "Reset on new image")
                    )
                ),
                column(width = 4,
                       div(class = "option-group",
                           div(class = "option-header", "Clean selected points"),
                           actionButton("ma", "Moving Average"),
                           downloadButton('downloadData', 'Download')))
               ),
               actionButton("reset", "Reset range"),
               uiOutput("plotui"),
               conditionalPanel(condition = "input.variable.indexOf('temperature') != -1", 
                                plotOutput("temp_plot", height = "200px")),
               conditionalPanel(condition = "input.variable.indexOf('relative_humidity') != -1",
                                plotOutput("rh_plot", height = "200px")),
               h4("Points selected by brushing, with brushedPoints():"),
               DT::dataTableOutput("plot_brushed_points"),
               htmlOutput("notes2"),
               value = 2),
      id="tabs1")
  ))
)