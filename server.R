library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
library(googleVis)
library(reshape2)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

shinyServer(function(input, output, session) {
  
  variable.list <- c("Snow depth" = "snowdepth",
                     "Temperature" = "temperature",
                     "Relative humidity" = "relative_humidity")
  
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  # Put uploaded data into a reactive object
  original.data <- reactiveValues(info=NULL)
  
  observe({
    if(input$selectall == FALSE) {
      updateCheckboxGroupInput(session,"variable","Variable:",choices=variable.list)
    }
    else
    {
      updateCheckboxGroupInput(session,"variable","Variable:",choices=variable.list,selected=variable.list)
    }
  })
  
  observeEvent(input$sd_plot_dblclick, {
    brush <- input$sd_plot_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  observeEvent(input$reset, {
    ranges$x <- NULL
    ranges$y <- NULL
  })
  
  observeEvent(input$ma, {
    s <- input$plot_brushed_points_rows_selected
    if (length(s)) {
      start.item <- original.data$info[(as.numeric(s[1]) - 1), 
                                       "snowdepth"]
      end.item <- original.data$info[(as.numeric(s[length(s)]) + 1), 
                                     "snowdepth"]
      increment <- (end.item - start.item) / (length(s) + 1)
      original.data$info[s, "snowdepth"] <- start.item + 
        increment * c(1:length(s))
    }
  })
  
  Data <- reactive({
    # input$file1 will be NULL initially. After the user selects and uploads a 
    # file, it will be a data frame with 'name', 'size', 'type', and 'datapath' 
    # columns. The 'datapath' column will contain the local filenames where the 
    # data can be found.
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    df.raw <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    # calculate term and pupil averages
    if (is.null(input$variable) || !(input$variable %in% colnames(df.raw))) {
      return()
    } else {
      variable <- c("time", input$variable)
      info <- df.raw[, variable, drop=FALSE]
    }
    
    original.data$info <- info
    # create a list of data for use in rendering
    return(list(df.raw=df.raw, info=info))
  })
  
  # allows pageability and number of rows setting
  myOptions <- reactive({  
    list(
      page=ifelse(input$pageable==TRUE,'enable','disable'),
      pageSize=input$pagesize
    ) 
  } )
  
  output$raw <- renderGvis({
    if (is.null(input$file1)) { return() }
    gvisTable(Data()$df.raw,options=myOptions())         
  })
  
  output$plotui <- renderUI({
    plotOutput("sd_plot", height = "200px", 
               dblclick = dblclickOpts(
                 id = "sd_plot_dblclick"
               ),
               brush = brushOpts(
                 id = "sd_plot_brush",
                 delay = input$brush_delay,
                 delayType = input$brush_policy,
                 direction = input$brush_dir,
                 resetOnNew = input$brush_reset
               )
    )
  })
  
  output$sd_plot <- renderPlot({
    if (is.null(input$file1)) {
      return()
    }
    else if (!("snowdepth" %in% input$variable)) {
      return()
    }
    else {
      df <- original.data$info
      s <- input$plot_brushed_points_rows_selected
      field.length = length(colnames(df))
      if (length(s)) {
        temp.df <- df[s, ]
        ggplot(df, aes_string(x="time", y="snowdepth")) + 
          geom_line(linetype=2) +
          geom_point(size=2) +
          geom_point(aes_string(x="time", y="snowdepth"), data=temp.df, size=3, 
                     colour = "red") + 
          xlab("Time") + ylab("Snow depth, cm") + 
          coord_cartesian(xlim = ranges$x, ylim = ranges$y)
      } else {
        ggplot(df, aes_string(x="time", y="snowdepth")) + 
          geom_line(linetype=2) +
          geom_point(size=2) +
          xlab("Time") + ylab("Snow depth, cm") + 
          coord_cartesian(xlim = ranges$x, ylim = ranges$y)
      }
    }
  })
  
  output$temp_plot <- renderPlot({
    if (is.null(input$file1)) {
      return()
    }
    else if (!("temperature" %in% input$variable)) {
      return()
    }
    else {
      df <- original.data$info
      s <- input$plot_brushed_points_rows_selected
      field.length = length(colnames(df))
      if (length(s)) {
        temp.df <- df[s, ]
        ggplot(df, aes_string(x="time", y="temperature")) + 
          geom_line(linetype=2) +
          geom_point(size=2) +
          geom_point(aes_string(x="time", y="temperature"), data=temp.df, size=3, 
                     colour = "red") + 
          xlab("Time") + ylab("Temperature, deg C") + 
          coord_cartesian(xlim = ranges$x, ylim = ranges$y)
      } else {
        ggplot(df, aes_string(x="time", y="temperature")) + 
          geom_line(linetype=2) + 
          geom_point(size=2) +
          xlab("Time") + ylab("Temperature, deg C") +
          coord_cartesian(xlim = ranges$x, ylim = ranges$y)
      }
    }
  })
  
  output$rh_plot <- renderPlot({
    if (is.null(input$file1)) {
      return()
    }
    else if (!("relative_humidity" %in% input$variable)) {
      return()
    }
    else {
      df <- original.data$info
      s <- input$plot_brushed_points_rows_selected
      field.length = length(colnames(df))
      if (length(s)) {
        temp.df <- df[s, ]
        ggplot(df, aes_string(x="time", y="relative_humidity")) + 
          geom_line(linetype=2) +
          geom_point(size=2) +
          geom_point(aes_string(x="time", y="relative_humidity"), data=temp.df, size=3, 
                     colour = "red") + 
          xlab("Time") + ylab("Relative Humidity, %") + 
          coord_cartesian(xlim = ranges$x, ylim = ranges$y)
      } else {
        ggplot(df, aes_string(x="time", y="relative_humidity")) + 
          geom_line(linetype=2) + 
          geom_point(size=2) +
          xlab("Time") + ylab("Relative Humidity, %") +
          coord_cartesian(xlim = ranges$x, ylim = ranges$y)
      }
    }
  })
  
  output$caption1 <- renderText( {
    if (is.null(input$file1)) { return() }
    
    "This is the raw table output of the data you uploaded"
  })
  
  output$caption2 <- renderText( {
    if (is.null(input$file1)) { return() }
    "The time-series figure of snowdepth, temperature and relative humidity"
  })
  
  output$notes2 <- renderUI( {
    if (is.null(input$file1)) { return() }
    HTML("This is an interactive plot, you could crop on the region of the data that you want to clean, 
         and the algorithm to clean the data could be selected from the sidebar")
    
  })
  
  # Print out the points that being brushed
  output$plot_brushed_points <- DT::renderDataTable({
    dat <- original.data$info
    res <- brushedPoints(dat, input$sd_plot_brush)
    DT::datatable(res)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('output-', Sys.Date(), '.csv', sep='')
    },
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      # Write to a file specified by the 'file' argument
      write.csv(original.data$info, file, sep = ",",
                  row.names = FALSE)
    }
  )
})