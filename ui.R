library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Langton's Ant"),
 
  # Sidebar with a slider input for the number of bins
  sidebarPanel(
     
################ sample size
    
      sliderInput("size", "Number of Ants:",
                  min = 1,
                  max = 2,
                  value = 1, step = 1),

# Sidebar with a slider input for the number of bins
      sliderInput("x", "size of the map :",
                  min = 1,
                  max = 1000,
                  value = 100),

# Sidebar with a slider input for the number of bins
sliderInput("N_i","Number of iterations :",
            min = 1,
            max = 11000,
            value = 1,
            step = 1,
            animate = animationOptions(interval=300, loop=FALSE, playButton=NULL, pauseButton=NULL)),
            actionButton("Refresh", "Rechargez"),
 ),




    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("golPlot",height='700px'),
      downloadButton("report", "Generate report") #pour avoir le bouton sur lequel on va cliquer
    )
  )




