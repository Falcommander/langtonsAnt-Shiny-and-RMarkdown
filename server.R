library(animation)
library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  langton_ant=function(n, num, iterationsMax) 
  {
    map = matrix(data = n, nrow = n, ncol = n)
    iteration = 1
    tab <- array(map, dim = c(n, n, iterationsMax))
    if(num == 1)
    {
      p = floor(c(n/2, n/2))
      d = sample(1:4, 1)
      
      while((p[1] > 0 & p[1] <= n 
            & p[2] > 0 & p[2] <= n) & iteration < iterationsMax) {
          
        tab[1:n,1:n,iteration] = map
          
          if(map[p[1], p[2]] == 1)
          {
            map[p[1], p[2]] = 0
            p = p + switch(d, c(0, 1), c(-1, 0), c(0, -1), c(1, 0))
            d = ifelse(d == 4, 1, d + 1)
          }else{
            map[p[1], p[2]] = 1
            p = p + switch(d, c(0, -1), c(1, 0), c(0, 1), c(-1, 0))
            d = ifelse(d == 1, 4, d - 1)
         }
        iteration = iteration + 1
      }
    }else if(num == 2)
    {
      p1 = floor(c(n/2, n/2))
      d1 = sample(1:4, 1)
      p2 =  floor(c((n/3), (n/3)))
      d2 = sample(1:4, 1)
      
      while(((p1[1] > 0 & p1[1] <= n 
            & p1[2] > 0 & p1[2] <= n) 
            & 
            (p2[1] > 0 & p2[1] <= n 
            & p2[2] > 0 & p2[2] <= n)) & iteration < iterationsMax) {
        
        tab[1:n,1:n,iteration] = map
       
          if(map[p1[1], p1[2]] == 1)
          {
            map[p1[1], p1[2]] = 0
            p1 = p1 + switch(d1, c(0, 1), c(-1, 0), c(0, -1), c(1, 0))
            d1 = ifelse(d1 == 4, 1, d1 + 1)
          }else{
            map[p1[1], p1[2]] = 1
            p1 = p1 + switch(d1, c(0, -1), c(1, 0), c(0, 1), c(-1, 0))
            d1 = ifelse(d1 == 1, 4, d1 - 1)
          }
        
          if(map[p2[1], p2[2]] == 1)
          {
            map[p2[1], p2[2]] = 0
            p2 = p2 + switch(d2, c(0, -1), c(1, 0), c(0, 1), c(-1, 0))
            d2 = ifelse(d2 == 1, 4, d2 - 1)
          }else{
            map[p2[1], p2[2]] = 1
            p2 = p2 + switch(d2, c(0, 1), c(-1, 0), c(0, -1), c(1, 0))
            d2 = ifelse(d2 == 4, 1, d2 + 1)
          }
        iteration = iteration + 1
      }
      
    }
    return(tab)
  }

  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically
  #     re-executed when inputs change
  #  2) Its output type is a plot
  
    
    output$golPlot <- renderPlot({
      
      array = new.array()
      print(range(array))
      par(mar=c(0,0,0,0))
      plot(0,0,type="n",xlim=c(0,1),ylim=c(0,1),frame=F,xlab="",ylab="",xaxt="n",yaxt="n",xaxs="i",yaxs="i")
      image(array[,,input$N_i], xaxt = "n", yaxt = "n", bty = "n", col=grey(c(0,1)))
      
    })
    
    new.array <- reactive({
      input$Refresh
      langton_ant(isolate(input$x), isolate(input$size), 11000)
    })
    
    
    
    
    
    output$report <- downloadHandler( #genere le fichier Markdown
      filename = "LangtonsAnt.docx",
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "langtonsAnt.Rmd")
        file.copy("langtonsAnt.Rmd", tempReport, overwrite = TRUE)
        # Set up parameters to pass to Rmd document
        params <- list(n=input$x,
                       size=input$size,  
                       iterations=input$N_i)
        
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )

})


