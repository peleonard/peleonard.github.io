library(shiny)
library(data.table)
library(tidyverse)
library(RcppMovStat)

ui <- fluidPage(
  titlePanel("Visulization of movMeanUE in R package RcppMovStat"),
  sliderInput(inputId = "ww", label = "Window Width:",
              min = 1, max = 3e4 + 1, value = 1e4, step = 5e3),
  sliderInput(inputId = 'ss', label = "Step Size:", 
              min = 100, max = 500, value = 300, step = 50),
  plotOutput(outputId = "plot2")
) 

server <- function(input, output) {
  output$plot2 <- renderPlot({
    Stl.1.Sw2 <- fread("Stl_1_Sw2.csv")
    
    # Rolling Statistics for PM
    pm.rs <- data.table(movMeanUE(Stl.1.Sw2$PM, Stl.1.Sw2$pos - Stl.1.Sw2$pos[1] + 1, 
                                  input$ww, input$ss, na_rm = T, sizeD = T)) 
    setnames(pm.rs, c("pos", "PM", "PM.rm"))
    pm.rs[ ,":="(PM.rmed = movQtUE(Stl.1.Sw2$PM, Stl.1.Sw2$pos - Stl.1.Sw2$pos[1] + 1, 
                                   input$ww, prob = .5, input$ss, na_rm = T, sizeD = T)[,3],
                 pos = pos + Stl.1.Sw2$pos[1] - 1)]
    
    Co.AC2 <- melt(pm.rs, id.vars = c("pos", "PM"), measure.vars = c("PM.rm", "PM.rmed"),
                   variable.name = "rs")
    
    
    ggplot(Co.AC2, aes(x = pos)) + geom_point(aes(y = PM), size = .8) + 
      geom_line(aes(y = value, color = rs), size = .5) + 
      theme_gray() + theme(legend.position = "bottom")
  })
}
shinyApp(ui, server)








