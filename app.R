#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#


library(shiny)
library(shinythemes)
library(utils)
library(dplyr)
library(ggplot2)

round.up <- function(x, decimals = 1){
  round(x + (5 * 10 ^ (-decimals - 1)), digits = decimals)
}

round.down <- function(x, decimals = 1){
  round(x - (5 * 10 ^ (-decimals - 1)), digits = decimals)
}


PCAtestdata <- read.delim("~/Desktop/PCAtestdata.txt", stringsAsFactors = F)
PCAlist <-c("PCA1", "PCA2", "PCA3", "PCA4", "PCA5", "PCA6", "PCA7", "PCA8", "PCA9", "PCA10")
ethnicspecific <- c("African", "East Asian", "Southeast Asian", "South Asian", "European", "Hispanic", "Melanesian", "East Polynesian",  "West Polynesian", "Niuean", "Pukapukan", "Polynesian", "Unspecified")


##################################
# Define UI for application that draws a histogram
ui = fluidPage(
    theme = shinytheme("spacelab"),
    headerPanel('PCA clustering'),
    fluidRow(
      column(width = 4,
      selectInput('xcol', 'X Variable', PCAlist),
      selectInput('ycol', 'Y Variable', PCAlist, selected=PCAlist[2]),
      selectInput('ethnicclass', 'Choose ethnic groups to include', choices = ethnicspecific, multiple = TRUE )
      #numericInput('clusters', 'Cluster count', 3, min = 1, max = 9)
        ), 
   
      column(width = 8,
      plotOutput('plot1', width = "80%", height = "900px",
                click = clickOpts(id = "plot1_click"),
                brush = brushOpts(id = "plot1_brush"),
                hover = hoverOpts(id = "plot1_hover")
                 )
      #tableOutput("reactivetable")
      #downloadButton('downloadData', 'Download')
        )
      ),
    
    fluidRow(
      column(width = 6,
             h4("Points near click"),
             verbatimTextOutput("click_info")
        ),
      column(width = 6,
             h4("Brushed points"),
             verbatimTextOutput("brush_info")
        )
    )
  )


####################################################################################################
server <- function(input, output, session) {
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    #return(PCAtestdata %>% select(input$xcol, input$ycol, PCAETHSPECIFIC) %>% filter(PCAETHSPECIFIC %in% input$ethnicclass))
    return(PCAtestdata[, c(input$xcol, input$ycol, "PCAETHSPECIFIC")])
  })
  
  #clusters <- reactive({filter(selectedData, PCAETHSPECIFIC %in% input$ethnicclass)})
  #dat$colPoints <- make.transparent(possiblecol[as.factor(PCAETHSPECIFIC$EllipsePlot)], transparency = 85)
  
    
  output$plot1 <- renderPlot({
    palette <-c("#999999", "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3","#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#015249", "#76323F", "#000080", "#49274A")
    #palette<-c("#E24E42", "#E9B000", "#EB6E80", "#008F95", "#94618E", "#49274A", "#4484CE", "#015249", "#F7882F", "#8FC33A", "#535353", "#B37D4E", "#76323F")
    dat <- selectedData()
    dat$colPoints <- palette[as.factor(dat$PCAETHSPECIFIC)]
    x.max <- round.up(max(dat[,1], na.rm = TRUE), decimals = 3)
    x.min <- round.down(min(dat[,1], na.rm = TRUE), decimals = 3)
    y.max <- round.up(max(dat[,2], na.rm = TRUE), decimals = 3)
    y.min <- round.down(min(dat[,2], na.rm = TRUE), decimals = 3)
   plot(x=(dat[,1]),y=(dat[,2]),
         col = dat$colPoints,
         xlim = c(x.min, x.max), ylim = c(y.min, y.max),
         xlab = input$xcol, ylab = input$ycol,
         pch = 20, cex = 2)
    dataEllipse(x=(dat[,1]), y=(dat[,2]), groups = factor(dat$PCAETHSPECIFIC), center.cex = 0, robust = TRUE, grid = FALSE, levels = 0.8, col = palette, add = TRUE, plot.points = FALSE, group.labels = NA)
    legend(x = "topright", legend = levels(factor(dat$PCAETHSPECIFIC)), col = palette, lwd = 6, bty = "n", cex = 1)
    #points(dat, pch = 3, cex = 1, lwd = 4)
    })
  
  #output$plot1 <- renderPlot({
  #  ggplot(mtcars2, aes(wt, mpg)) + geom_point()
  #})
 
  output$click_info <- renderPrint({
    nearPoints(PCAtestdata[,c("SUBJECT",input$xcol,input$ycol,"PCAETHBROAD","PCAETHSPECIFIC")], input$plot1_click, xvar=input$xcol, yvar=input$ycol, addDist = TRUE)
  })
  
  output$brush_info <- renderPrint({
    brushedPoints(PCAtestdata[,c("SUBJECT",input$xcol,input$ycol,"PCAETHBROAD","PCAETHSPECIFIC")], input$plot1_brush, xvar=input$xcol, yvar=input$ycol)
  })
}
  
  
  
  
  #output$downloadData <- downloadHandler(
   # filename = function() { 
    #  paste(input$dataset, '.csv', sep='') 
    #},
    #content = function(file) {
    #  write.csv(datasetInput(), file)
    #})
  
  
  


# Run the application 
shinyApp(ui = ui, server = server)

