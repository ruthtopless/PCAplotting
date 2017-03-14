#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#


library(shiny)
library(shinythemes)
library(utils)
library(dplyr)
library(ggplot2)
library(car) #dataEllipse
library("ggrepel") # label points


PCAtestdata <- read.delim("PCAtestdata.txt", stringsAsFactors = F)
PCAlist <-c("PCA1", "PCA2", "PCA3", "PCA4", "PCA5", "PCA6", "PCA7", "PCA8", "PCA9", "PCA10")
ethnicspecific <- c("African", "East Asian", "Southeast Asian", "East or Southeast Asian", "South Asian", "European", "Hispanic", "Melanesian", "East Polynesian",  "West Polynesian", "Niuean", "Pukapukan", "Polynesian", "Unspecified")


##################################
# Define UI for application that draws a histogram
ui = fluidPage(

  theme = shinytheme("spacelab"),
  headerPanel('PCA clustering'),
  fluidRow(
    column(width = 4,
           selectInput('xcol', 'X Variable', PCAlist),
           selectInput('ycol', 'Y Variable', PCAlist, selected=PCAlist[2]),
           selectInput('ethnicclass', 'Choose ethnic groups to include', choices = ethnicspecific, multiple = TRUE ),
           textInput("highlight_subjects", "Type in subject ID to highlight on plot"),
           
           h4("Points near click"),
           tableOutput("click_info"),
           
           tableOutput("subjectdata")
    ), 

    
    column(width = 8,
           plotOutput('plot1', width = "80%", height = "900px",
                      click = clickOpts(id = "plot1_click"),
                      brush = brushOpts(id = "plot1_brush"),
                      hover = hoverOpts(id = "plot1_hover")
           )
           
           #downloadButton('downloadData', 'Download')
    )
  ),
  
  fluidRow(
    column(width = 12,
           h4("Brushed points"),
           dataTableOutput("brush_info")
    )
  ),
  
  
  fluidRow(
    column(width = 4,
           selectInput('xcol2', 'X Variable', PCAlist, selected=PCAlist[3]),
           selectInput('ycol2', 'Y Variable', PCAlist, selected=PCAlist[4]),
           h4("Points near click"),
           tableOutput("click_info2")
    ),
    column(width = 8,
           plotOutput('plot2', width = "80%", height = "900px",
                      click = clickOpts(id = "plot2_click"),
                      brush = brushOpts(id = "plot2_brush")
                      #hover = hoverOpts(id = "plot2_hover")
           )
    )
  ),
  
  fluidRow(
    column(width = 12,
           h4("Brushed points"),
           tableOutput("brush_info2")
    )
  )
)



####################################################################################################
server <- function(input, output, session) {
  
  #function to assign plotting colours to ethinicities
  colour_eth <- function(x) {
    unlist(lapply(x, function(x) {switch(x,
                                         African = "#999999",
                                         `East Asian`="#E41A1C",
                                         `Southeast Asian`="#377EB8",
                                         `South Asian`="#4DAF4A",
                                         European = "#984EA3",
                                         Hispanic= "#FF7F00",
                                         Melanesian="#FFFF33",
                                         `East Polynesian`="#A65628",
                                         `West Polynesian`="#F781BF",
                                         Niuean="#015249",
                                         Pukapukan="#76323F",
                                         Polynesian="#000080",
                                         Unspecified="#49274A"
    )
    }))
  }
  
  # define the colour scale to be used for plotting
  eth_col_scale <- scale_colour_manual(values=c("African" = "#F8766D",
                                                "East Asian"="#619CFF",
                                                "Southeast Asian"="#00B0F6",
                                                "East or Southeast Asian" = "#1565C0",
                                                "South Asian"="#E58700",
                                                "European" = "#00BA38",
                                                "Hispanic"= "#009966",
                                                "Melanesian"="#6C2DC7",
                                                "East Polynesian"="#E9B000",
                                                "West Polynesian"="#B983FF",
                                                "Niuean"="#FF67A4",
                                                "Pukapukan"="#E76BF3",
                                                "Polynesian"="#00C0AF",
                                                "Unspecified"="#999999"),
                                       name="Ethnicities"
  )
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    eth <- input$ethnicclass
    if(is.null(eth)) eth <- ethnicspecific  
    return(PCAtestdata %>% 
             select(SUBJECT, ETH_DESCRIP, which(input$xcol == names(.)), which(input$ycol == names(.)), PCAETHSPECIFIC, SPECCOLOUR) %>% 
             filter(PCAETHSPECIFIC %in% eth))
  })
  
  highlight_sub <- reactive({ 
    subj <-input$highlight_subjects
    return(PCAtestdata %>% select(SUBJECT, ETH_DESCRIP, which(input$xcol == names(.)), which(input$ycol == names(.)), PCAETHSPECIFIC) %>% filter(SUBJECT %in% subj))
  })
  
  output$subjectdata <- renderTable({ highlight_sub()})
  
  
  output$plot1 <- renderPlot({
    dat <- selectedData()%>% dplyr::rename_(xcol = input$xcol, ycol = input$ycol)
    highlight <- dat %>% filter(SUBJECT %in% input$highlight_subjects)
    dat   %>% 
      ggplot(., aes(x = xcol, y = ycol, colour = PCAETHSPECIFIC)) + 
      geom_point() + 
      eth_col_scale+ # the colour scale defined further up
      xlab(input$xcol) +
      ylab(input$ycol) + 
      ylim(c(min(PCAtestdata[, input$ycol]), max(PCAtestdata[, input$ycol]) )) + 
      xlim(c(min(PCAtestdata[, input$xcol]), max(PCAtestdata[, input$xcol]) )) +
      theme(legend.position='right',
            legend.text = element_text(size = rel(1.5)),
            legend.title = element_text(size = rel(2.0)))+
      geom_point(data = highlight, shape = 18, colour = 'black', size = 5) + 
      geom_text_repel(data = highlight,aes(x = xcol, y = ycol,  label = SUBJECT), colour="black",fontface = 'bold', nudge_x = 0.002, nudge_y = 0.002 )
    
  })
  
  
  
  output$click_info <- renderTable({nearPoints(PCAtestdata[,c("SUBJECT",input$xcol,input$ycol,"ETH_DESCRIP","PCAETHBROAD","PCAETHSPECIFIC")], input$plot1_click, xvar=input$xcol, yvar=input$ycol, addDist = TRUE)
  })
  
  
  plot1brushselected<- reactive({
    brushedPoints(PCAtestdata[,c("SUBJECT",input$xcol,input$ycol, "ETH_DESCRIP","PCAETHBROAD","PCAETHSPECIFIC")], input$plot1_brush, xvar=input$xcol, yvar=input$ycol)
  })
  
  output$brush_info <- renderDataTable({ plot1brushselected()},
   options = list(lengthMenu = list(c(10, 25, 50, -1), c('10','25','50','All')), pageLength = 10)
   #options = list(scrollY = 200px, scrollCollapse = TRUE, paging = FALSE) #for vertical scrolling
     )
  
  
  plot1brushselected2<- reactive({
    brushedPoints(PCAtestdata[,c("SUBJECT",input$xcol,input$ycol,input$xcol2,input$ycol2, "ETH_DESCRIP","PCAETHBROAD","PCAETHSPECIFIC")], input$plot1_brush, xvar=input$xcol, yvar=input$ycol)
  })
  
  output$plot2 <- renderPlot({
    dat2 <- plot1brushselected2()
    dat2 %>% dplyr::rename_(xcol = input$xcol2, ycol = input$ycol2) %>%  
      ggplot(., aes(x = xcol, y = ycol, colour = PCAETHSPECIFIC) ) + 
      geom_point() + 
      xlab(input$xcol2) + 
      ylab(input$ycol2) + 
      eth_col_scale +              
      ylim(c(min(PCAtestdata[, input$ycol2]), max(PCAtestdata[, input$ycol2]) )) + 
      xlim(c(min(PCAtestdata[, input$xcol2]), max(PCAtestdata[, input$xcol2]) ))
  })
  
  output$click_info2 <- renderTable({
    nearPoints(PCAtestdata[,c("SUBJECT",input$xcol2,input$ycol2,"PCAETHBROAD","PCAETHSPECIFIC")], input$plot2_click, xvar=input$xcol2, yvar=input$ycol2, addDist = FALSE)
  })
  
  output$brush_info2 <- renderDataTable({
    brushedPoints(PCAtestdata[,c("SUBJECT",input$xcol2,input$ycol2,"PCAETHBROAD","PCAETHSPECIFIC")], input$plot2_brush, xvar=input$xcol2, yvar=input$ycol2) 
      }) 
  
  
  
} #end of server

#output$downloadData <- downloadHandler(
# filename = function() { 
#  paste(input$dataset, '.csv', sep='') 
#},
#content = function(file) {
#  write.csv(datasetInput(), file)
#})





# Run the application 
shinyApp(ui = ui, server = server)

