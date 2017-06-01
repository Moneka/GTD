#############################
  #R Shiny for 3 Continents
#############################
  
library(shiny)
Attacks_N<- read.csv("N_America.csv")

n1 <- Attacks_N[,-1]
rownames(n1) <- Attacks_N[,1]
Attacks_S<- read.csv("S_America.csv")

s1 <- Attacks_S[,-1]
rownames(s1) <- Attacks_S[,1]

Attacks_E<- read.csv("Eastern_Europe.csv")

e1 <- Attacks_E[,-1]
rownames(e1) <- Attacks_E[,1]

ui <- fluidPage(    
  
  # Give the page a title
  titlePanel("Terrorist Attacks in Different region"),
  
  # Create a spot for the barplot
  
  tabsetPanel(type = "tabs", 
              tabPanel("North America",
                       fluidRow(
                         # Define the sidebar with one input
                         sidebarPanel(
                           selectInput("countryn1", "Country Name:", 
                                       choices=colnames(n1)),
                           helpText("Dataset from Global Terrorism Database.")
                         ),
                         verbatimTextOutput("Summary_n1"),
                         plotOutput("nPlot")
                       )),
              tabPanel("South America",
                       fluidRow(
                         # Define the sidebar with one input
                         sidebarPanel(
                           selectInput("countrys1", "Country Name:", 
                                       choices=colnames(s1)),
                           helpText("Dataset from Global Terrorism Database.")
                         ),
                         verbatimTextOutput("Summary_s1"),
                         plotOutput("sPlot")
                         
                         
                       )),
              tabPanel("Eastern Europe",
                       fluidRow(
                         # Define the sidebar with one input
                         sidebarPanel(
                           selectInput("countrye1", "Country Name:", 
                                       choices=colnames(e1)),
                           helpText("Dataset from Global Terrorism Database.")
                         ),
                         verbatimTextOutput("Summary_e1"),
                         plotOutput("ePlot")
                         
                         
                       ))
              
              
  )
)




server <- function(input, output) {
  
  # Fill in the spot we created for a plot
  output$nPlot <- renderPlot({
    
    # Render a barplot
    barplot(n1[,input$countryn1],
            col=cm.colors(45),
            names.arg=rownames(n1), 
            main=input$countryn1,
            ylab="Number of Terrorist Attacks",
            xlab="Year",las=3)
    
  })
  output$Summary_n1 <- renderPrint({
    summary(subset(n1,select=input$countryn1))
  })
  
  output$sPlot <- renderPlot({
    
    # Render a barplot
    barplot(s1[,input$countrys1],
            col=cm.colors(45),
            names.arg=rownames(s1), 
            main=input$countrys1,
            ylab="Number of Terrorist Attacks",
            xlab="Year",las=3)
    
  })
  output$Summary_e1 <- renderPrint({
    summary(subset(e1,select=input$countrye1))
  })
  
  output$ePlot <- renderPlot({
    
    # Render a barplot
    barplot(e1[,input$countrye1],
            col=cm.colors(45),
            names.arg=rownames(e1), 
            main=input$countrye1,
            ylab="Number of Terrorist Attacks",
            xlab="Year",las=3)
    
  })
  
}

shinyApp(ui = ui, server= server)
