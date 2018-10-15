
library(shiny)
library(dplyr)
load("Final.RData")

Found <- 'N'

css <- "
#large .selectize-input { line-height: 40px; }
#large .selectize-dropdown { line-height: 30px; }"

# ui
ui <- fluidPage(
   
  tags$head(tags$style(
    HTML('
         #item {
         color: #8b0000;
         font-size: 200%;
         }
         
         #others {
         color: gold;
         font-size: 200%;
         }
         
         tabPanel{ 
         font-family: "Calibri";
         color: gold;
         }
         
         body, label, input, button, select { 
         font-family: "Calibri";
         background: #000000 url("https://www.seriouseats.com/images/2016/01/20160115-things-never-to-but-at-supermarket-.jpg")  bottom left;
         background-position: center center;
         background-repeat: no-repeat;
         background-attachment: fixed;
         background-size: cover;
         }'))),
  
  #background: #000000 url("http://massyfigini.github.io/assets/css/images/DocStrange.jpg")  bottom left;
  
  titlePanel(HTML("<font size=10 color=#484848><b><center>Market Basket Analysis App</center></b></font><br/>"),windowTitle="Market Basket Analysis App"),
  
  sidebarLayout(
    sidebarPanel(tags$style(".well {background-color:#d3d3d3;}"),
                 tabsetPanel(
                   
                   # primo tab: input
                   tabPanel(HTML("<font color=black>Input</font>")
                            ,tags$style(type='text/css', css)
                            ,selectInput("item", "", Final$input)
                            ,actionButton("go","Add to cart", icon = icon("cart-arrow-down"))
                            ,HTML("<br/><br/><a href=http://www.massimilianofigini.com>Massimiliano Figini</a>")),
                   
                   # secondo tab: istruzioni
                   tabPanel(HTML("<font color=black>About the app</font>"),
                            HTML("<br/><b>Instructions</b><br/>
                                 In the input tab, you have to select one item, and the app will said you the most frequently bought with it.<br/>
                                 <br/><b>Data</b><br/>
                                 The data used for the algorithm are provided by Instancart, you can download the data and see terms and conditions 
                                 <a href=https://tech.instacart.com/3-million-instacart-orders-open-sourced-d40d29ead6f2>here</a>.
                                 <br/><br/><b>Algorithm</b><br/>
                                 The algorithm is a simple apriori model.
                                 You can find all the code on my
                                 <a href=https://github.com/massyfigini/NextWordApp>Github</a>
                                 <br/><br/><a href=http://www.massimilianofigini.com>Massimiliano Figini</a>")))
                 
                 ,width=4
                            ),
    
    mainPanel(
      HTML("<font size=5 color=#484848>Frequently bought with</font><br/>"),
      htmlOutput("item")
      #HTML("<br/><br/>"),
      #HTML("<font size=5 color=white>Frequently bought with</font><br/>"),
      #htmlOutput("others")
    )
    )
  
)

# server
server <- function(input, output) {

  observeEvent(input$go, {
    
    # se non scritto niente non fare nulla
    if(input$item=="")
    {
      
    } else {
    
    # altrimenti...
    a <- tolower(input$item)
    #a <- unlist(strsplit(a, " ", fixed=TRUE))
    
    # cerca nella tabella
      z <- a[1]
      Next <- Final %>% filter(input == a) %>% select(output1,output2,output3)
      if(nrow(Next) == 0) {
        # trovato nulla
        Found <- 'N'
      } else {
        # trovato!
        Found <- 'B'
        B1 <- Next[1]
        B2 <- Next[2]
        B3 <- Next[3]
      }
    
    output$item <- renderPrint({
      if(input$item == '') {print(HTML(""), row.names=FALSE)
      } else {
        if(Found == 'N') {
        } else {
          print(unname(B1), row.names=FALSE)
        }}
      
    })
    
    
    # output$others <- renderPrint({
    #   if(input$item == '') {print(HTML(""), row.names=FALSE)
    #   } else {
    #     if(Found == 'N') {
    #       HTML("<font size=5 color=red>Item not found!</font>")
    #     } else {
    #       print(unname(B2), row.names=FALSE)
    #       cat(",\n")
    #       print(unname(B3), row.names=FALSE)
    #     }}
    # })
    }
  })
  
}


shinyApp(ui = ui, server = server)
