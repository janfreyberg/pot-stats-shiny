#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(stringr)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("pOt User über die Jahre"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         textInput("usernames", "User Namen", value="Sharku, [Amateur]Cain",
                   placeholder="Mit kommas getrennt"),
         textInput("userids", "User IDs", value="7436",
                   placeholder="Mit kommas getrennt"),
         checkboxInput("toggleEdits", "Edits auch anzeigen?"),
         checkboxInput("toggleSize", "Punkte nach Postlaenge skalieren?",
                       value=TRUE)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("actionPlot"),
         plotOutput("lengthPlot")
      )
   )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
   
    data <- read_csv("allUserStatistics.csv") %>%
      gather(actiontype, naction, TotalPosts, Edits) %>%
      rename(avglength = AvgLengthPerPost,
             totallength = LengthOfAllPosts,
             Jahr = Year,
             Username = UserName) %>%
      mutate(actiontype = factor(actiontype, levels=c("TotalPosts", "Edits")),
             Jahr = factor(Jahr))

    
    output$actionPlot <- renderPlot({
      # separate input string
      filternames <- input$usernames %>%
        str_split(",") %>%
        unlist() %>%
        str_trim()
      filterids <- input$userids %>%
        str_split(",") %>%
        unlist() %>%
        str_trim()
      
      p <- data %>%
        # filter by username
        filter(Username %in% filternames | UserId %in% filterids) %>%
        # filter out edits if not requested
        {if(input$toggleEdits) . else filter(., actiontype=="TotalPosts")} %>%
        ggplot(aes(x = Jahr,
                   y = naction,
                   color = Username,
                   group = interaction(Username, actiontype))) +
        geom_line(aes(linetype = actiontype)) +
        guides(linetype=FALSE) +
        ylab("Anzahl an Aktionen")
      
      if(input$toggleSize){
        p <- p + geom_point(aes(size = avglength))
      }else{
        p <- p + geom_point()
      }
      
      p
    })
    
    output$lengthPlot <- renderPlot({
      # separate input string
      filternames <- input$usernames %>%
        str_split(",") %>%
        unlist() %>%
        str_trim()
      filterids <- input$userids %>%
        str_split(",") %>%
        unlist() %>%
        str_trim() %>%
        as.numeric()
      
      data %>%
        # filter by username
        filter(Username %in% filternames | UserId %in% filterids) %>%
        # filter out edits if not requested
        {if(input$toggleEdits) . else filter(., actiontype=="TotalPosts")} %>%
        ggplot(aes(x = Jahr,
                   y = totallength,
                   color = Username,
                   group = interaction(Username, actiontype))) +
        geom_line(aes(linetype = actiontype)) +
        geom_point() +
        guides(linetype=FALSE) +
        ylab("Zeichen in allen posts")
      
    })
})

# Run the application 
shinyApp(ui = ui, server = server)

