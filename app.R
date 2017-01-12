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
         checkboxInput("toggleEdits", "Edits auch anzeigen?"),
         checkboxInput("toggleSize", "Punkte nach Postlaenge skalieren?",
                       value=TRUE)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("timePlot")
      )
   )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
   
    data <- read_csv("allUserStatistics.csv") %>%
      gather(actiontype, naction, TotalPosts, Edits) %>%
      mutate(avglength = AvgLengthPerPost,
             totallength = LengthOfAllPosts,
             actiontype = factor(actiontype, levels=c("TotalPosts", "Edits")))
      
    
    output$timePlot <- renderPlot({
      # separate input string
      filternames <- input$usernames %>%
        str_split(",") %>%
        unlist() %>%
        str_trim()
      
      p <- data %>%
        # filter by username
        filter(UserName %in% filternames) %>%
        # filter out edits if not requested
        {if(input$toggleEdits) . else filter(., actiontype=="TotalPosts")} %>%
        ggplot(aes(x = Year,
                   y = naction,
                   color = UserName,
                   group = interaction(UserName, actiontype))) +
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
})

# Run the application 
shinyApp(ui = ui, server = server)

