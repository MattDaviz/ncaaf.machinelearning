library(shiny)

#initialize shiny interface
shinyUI(fluidPage(
  
  #App Title
  titlePanel("coachBrains: Brian Kelly, Notre Dame"),
  br(),
  sidebarLayout(
    
    #Sidebar panel
    #This has conditional panels within that are unique for each model
    sidebarPanel(
      sliderInput("Offense.Points", label = h5("Points Offensive Team:"), min=0, max=80, value= 0),
      sliderInput("Defense.Points", label = h5("Points Defensive Team:"), min=0, max=80, value= 0),
      selectInput("Down", label = h5("Down:"), choices = list("1st" = 1, "2nd" = 2, "3rd" = 3, "4th" = 4), selected = 1),
      sliderInput("Distance", label = h5("Yards to Go:"), min=1, max = 40, value = 10),
      sliderInput("Spot", label = h5("Yard Line (from opponent goal line):"), min=1,max=99, value = 75),
      selectInput("Period.Number", label = h5("Quarter:"), choices=list("1st"=1, "2nd"=2, "3rd"=3, "4th"=4, "OT"=4),selected =1),
      sliderInput("Clock", label = h5("Minutes Remaining in Quarter:"),  min=0,max=15, value = 15),
      sliderInput("Interceptions", label = h5("Offensive Team Total # Interceptions:"), min=0, max=6, value = 0),
      sliderInput("Fumbles", label = h5("Offensive Team Total # Fumbles:"), min=0, max=6, value = 0),
      sliderInput("Year", label = h5("Year:"),sep="", min=2005, max = 2015, value = 2015)
      
    ),
    
    
    #Main panel (where output is located)
    #Each tabPanel will have output for a particular model
    #First arg in tabPanel is the name, after is all that you want to display
    #br() is a break, or return to next line
    mainPanel(
      tabsetPanel(
        
        
          tabPanel("Random Forest", value = 1,
                 h1("Output:"),
                 br(),
                 h3(tableOutput("forestOutputs"))
        ),
        id = "conditionedPanels"
        )
    )
    
  )
)
)