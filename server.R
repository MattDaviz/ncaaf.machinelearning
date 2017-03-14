shinyapps::setAccountInfo(name='coachbrains', token='xxx', secret='xxx')

#Required libraries
library(shiny)
library(randomForest)
library(glmnet)
library(splines)

##Load Forests:
load("Qtr1NotreDame.RData")
load("Qtr2NotreDame.RData")
load("Qtr3NotreDame.RData")
load("Qtr4winNotreDame.RData")
load("Qtr4loseNotreDame.RData")
load("Qtr4tieNotreDame.RData")

shinyServer(
  
  function(input, output){
    
    
    # Reactive expression to compose a data frame containing all of the input values from the forest page
    forestValues <- reactive({
      
      # Compose data frame
      data.frame(
        "Distance" = as.integer(input$Distance),
        "Down" = as.integer(input$Down),
        "Fumbles" = as.integer(input$Fumbles),
        "Interceptions" = as.integer(input$Interceptions),
        "Spot" = as.integer(input$Spot),
        "Clock" = as.integer(input$Clock),
        "Year" = as.integer(input$Year),
        "Offense.Points" = as.integer(input$Offense.Points),
        "Defense.Points" = as.integer(input$Defense.Points),
        "Period.Number" = as.integer(input$Period.Number),
        "DownXDistance" = as.integer(input$Down)*as.integer(input$Distance),
        "Point.Differential" = as.integer(input$Offense.Points) - as.integer(input$Defense.Points)
      )
    })
    
    
    ###Output the forest predictions
    forestOutputs <- reactive({
      if(input$Period.Number==1){
        data.frame(
          "Play Prediction" = predict(playFit.1rf, forestValues(), "response")
         # "Likelihood of Pass" = paste(round(((predict(playFit.1rf,forestValues(),
          #                                             "vote")[2]*501-	((predict(playFit.1rf,forestValues(),"vote")[2]*501-250)/3))/501)*100),"%",sep="")
        )
      }
      else if(input$Period.Number==2){
        data.frame(
          "Play Prediction" = predict(playFit.2rf, forestValues(), "response")
         # "Likelihood of Pass" = paste(round(((predict(playFit.2rf,forestValues(),
          #                                             "vote")[2]*501-	((predict(playFit.2rf,forestValues(),"vote")[2]*501-250)/3))/501)*100),"%",sep="")
        )
      }
      else if(input$Period.Number==3){
        data.frame(
          "Play Prediction" = predict(playFit.3rf, forestValues(), "response")
        #  "Likelihood of Pass" = paste(round(((predict(playFit.3rf,forestValues(),
         #                                              "vote")[2]*501-	((predict(playFit.3rf,forestValues(),"vote")[2]*501-250)/3))/501)*100),"%",sep="")
        )
      }
      else if(input$Period.Number==4&input$Offense.Points>input$Defense.Points){
        data.frame(
          "Play Prediction" = predict(playFit.4winrf, forestValues(), "response")
         # "Likelihood of Pass" = paste(round(((predict(playFit.4winrf,forestValues(),
          #                                             "vote")[2]*501-	((predict(playFit.4winrf,forestValues(),"vote")[2]*501-250)/3))/501)*100),"%",sep="")
        )
      }
      else if(input$Period.Number==4&input$Offense.Points<input$Defense.Points){
        data.frame(
          "Play Prediction" = predict(playFit.4loserf, forestValues(), "response")
         # "Likelihood of Pass" = paste(round(((predict(playFit.4loserf,forestValues(),
          #                                             "vote")[2]*501-	((predict(playFit.4loserf,forestValues(),"vote")[2]*501-250)/3))/501)*100),"%",sep="")
        )
      }
      else if(input$Period.Number==4&input$Offense.Points==input$Defense.Points){
        data.frame(
          "Play Prediction" = predict(playFit.4tierf, forestValues(), "response")
          #"Likelihood of Pass" = paste(round(((predict(playFit.4tierf,forestValues(),
           #                                            "vote")[2]*501-	((predict(playFit.4tierf,forestValues(),"vote")[2]*501-250)/3))/501)*100),"%",sep="")
        )
      }
      
    })
    
    output$forestOutputs <- renderTable({
      forestOutputs()}, include.rownames = F
    )
    
    
    
    
    
  }
  
  
  
  
  
  
  
)

