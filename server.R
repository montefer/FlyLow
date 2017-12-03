library(shiny)
#import functions
source("jetblue_plot_function.R")
source("jetblue_holiday.R")
source("jetblue_reg_season.R")
source("jetblue_weeks.R")
source("jetblue_ml.R")

ui<-fluidPage(navbarPage("FlyLow - Airfare Predictions",
              tabPanel("Predicted Airfare Prices",
                       column(4,
                              wellPanel(
                                textInput(inputId = "depart", label="Departing Airport"),
                                textInput(inputId = "arrival", label="Destination Airport"),
                                dateInput(inputId = "date", label = ("Date input")),
                                submitButton("Update Charts", icon("refresh"))
                              )
                       ),
                       
                       column(5, offset = 3,
                             tabPanel("Predicted Price vs. Date Range", imageOutput(outputId="plotPredict")))
                       ),
              tabPanel("Dataset graphs",
                fluidRow(
                column(4,
                       wellPanel(
                         textInput(inputId= "depart", label="Departing Airport"),
                         textInput(inputId= "arrival", label="Destination Airport"),
                         dateInput(inputId = "date", label = ("Date input")),
                         submitButton("Update Charts", icon("refresh"))
                       )
                ),
                
                column(4, offset=2,
                       tabsetPanel(
                         tabPanel("General Price vs. Date", imageOutput(outputId="plotDP")), 
                         tabPanel("Holiday Season Price vs. Date", imageOutput(outputId="plotH")),
                         tabPanel("Regular Season Price vs. Date", imageOutput(outputId="plotR")),
                         tabPanel("Weekday Price vs. Date", imageOutput(outputId="plotWd")),
                         tabPanel("Weekend Price vs. Date", imageOutput(outputId="plotWe"))
                       )
                )
              )
              ),
              tabPanel("Other",
                       fluidRow(
                         column(4,
                                wellPanel(
                                  textInput(inputId= "depart", label="Departing Airport"),
                                  textInput(inputId= "arrival", label="Destination Airport"),
                                  dateInput(inputId = "date", label = ("Date input")),
                                  submitButton("Update Charts", icon("refresh"))
                                )
                         ),
                         
                         column(4, offset=2,
                                tabsetPanel(
                                  tabPanel("General Price vs. Date", imageOutput(outputId="plotDP")), 
                                  tabPanel("Holiday Season Price vs. Date", imageOutput(outputId="plotH")),
                                  tabPanel("Regular Season Price vs. Date", imageOutput(outputId="plotR")),
                                  tabPanel("Weekday Price vs. Date", imageOutput(outputId="plotWd")),
                                  tabPanel("Weekend Price vs. Date", imageOutput(outputId="plotWe"))
                                )
                         )
                       )
              )
)
)


server<-function(input, output){
  observe(gplot<-generate_plot(input$depart, input$arrival))
  observe(ghplot<-generate_holiday_plot(input$depart, input$arrival))
  observe(grplot<-generate_regular_season_plot(input$depart, input$arrival))
  observe(gwplot<-generate_weeks_plots(input$depart, input$arrival))
  #observe(generate_predicted_plot(input$depart, input$arrival, input$date))
  
  
  
                       ####################Render plot images########################
  output$plotDP<-renderImage({
    filename<-paste("Flights_To_From_Plots/",input$depart," to ",input$arrival,".png",sep="")
    # Return a list containing the filename
    list(src = filename, width = 700, height = 700)
  }, deleteFile = FALSE)
  
  output$plotH<-renderImage({
    filename<-paste("Holiday_Flights_To_From_Plots/",input$depart," to ",input$arrival,".png",sep="")
    # Return a list containing the filename
    list(src = filename, width = 700, height = 700)
  }, deleteFile = FALSE)
  
  output$plotR<-renderImage({
    filename<-paste("Regular_Season_Flights_To_From_Plots/",input$depart," to ",input$arrival,".png",sep="")
    # Return a list containing the filename
    list(src = filename, width = 700, height = 700)
  }, deleteFile = FALSE)
  
  output$plotWd<-renderImage({
    filename<-paste("Weekday_Flights_To_From_Plots/",input$depart," to ",input$arrival,".png",sep="")
    # Return a list containing the filename
    list(src = filename, width = 700, height = 700)
  }, deleteFile = FALSE)
  
  output$plotWe<-renderImage({
    filename<-paste("Weekend_Flights_To_From_Plots/",input$depart," to ",input$arrival,".png",sep="")
    # Return a list containing the filename
    list(src = filename, width = 700, height = 700)
  }, deleteFile = FALSE)
  
  output$plotPredict<-renderImage({
    filename<-paste("Predicted_Flights_To_From_Plots/",input$depart," to ",input$arrival,".png",sep="")
    # Return a list containing the filename
    list(src = filename, width = 700, height = 700)
  }, deleteFile = FALSE)
  
}

shinyApp(ui = ui, server=server)

