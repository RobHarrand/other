############################
#R code for Data Science Specialisation Capstone - ui.R
#Completed 19/04/16
#Created by RH
############################

library(shinythemes)

shinyUI(fluidPage(theme = shinytheme("cerulean"),
  titlePanel("Next Word Prediction Applet"),
  
  sidebarLayout(
    sidebarPanel(
      h2("Instructions"),
      p(strong("1) Enter a word or sentence and click PREDICT")),
      p(strong("2) Update the sentence using any of the 3 UPDATE buttons. Alternatively, type in some extra text. 
               The word-cloud may also offer some additional matches")),
      p(strong("3) Click PREDICT again")),
      p(strong("4) If the word isn't in the dictionary, choose between CHOOSE ALT. or KEEP AS IS")),
        
      
      h4("Prediction"),
      actionButton("Predict", label = strong("PREDICT")),
      actionButton("Update1", label = "Update 1"),
      actionButton("Update2", label = "Update 2"),
      actionButton("Update3", label = "Update 3"),
      h4("If word not found"),
      actionButton("Alt", label = "Chose alt."),
      actionButton("Keep", label = "Keep as is")
    
    ),
    
    mainPanel(
      textInput("Sentence", label = "Sentence", width = '100%'),
      h2("Predictions"),
      textOutput("text1"),
      textOutput("text2"),
      textOutput("text3"),
      h2("Word-cloud"),
      plotOutput("plot"),
      HTML('<footer><i>
              This Shiny App has been developed and submitted in accordance with the requirements for the Coursera Data
Science Capstone Project (Rob Harrand 2016)
           </i></footer>')
      )
  )
))


