# Joerg heintz, October 12th 2015
# simple software recommendation system

library(shiny)
library(ggplot2)
library(DiagrammeR)
library(xlsx)

mysoft <<- read.xlsx("data/myrecdata.xlsx", 1, check.names=FALSE)
mypipe <<- read.xlsx("data/myrecdata.xlsx", 2, header = TRUE, check.names=FALSE)
row.names(mysoft) <- NULL
row.names(mypipe) <- NULL

    pipechoices <<- names(mypipe) # pipenames
pipestepchoices <<- as.character(mysoft[,2]) # steps of each pipeline
softperpite <<- as.character()
softchoices <<- as.character(mysoft[,3]) # complete software list


shinyUI(fluidPage(theme = "bootstrap_hero.css",
    titlePanel( h3("Simple Software Recommendation System for ChIP-seq", style = "color:#FFA500")),
        sidebarLayout(position = "left",
            sidebarPanel(width =2,
                 # chose standard pipeline
                 selectInput("selectpipe", "1) Standard Pipelines", choices = pipechoices),
                 # software details
                 uiOutput("pipelinestep"), # software per pipeline step,
                 # construct software workflow based on selected pipeline step (???)
                 selectInput("soft_category", "3) Software Assessment Categories",
                             c("functionality", "performance", "compatibility", "popularity")),
                 uiOutput("ui"),
                 uiOutput("pconst")
                 #uiOutput("scorecard_table")
            ),

            mainPanel( 
                
                column(4, wellPanel(offset = 0,
                    h4("Standard Pipeline"),
                    grVizOutput('standp', width = "100%", height = "400px" ))),
                column(4, wellPanel(offset = 0,
                    h4("Software Scorecard"),
                    h6("requirement fullfilment"),
                    h6("performance"),
                    h6("compatibility"),
                    h6("additional functions"),
                    h6("popularity metrics"),
                    h4("user decision")),
                       height = "400px" ),
                column(4, wellPanel(offset = 0,
                    h4("Software Workflow"),
                    grVizOutput('userp', width = "100%", height = "400px" ))),
                column(width = 12, offset = 0, wellPanel(
                    htmlOutput("tabletitle"),
                    tableOutput("view")))
               
            )
        )
))



