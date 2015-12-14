# Joerg heintz, October 12th 2015
# simple software recommendation system

# Variables:
# mypipelinesoftInput dataframe, global, DT-datatable output 
# pipelinesoftInput() shiny dataframe, global, DT datatable output - has restrictions compared to R dataframe

library(shiny)
library(ggplot2)
library(XML)
library(xlsx)
library(XLConnect)
library(DiagrammeR)
library(htmlwidgets)
library(googleVis)
library(DT)
library(markdown)
library(scales)
library(scholar)
library(png)
library(grid)
library(plyr)
library(dplyr)

print(paste("Load myrecdata: ", timestamp()))
mysoft <<- read.xlsx("data/myrecdata.xlsx", "software", check.names=FALSE)
mypipe <<- read.xlsx("data/myrecdata.xlsx", "pipeline", header = TRUE, check.names=FALSE)


source("papercitationhistory.R")
source("benchmarking.R")


row.names(mysoft) <- NULL
row.names(mypipe) <- NULL
print(paste("End load myrecdata: ", timestamp()))

pipechoices <<- names(mypipe) # pipenames
softperpipe <<- NULL
softchoices <<- as.character(mysoft[,3]) # complete software list
softassesscat <<- "functional"
pipelinestep <<- "read mapping"

shinyUI(fluidPage(theme = "bootstrap.css", 
    tags$style(type='text/css', " .selectize-dropdown { font-size: 12px; line-height: 12px;}
    "),
    titlePanel( h3("Automated Software Decision Support System for ChIP-seq", style = "color:#FFA500")),
        
    fluidRow(
        column(3,
            wellPanel(h4("Simplified ChIP-seq Pipeline"),
                             grVizOutput('standp', height = "300px" )
                        )
        ),
        
        column(4,
                wellPanel(h4("Software Recommendation"),
                    DT::dataTableOutput("softrecommend", height = "300px")
                )
        ),
        
        column(5,
               wellPanel(h4('Software Decision Support'),
                   tabsetPanel(
                       tabPanel(h6('Software Comparison'),
                                DT::dataTableOutput("decTable")
                       ),
                       tabPanel(h6('Software Lineup'),
                                DT::dataTableOutput("benchdatatableout")
                       )
                   )
               )
        )
        
    ),
    
    fluidRow(
        column(3, wellPanel( 
            h4("Controls"),offset = 0,  height = "300px",
            # Standard Pipelines
            selectInput("selectpipe", "A) Pipeline", choices = pipechoices),
            # Pipeline Steps (Software choices)
            uiOutput("pipelinestep"),
            selectInput("soft_category", "C) Assessment Category",
                        c("functionality", "performance", "compatibility", "popularity"))
        )),
        column(6, wellPanel(
            h4(htmlOutput("tabletitle")),
            DT::dataTableOutput("softdatatableout")
        )),
        column(3, wellPanel(
            h4('Software Paper Citation History'), offset = 0, 
            plotOutput("papertrend", height = '300px')
        ))
        
                
    )
))


