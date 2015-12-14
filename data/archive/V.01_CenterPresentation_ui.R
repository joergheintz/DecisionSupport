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
    titlePanel( h4("Automated Software Decision Support System for ChIP-seq", style = "color:#FFA500")),
        
    fluidRow(
        column(2, wellPanel( 
            h4("Controls"),offset = 0,  height = "300px",
                            # Standard Pipelines
                            selectInput("selectpipe", "1) Standard Pipelines", choices = pipechoices),
                            # Pipeline Steps (Software choices)
                            uiOutput("pipelinestep"),
                            selectInput("soft_category", "3) Assessment Categories",
                                        c("functionality", "performance", "compatibility", "popularity"))
        )),
        column(7,
            wellPanel(
                tags$div(class = "row",
                    h4("Simplified ChIP-seq Pipeline and Recommended Software"),
                        tags$div(class = "span1", style="display:inline-block",
                             grVizOutput('standp', height = "300px" )
                        ),
                    tags$div(class = "span1", style="display:inline-block",width = '10px'
                    ),
                        tags$div(class = "span1", style="display:inline-block",
                            DT::dataTableOutput("softrecommend", height = "300px")
                        )
                )
            )
        ),
        
        column(3, wellPanel(
            h4('Main Paper Citation History'), offset = 0, 
            plotOutput("papertrend", height = '300px')
        ))
    ),
    
    fluidRow(
        column(width = 7, wellPanel(
            h4(htmlOutput("tabletitle")),
            DT::dataTableOutput("softdatatableout")
        )),
        column(5, 
            wellPanel(
                tabsetPanel(
                    tabPanel(h6('Software Decision Support'),
                             DT::dataTableOutput("decTable")    
                    ),
                    tabPanel(h6('Software Lineup'),
                         DT::dataTableOutput("benchdatatableout")
                    )
                )
            )
        )
                
    )
))


