# Joerg heintz, October 12th 2015
# simple software recommendation system


PipeChoices <<- c("pipeline: peak calling", "pipeline: differential binding sites", "pipeline: motif analysis", "pipeline: peak annotation", "pipeline: gene ontology analysis")


library(shiny)
library(ggplot2)
library(DiagrammeR)

shinyUI(fluidPage(theme = "bootstrap.css",
    titlePanel( h2("simple software recommendation system", style = "color:#FFA500")),
        sidebarLayout(position = "left",
        sidebarPanel(
                
             selectInput("dataset", "choose a pipeline:", 
                        choices = PipeChoices),

               
             "pipeline: peak calling" = peakcalling,
             "pipeline: differential binding sites" = bindingsites,
             "pipeline: motif analysis" = motifanalysis,
             "pipeline: peak annotation" = peakannotation,
             "pipeline: gene ontology analysis" = geneontologyanalysis)
                
                

                   
                 
                
                
                
            ),
            mainPanel(
                # Use imageOutput to place the image on the page
                #imageOutput("myImage"),
                #imageOutput("pipeline"),
                
                
               #div(dataTableOutput("mytable1"), style = "font-size:80%")
               
               
            )
        )
)
)



