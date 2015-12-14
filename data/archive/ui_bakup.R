library(shiny)
library(ggplot2)
mydata<- read.csv("data/software_recommend_db_test.csv")
shinyUI(fluidPage(
    titlePanel("simple software recommendation system"),
        sidebarLayout(position = "left",
            sidebarPanel(
                selectInput("dataset", label = h3("choose a dataset:", style ="color:blue"), choices = c("software_recommend_db_test", "cars")),
                selectInput("select", label = h3("procedures", style = "color:blue"), 
                            choices = list("immunoprecipitation" = 1, "methylation" = 2, "splicing" = 3), selected = 1),
                #checkboxGroupInput('fields', label = h3('select fields:', style = "color:blue"), choices = names(mydata), selected = names(mydata)),
                    conditionalPanel(
        
                        'input.dataset === "mydata"',
                        checkboxGroupInput('fields', label = h3('select fields:', style = "color:blue"), choices = names(mydata), selected = names(mydata))
                    )
            ),
            mainPanel(# Copy the line below to make a select box 
                tableOutput('table')
            )
        )
        )
)



