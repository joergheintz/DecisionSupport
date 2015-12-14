library(shiny)
library(ggplot2)
library(xlsx)
library(DiagrammeR)
library(htmlwidgets)

# basic data
newnode <<- character()
pipelinestep <<- character()
userpipe <<- create_graph()
n<<- 0
b<<- NULL
grVizOutput('userp', width = "200px", height = "400px" )# pipelines


### create standard pipeline
StandPipelines <- function(standnodes){
    pipe_nodes <<- na.omit(mypipe[,standnodes])
    # prepare edges - detecting branchings
        myfrom <- as.character(pipe_nodes[1:(length(pipe_nodes)-1)])
        myto <- as.character(pipe_nodes[2:(length(pipe_nodes))])
        myedge <- as.data.frame(cbind(myfrom, myto))
        myedge <- myedge[(myedge$myfrom != 1) & (myedge$myto != 1), ]
    
    # checking filter-out branching information 
        pipe_nodes <<- na.omit(mypipe[,standnodes])
        pipe_nodes <<- pipe_nodes[pipe_nodes != 1]
    nodes_pipeline <- create_nodes(
        nodes = pipe_nodes,
        lable = FALSE, type = "lower", color = "white", fontcolor = "white", fontsize = "12", style = "filled", 
        penwidth ="1", fillcolor = "none", shape = "rectangle")
    edge_pipeline <- create_edges(
        from = myedge[,"myfrom"],
        to = myedge[,"myto"],
        relationship = "requires", 
        color = "white", 
        data = c("1", "2", "3", "4"))  
    StandardPipe <<- create_graph(
        nodes_df = nodes_pipeline,
        edges_df = edge_pipeline,
        graph_attrs = c("bgcolor = none"),
        node_attrs = c("fontname = Helvetica", "fillcolor = none"),
        edge_attrs = c("arrowsize = 1")) 
    reC <<- render_graph(StandardPipe)
}

###################################################################################################################################

userpipe <<- create_graph(
    graph_attrs = c("bgcolor = none"),
    node_attrs = c("fontcolor = white", "color = white", "fontsize = 11", "shape = ellipse", "width = 3"),
    edge_attrs = c("arrowsize = 1", "color = white"))
   
softconstr <<- function(newnode){
  
## add node
    print(paste("newnode: ", newnode))
    userpipe <<- add_node(
        graph = userpipe, node = newnode, label = paste(newnode, " peak calling"))
        print(node_info(userpipe))
        
## ad edge
ncount <<- node_count(userpipe, type = FALSE)
    print(paste("edgeinfo: ", edge_info(userpipe), "   ncount: ", ncount, "   n: ", n))
    if (ncount > 1) {
        mynodes <<- node_info(userpipe)
        userpipe <<- add_edges(userpipe,
                    from = as.character(mynodes[n-1, 1]),
                    to = as.character(mynodes[n,1])
                                )
        n <- ncount
        print(paste("n: ", n, "if: # nodes: ", ncount,  "   from: ", mynodes[ncount-1,1], "to ", mynodes[ncount,1]))
    }
                        
 b<<- render_graph(userpipe)
            }

#########################################################################################################################
shinyServer(function(input, output, session) {
    options(shiny.trace = TRUE)
    
    # standard pipelines: user selects one of determined pipelines
    observeEvent(input$selectpipe, {
        StandPipelines(callStandardPipe())
        output$standp <- renderGrViz({reC})
        pipestepchoices <<- pipe_nodes
        output$pipelinestep <<- renderUI({selectInput("pipelinestep", "select pipeline step :", choices = as.character(pipe_nodes))})
        print(paste("pipestepchoices ", pipestepchoices))
        
        })
    
    callStandardPipe <<- reactive({
        switch(input$selectpipe,
               "peak calling" = "peak calling",
               "differential binding" = "differential binding",
               "motif analysis" = "motif analysis",
               "peak annotation" = "peak annotation",
               "gene ontology analysis" = "gene ontology analysis")})
    
    # software per pipeline step -> subsetting software based on pipeline step & output on data table
    pipelinestepsoftInput <<- reactive({
        mypipefilter <- input$pipelinestep
        softperpipe <<- mysoft[mysoft$goal==mypipefilter ,c(1,3,5:7), drop = FALSE]
        ## provides software choices related to the pipeline step
        output$pconst <<- renderUI({selectizeInput(
            'pconst', 'construct software workflow:', choices = as.character(mysoft[mysoft$goal==mypipefilter, 3]),
            multiple = TRUE, options = list(maxItems = 1))})
        ## input for outputDataTable
        softperpipe 
        })  
    
    # construct software pipeline step by step  
    observeEvent(input$pconst, {print(input$pconst)
                                newnode <- as.character(input$pconst)
                                n<<-n+1
                                softconstr(newnode) # call funcetion add node
                                output$userp <- renderGrViz({b})})
    
####################################################################### output to ui
    output$ui <- renderUI({
        if (is.null(input$input_type))
            return()
        switch(input$input_type,
               "text" = textInput("dynamic", "Dynamic",
                                  value = "starting value"),
               "numeric" =  numericInput("dynamic", "Dynamic",
                                         value = 12),
               "selectInput" = selectInput("dynamic", "Dynamic",
                                           choices = c("Option 1" = "option1",
                                                       "Option 2" = "option2"),
                                           selected = "option2")
               )
    })
    
    output$input_type_text <- renderText({
        input$input_type
    })
    output$dynamic_value <- renderPrint({
        str(input$dynamic)
    })
    #############################################################################
    
    # software table 
    output$view <- renderDataTable( options = list(searching = TRUE, pageLength=10, paging = FALSE, filter = FALSE),{pipelinestepsoftInput()})


})


