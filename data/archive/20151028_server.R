library(shiny)
library(ggplot2)
library(xlsx)
library(DiagrammeR)
library(htmlwidgets)

# basic data
newnode <<- character()
ncount <<- 0
pipelinestep <<- character()
mypipefilter <<- character()
userpipe <<- create_graph()
n <<- 0
b <<- NULL
grVizOutput('userp', width = "200px", height = "400px" )# pipelines


### create standard pipeline
StandPipelines<- function(standnodes, nodecolor="#B45F04 "){
    pipe_nodes <<- na.omit(mypipe[,standnodes])
    # prepare edges - detecting branchings
        myfrom <- as.character(pipe_nodes[1:(length(pipe_nodes)-1)])
        myto <- as.character(pipe_nodes[2:(length(pipe_nodes))])
        myedge <- as.data.frame(cbind(myfrom, myto))
        myedge <- myedge[(myedge$myfrom != 1) & (myedge$myto != 1), ]
    # checking filter-out branching information 
        pipe_nodes <<- na.omit(mypipe[,standnodes])
        pipe_nodes <<- pipe_nodes[pipe_nodes != 1]
    nodes_pipeline <<- create_nodes(
        nodes = pipe_nodes,
        lable = FALSE, type = "lower", color = "white", fontcolor = "white", fontsize = "12", style = "filled", 
        penwidth ="1", fillcolor = "none", shape = "rectangle")
        # coloring the selected pipeline step
        if (!is.null(mypipefilter)) {
        nodes_pipeline[nodes_pipeline$nodes == mypipefilter, "fillcolor"] <<- nodecolor 
        }
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
        node_attrs = c("fontname = Helvetica"),
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
        graph = userpipe, node = newnode, label = paste(newnode, ncount))
        print(node_info(userpipe))
        
## ad edge
ncount <<- node_count(userpipe, type = FALSE)
    print(paste("edgeinfo: ", edge_info(userpipe), "   ncount: ", ncount, "   n: ", n))
    if (ncount > 1) {
        mynodes <<- node_info(userpipe)
        userpipe <<- add_edges(userpipe,
                    from = as.character(mynodes[n-1, 1]),
                    to = as.character(mynodes[n,1]))
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
        pipestepchoices <<- pipe_nodes
        output$pipelinestep <<- renderUI({selectInput("pipelinestep", "2) Pipeline Steps", choices = as.character(pipe_nodes))})
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
            mypipefilter <<- input$pipelinestep  # the pipeline steps are directly read from the graph see line 93
            mysubcol <<- as.logical(sapply(mysoft[mysoft$goal == input$soft_category,], function(x) grepl("^1$", x)))
            softperpipe <<- mysoft[mysoft$goal==mypipefilter ,mysubcol, drop = FALSE]
            print(paste("mypipefilter  ------>", mypipefilter))
            ## colors the selected pipeline step node
            StandPipelines(callStandardPipe())
            output$standp <<- renderGrViz({reC})
            ## provides software choices related to the pipeline step
            output$pconst <<- renderUI({selectizeInput(
                'pconst', '5) Software', choices = as.character(mysoft[mysoft$goal==mypipefilter, 3]),
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
    
################################ output to ui  #######################################
    output$ui <- renderUI({
        if (is.null(input$soft_category))
            return()
        switch(input$soft_category,
               "functionality" = selectInput("functionality per mechanism", "4) Software Assessment Subcategories",
                                            choices = c("alignment" = "option1",
                                                         "mapping" = "option2",
                                                         "assembling" = "option3")),
               "performance" =  selectInput("dynamic", "4) to be defined",
                                            choices = c("x" = "option1",
                                                        "y" = "option2")),
               "compatibility" = selectInput("dynamic", "4) System & Data",
                                           choices = c("4) System" = "option1",
                                                       "Data" = "option2"),
                                           selected = "option2"),
               "popularity" = selectInput("dynamic", "4) Citations & Search Hits",
                                            choices = c("hits pubmed central" = "option1",
                                                         "paper citations" = "option2"),
                                            selected = "option2")
               )
    })
    scoreInput <<- reactive({
        input$soft_category
        print("functionality") 
        softperpipe <<- mysoft[ ,c(15:17), drop = FALSE]})
    output$input_type_text <- renderText({
        input$soft_category})
    output$dynamic_value <- renderPrint({
        str(input$dynamic)})
    
    #############################################################################
    
    # software table 
    output$view <- renderTable( 
        {pipelinestepsoftInput()
            },
        include.rownames=FALSE
        
        )
        
        
    output$tabletitle <- renderText({ 
        paste("Software for ", input$pipelinestep, "      [", input$soft_category, "]", '<br/>', '  ')
            
    })
    

})


