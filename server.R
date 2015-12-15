# Joerg Heintz, joergheintz@gmail.com,  December 2015
# simple software recommendation system


library(shiny)

# variables
    newnode <<- character()
    ncount <- 0
    sr <- 0 # id of selected recommendation table, triggers update of decision support table
    x0 <- 'multiple'
    pipelinestep <<- character()
    mypipefilter <<- character()
    userpipe <<- create_graph()
    tdecisionTable <- data.frame()
    n <- integer
    b <<- NULL
    s <<- NULL

# create standard pipeline
    grVizOutput('userp', width = "200px", height = "400px" )# pipelines


StandPipelines<- function(standnodes, nodecolor="#2980b9 "){
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

#####################################################

userpipe <<- create_graph(
    graph_attrs = c("bgcolor = none"),
    node_attrs = c("fontcolor = white", "color = white", "fontsize = 11", "shape = ellipse", "width = 3"),
    edge_attrs = c("arrowsize = 1", "color = white"))
   
softconstr <<- function(newnode){
## add node
    #print(paste("newnode: ", newnode))
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
        #print(paste("n: ", n, "if: # nodes: ", ncount,  "   from: ", mynodes[ncount-1,1], "to ", mynodes[ncount,1]))
    }
    b<<- render_graph(userpipe)
 }

#####################################################

shinyServer(function(input, output, session) {
    options(shiny.trace = TRUE)
    
    # standard pipelines: user selects one of determined pipelines
    callStandardPipe <<- reactive({
            switch(input$selectpipe,
               "peak calling" = "peak calling",
               "differential binding" = "differential binding",
               "motif analysis" = "motif analysis",
               "peak annotation" = "peak annotation",
               "gene ontology analysis" = "gene ontology analysis")})
    
        observeEvent(input$selectpipe, {
            StandPipelines(callStandardPipe())
        
        output$pipelinestep <<- renderUI({
            selectInput("pipelinestep", "B) Pipeline Step", 
            choices = as.character(pipe_nodes))
        })
    })
   
    # software per pipeline step -> subsetting software based on pipeline step & output on data table
    pipelinestepsoftInput <<- reactive({
            print(paste("Reactive Function pipelinestepsoftInput"))
             #if (is.na(input$soft_category)) softassesscat = "functionality"
             #if (is.na(input$pipelinestep)) mypipefilter = "read mapping"
            softassesscat<<- input$soft_category
            mypipefilter <<- input$pipelinestep 
                
            # selecting mysoft data.frame, mysubcol vector reads enabled fields ("1") from myrecdata.xlsx
            mysubcol <<- as.logical(sapply(mysoft[mysoft$goal == softassesscat,], function(x) grepl("^1$", x)))
            softperpipe <- mysoft[mysoft$goal==mypipefilter ,mysubcol, drop = FALSE]
            print(paste("softperpipe: ", softperpipe))
            
            subsoft<-as.character(softperpipe[,"Software"])
            #print(paste("subsoft: ", subsoft, " timestamp = ", timestamp()))
            
            # software selection per pipeline step
            output$pconst <<- renderUI({selectizeInput(
                'pconst', '4) Software', choices = as.character(mysoft[mysoft$goal==mypipefilter, 3]),
                 multiple = TRUE,  options = list(maxItems = 1, placeholder = 'Please select an option below'))
            })
            
            ## colors the selected pipeline step node
            StandPipelines(callStandardPipe())
            output$standp <<- renderGrViz({reC})
            
            output$softrecommend = DT::renderDataTable(
                recommendationTable(),
                rownames = FALSE, class = "cell_border", 
                server = TRUE, 
                selection = 'single',
                style = 'bootstrap',
                options = list(
                    #columnDefs = list(list(searchable = FALSE), list(className = 'dt-center')),
                    dom = 't', # "ft" is with search field
                    scrollY = '200px', paging = FALSE,
                    #scrollX = '890px',
                    initComplete = JS(
                        "function(settings, json) {",
                        "$(this.api().table().header()).css({'background-color': '#2980b9', 'color': '#fff'});","}"
                        #"$(this.api().table().Seletion().css({selection-color})
                        
                    ),
                    extensions = 'ColVis'
                )
            )
        
            ## call citation history function
            output$papertrend <- renderPlot({
                cithist(subsoft)
            })
            
            ## input for outputDataTable
            softperpipe 
    })
    
    # Software Decision Support Table
    observeEvent({input$softrecommend_rows_selected}, {
        sr <<- as.vector(input$softrecommend_rows_selected)
        if (sr>0) {
            myd<- finalDecision(sr)
        }
        print("print selected row:")
        print(sr)
   
    
        output$decTable = DT::renderDataTable(
            myd, 
            rownames = TRUE, class = "cell_border", 
            server = TRUE, 
            style = 'bootstrap',
            selection = 'none',
            options = list(
                #columnDefs = list(list(searchable = FALSE), list(className = 'dt-center')),
                dom = 't', # "ft" is with search field
                scrollY = '200px', paging = FALSE,
                scrollX = '500px',
                columnDefs = list(list(searchable = FALSE, 
                           bsort = FALSE, 
                           className = 'dt-left',
                           autoWidth = TRUE,
                           width = '250px' , targets = c(0),
                           width = '150px' , targets = c(1),
                           width = '150px' , targets = c(2),
                           width = '150px' , targets = c(3)
                           #width = '15%' , targets = c(4)
                )),
                initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': '#2980b9', 'color': '#fff'});","}"),
                extensions = 'ColVis'
            )
        )
    })
  
################################ output to ui  #######################################
    
     scoreInput <<- reactive({
         input$soft_category
         softperpipe <<- mysoft[ ,c(15:17), drop = FALSE]})
    
    output$input_type_text <- renderText({
        input$soft_category})
    
    output$dynamic_value <- renderPrint({
        str(input$dynamic)})
    
######################################################################################
    
    # software table 
    output$tabletitle <- renderText({ 
        paste("Software Information System: Software \"",input$pipelinestep,"\" with focus on \"", input$soft_category, "\"", sep="")
        paste("Software Information System:   Software", input$soft_category, sep="")
    })
    
    # Knowledge Table
    output$softdatatableout = DT::renderDataTable(rownames = FALSE, class = "cell_border", 
        pipelinestepsoftInput(),
        server = TRUE, 
        selection = x0,
        style = 'bootstrap',
        options = list(
            dom = 'ft', # "ft" is with search field
            scrollY = '200px', paging = FALSE,
            scrollX = '890px',
            columnDefs = list(list(searchable = FALSE, 
                                   bsort = FALSE, 
                                   className = 'dt-left',
                                   autoWidth = TRUE,
                                   width = '10%' , targets = c(0),
                                   width = '10%' , targets = c(1),
                                   width = '10%' , targets = c(2),
                                   width = '70%' , targets = c(3)
                                   
            )),
            initComplete = JS(
                "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': 'grey', 'color': '#fff'});","
                }"
            
            )
        )
    )
    
                
    # Software Lineup: selection made on the Knowledge DataTable
    observeEvent(input$softdatatableout_rows_selected, {
        s <<- input$softdatatableout_rows_selected
        
        if (length(s) > 2) {
            x0 <- 'none'
            print(x0)
        }
        else {
            mypipelinesoftInput <<- as.data.frame(pipelinestepsoftInput())
            selectedSoft <<- as.character(mypipelinesoftInput[s,"Software"]) # s selected row from KnowledgeTable
            print("software name(s)for benchmarking and assessment category stored")
            
            benchmarking(selectedSoft, softassesscat)
            print("benchmarking function processed")
            output$benchdatatableout = DT::renderDataTable(mybench, rownames = FALSE, 
                server = TRUE,
                selection = 'none',
                style = 'bootstrap',
                options = list(
                        dom = 't',
                        scrollY = '200px', paging = FALSE,
                        scrollX = '590px',
                        extensions = 'ColVis',
                        columnDefs = list(list(searchable = FALSE, 
                                                className = 'dt-left',
                                                autoWidth = FALSE,
                                                #width = '20%' , targets = c(0),
                                                width = '40%' , targets = c(1),
                                                width = '40%' , targets = c(2)
                        )),
                        initComplete = JS(
                            "function(settings, json) {",
                                "$(this.api().table().header()).css({'background-color': '#2980b9', 'color': '#fff'});"
                                ,"}")
                )
            )
        }
    })
})