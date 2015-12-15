# Joerg Heintz, joergheintz@gmail.com, 12. November 2015
# comparing / benchmarking software


benchmarking <- function(softinput="FastQC", field ="functionality", seqmeth = "ChIP-seq"){
    # print(softinput)
    # print(field)
    # print(seqmeth)
    # seqmeth <- "ChIP-seq"
    # field <- "functionality"
    # softinput <- "FastQC"
    #softassesscategories <-c("functionality", "performance", "compatibility", "popularity", "differentiate")
    softassesscategories <- as.character(mysoft[grepl("^M[1-9]$", mysoft$id), "goal"])

# subset based on selected method - ChIP-seq is default
    mydata <- mysoft[((mysoft$method %in% seqmeth) & (!grepl("^M[1-9]$", mysoft$id))) , ]
    mybench <- mydata[(mydata$Software %in% softinput),]
    mygroup <- mysoft[(grepl("^M[1-9]$", mysoft$id)), ]
    mybench <- rbind(mygroup, mybench)
    mybench <- as.data.frame(t(mybench))
    print("table for benchmarking transposed")
    
# editing row and col names
    mybench$id <- row.names(mybench)
    row.names(mybench) <- NULL
    colnames(mybench) <- as.vector(as.matrix(mybench[2,]))
    mybench <- mybench[-c(1,2),]

# subsetting based on software criteria
    mylogic <- (mybench[, field] == 1) 
    maxc <- ncol(mybench)
    mydelta <- seq(maxc, length(softassesscategories)+1, -1)
    mybench<- mybench[(mylogic & !is.na(mylogic)), mydelta, drop = FALSE]
    colnames(mybench) <- as.vector(as.matrix(mybench[1,]))
    colnames(mybench)[1] <- "Benchmarking:"
    mybench<<-mybench[-c(1),]
    mybench
}

recommendationTable <<- function (){
    softrecommendTable <<- data.frame()
    softrecommendTableUserRec <<- data.frame()
    for (i in nodes_pipeline[,"nodes"]){
        standsoftrecommend <- mysoft[mysoft$goal==i ,c("goal", "Software", "citations"), drop = FALSE]
        standsoftrecommend <- standsoftrecommend[complete.cases(standsoftrecommend),]
        standsoftrecommend <- standsoftrecommend[order(desc(standsoftrecommend$citations)),]
        myselect <- standsoftrecommend[standsoftrecommend$citations >= 0.4 * max(standsoftrecommend$citations),]
        # table aggregated, used for recommendation table, given when two software are of similar popularity
            myselectUserRec <<- aggregate(Software ~ goal, myselect, paste, collapse = ', ')
            softrecommendTableUserRec<<- rbind(softrecommendTableUserRec, myselectUserRec)
        # table not aggregated, used for recommendation based on user input
            softrecommendTable <<- rbind(softrecommendTable, myselect)
    }
        rownames(softrecommendTableUserRec) <- NULL
        colnames(softrecommendTableUserRec) <- c("Pipeline Step", "Recommended Software")
        unique(softrecommendTableUserRec[,c("Pipeline Step", "Recommended Software")])
}



finalDecision <<- function(sr=0){
    print("Start function finalDecision")
    mydecisionTable<- data.frame()
    mysubVectorCol <- vector('character')
    mydata <- softrecommendTable[duplicated(softrecommendTable$goal) | duplicated(softrecommendTable$goal, fromLast=TRUE),]
   
    sub0 <- !(duplicated(mydata) | duplicated(mydata, fromLast=TRUE))
    
    mysubVectorCol <- as.vector(names(mysoft)[mysoft[mysoft$goal == "differentiate",] == "1"])
    mysubVectorCol <- mysubVectorCol[!is.na(mysubVectorCol)]
    mydata <- mydata[sub0,]
    mydata <- unique(mydata)
    
    srx <- length(unlist(strsplit(softrecommendTableUserRec[sr,"Software"], ",")))
    
    if (srx>1) {
        subGoal<-as.character(softrecommendTableUserRec[sr,"goal"])
        subDecision <- mydata[mydata$goal == subGoal, ]
        mydata <- subDecision
        print("end if then "  )
    }
    else
        return(NULL)
    

    for (i in row.names(mydata)){
        decision<-mysoft[row.names(mysoft) == i, mysubVectorCol]
        mydecisionTable <- rbind(mydecisionTable, decision)
    }
    mydecisionTable <<- cbind(mydata, mydecisionTable)
    print("mydecisonTable")
    print(mydecisionTable)
    
    decisionTable <- mydecisionTable[, colSums(is.na(mydecisionTable)) != nrow(mydecisionTable)]
    decisionTable[decisionTable==-1] <- 'no'
    decisionTable[decisionTable==1] <- 'yes'
    
    tdecisionTable <<- as.data.frame(t(decisionTable))
    cnames<-as.character(as.matrix((tdecisionTable[1,])))
    colnames(tdecisionTable)<<-cnames
    tdecisionTable <<- tdecisionTable[c(-1),]
    #print(tdecisionTable)
    tdecisionTable
}
