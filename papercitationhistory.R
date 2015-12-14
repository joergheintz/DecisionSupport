# Joerg Heintz, November 2015
# The software downloads from google scholar the citation history of the papers listed in the myrecdata.xlsx,
# and generates a line graphic. 
library(xlsx)
library(dplyr)

# citation history download from google scholar. Google resets the connection by frequent access. 
# paphist <- function(sowa){
#     mycol <- c("Software", "googlearticleID", "googleauthID", "reference")
#     mypapin <<- read.xlsx("data/myrecdata.xlsx", 1, check.names=FALSE)[,mycol]
#     mypapin <<- mypapin[complete.cases(mypapin),]
# 
#     for (i in 1:(length(mypapin$Software))){
#         autID <- mypapin[i, "googleauthID"]
#         #print(autID)
#         papID <- mypapin[i, "googlearticleID"]
# 
#         #print(papID)
#         myarticle <- get_article_cite_history(autID, papID)
# 
#         ## fill up with software name
#         myname <- rep(mypapin[i, "Software"], length(myarticle[,1]))
# 
#         ## fill with date
#         myd <- Sys.Date()
#         mydate<-rep(myd, length(myarticle[,1]))
# 
#         ## fill with main paper title
#         mypaptit <- rep(mypapin[i,4], length(myarticle[,1]))
# 
#         ## build data frame with paper hitory
#         mypap<-cbind(myname, myarticle, mydate, mypaptit)
# 
#         #print(mypap)
#         myfil <- paste("data/papercitationhist/", mydate[1], "_", mypapin[i,1], ".csv", sep = '')
#         write.csv(mypap, myfil, row.names = FALSE)
#     }
# }

# collecting & merging paper citation history files
mymerge <<- function(){
    myfiles <- list.files('data/papercitationhist')
    myDB <- data.frame()
    for (i in myfiles) {
        if (!grepl("xlsx", i)){
            #print(i)
            myfil <- paste('data/papercitationhist/', i, sep = "")
            mydatadelta <- read.csv(myfil, header = TRUE)
            mydatadelta$sum.citation <- sum(mydatadelta$cites)
            myDB <- rbind.fill(myDB, mydatadelta)
        }
    }
    
    myDB<-myDB[,c(1:3,7,5,6,4)]
    names(myDB)<- c("Software", "Year", "Citation", "Sum.Citation","Retrieving.Date", "Reference", "googlearticleID")
    myDB<-myDB[order(desc(myDB$Sum.Citation), myDB$Software, myDB$Year, myDB$Citation),]
    mysumhist<-aggregate(myDB$Citation, list(myDB$Software, myDB$Retrieving.Date, myDB$Reference), FUN = sum)[,c(1,4,2,3)]
    names(mysumhist) <- c("Software", "Sum.Citation", "Retrieving.Date", "Reference")
    write.xlsx(myDB, "data/papercitationhist/papercithist.xlsx", "CitationHistory", row.names = FALSE)
    write.xlsx(mysumhist,"data/papercitationhist/papercithist.xlsx", "SumCitationHistory", row.names = FALSE, append = TRUE)
    print("Status: Papercithist.xlsx written, see data/paperciationhist/")
}

# function writes citation history numbers into myrecdata.xlsx, format of excel sheet is preserved by setStyleAction-function
PopulateMyrecdataCitations <- function(){
    mycitation <- read.xlsx("data/papercitationhist/papercithist.xlsx", "SumCitationHistory")
    mysoft <<- read.xlsx("data/myrecdata.xlsx", "software", check.names=FALSE)
    wb<- loadWorkbook("data/myrecdata.xlsx")
    setStyleAction(wb,XLC$"STYLE_ACTION.NONE")
    myc<- mysoft
    for (sof in mycitation[, "Software"]){
        myc[myc$Software == sof, "citations"] <- mycitation[mycitation$Software == sof, "Sum.Citation"]
    }
    writeWorksheet(wb, myc, "software" )
}

## function rendering citation history to graphic
cithist <<- function(subsoft){
    print(paste("Start cithist: ", timestamp()))
    print(paste(" Subsoft: ", subsoft))
    
    mydata <- data.frame()
    row.names(mydata)<-NULL
    my0<<-read.xlsx("data/papercitationhist/papercithist.xlsx", "CitationHistory", header = TRUE)
    mydata <- my0[my0$Software  %in% subsoft, ]
    #print(paste("print vector with software for the selected pipelinesteop:", subsoft))
    #print(head(mydata))
    print("Status: citation history retrieved ")
    
    # coordinates for google image 
    xmin = min(mydata$Year)
    delta <- max(mydata$Year) - min(mydata$Year)
    xmax <- xmin + round(delta*0.2)

    #print(mydata)
    img <- readPNG("images/googlescholar.png")
    g <- rasterGrob(img, height = 0.4, width = 0.7)
    
    # by default ggplot2 orders the legend by the factor-level. Changed that to ordering by sum.citation 
    mydata$Software <- factor(mydata$Software, levels=(unique(mydata$Software)))
     
    p<-ggplot(mydata, aes(x=Year, y=Citation)) + 
        geom_line(aes(color = Software), size = 1.1) + 
        coord_cartesian(ylim = c(0, max(mydata$Citation+0.02*max(mydata$Citation)))) + 
        theme(legend.position= c(.05, .9), legend.justification=c(0,1),
            axis.text.x=element_text(colour="white"),
            axis.text.y=element_text(colour="white"),
            axis.title.x = element_text(colour = "white", vjust = -0.1, hjust = 1),
            axis.title.y = element_text(colour = "white", vjust = 0.9, hjust = 0.97),
            #legend.margin = unit(0, "cm"),
            legend.background = element_rect(fill = alpha('white', 0.4)),
            legend.text =element_text(size = 8),
            plot.background = element_rect(fill = "#2b3e50"),  
            panel.background = element_rect(fill = "grey")) +
        annotation_custom(g,  xmin = xmin, ymin = round(max(mydata$Citation)*0.85), xmax = xmax, ymax = round(max(mydata$Citation)))
        #print(p)
        p
}

