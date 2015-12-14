# Joerg Heintz, August 20th, 2015
# Collects data from pubmed central using RISmed package. PMID, Citations, Publication Year, Journal, Title, Abstract

main_software <- function(st=0, en=0){
library(RISmed)
library(dplyr)
myhit <- as.numeric(vector())
software_publication_collection <- data.frame()


# Retrievs software, search term from softw_align_listhods.csv
st <- 1
en <- 4
print("========>>>>  retrieving search terms from myrecdata")
reference <<- read.xlsx("05_AppAlignSoft/data/myrecdata.xlsx", 1, check.names=FALSE)[,c("Software", "reference"), drop = FALSE]
reference <<- reference[complete.cases(reference) & !grepl("1", reference[,2]),]


search_term <- reference[st:en,c("reference"), drop = FALSE]
Mnum <- st
# loops through the search terms.
for (n in seq_along(search_term[,1])) {
    # Call function to download the data set from pubmed        
    if (n <= en) {  # termination point, e.g. row 5 
        #pubmed_search<-dget("pubmed_software_ranking.R")
        Mnum <- st + n - 1
    }
    print("end retrieving search terms")
    query <- paste(search_term[n,1])
    SoftName <- search_term[n,1]
    print(paste("========>>>> ID: ", Mnum, "  PubMed search-term: ", query))
    
    # Search Pubmed, retrieve complete record, subset record 
    ngs_search <- EUtilsSummary(query, type = "esearch",db = "pubmed",mindate = 2000, maxdate = 2015)
    hits <- QueryCount(ngs_search)
    ngs_records
    myhit[Mnum] <- hits
    search_term[Mnum,4] <- myhit[Mnum]
    
    if (hits != 0) {
        print(paste0(Sys.time(),':   ', QueryCount(ngs_search),' hits'))
        ngs_records <- EUtilsGet(ngs_search)
        print(paste0(Sys.time(), ":   ngs record retrieved"))
        pubmed_data <- data.frame('PMID' = PMID(ngs_records), 'Citations' = Cited(ngs_records), 'YearPubmed' = YearPubmed(ngs_records), 'Journal' = Title(ngs_records), 'Title' = ArticleTitle(ngs_records))
        pubmed_data <- pubmed_data %>% mutate(Software = SoftName, Search_Term = query) %>% select(PMID, Software, Search_Term, Journal, Citations, YearPubmed, Title)  %>% arrange(-YearPubmed, -Citations)
        print(paste0(Sys.time(), ":   pubmed dataframe generated"))
        
        # Overview how many articles per year, no journal information
        #years <- YearPubmed(ngs_records)
        #ngs_pubs_count <- as.data.frame(table(years))
        print('writing search results into csv file')
        
        # Filename for search results, store search results
        filename2 = paste("01_csv/ID_", Mnum,"_Hits_",hits,"_",SoftName, '.csv', sep = '')
        #write.table(pubmed_data,filename,quote=F,row.names=F, sep='\t')
        write.csv(pubmed_data, filename2, row.names = FALSE)
        # generate software - paper collection
        software_publication_collection <- rbind(pubmed_data, software_publication_collection)
    }
    else {
       print(paste(" >>>>>>>>>>>>  !! NO HITS FOR ", query, "in Pubmed !! <<<<<<<<<<<<<<< "))
       # filename2= paste("01_csv/", Mnum,"__0_hits__",SoftName, '.csv', sep='')
       # write.table("0_Hits", filename2, row.names=FALSE,sep=";")
    }}


write.csv(software_publication_collection, "method_pipeline_publication_collection.csv", row.names = FALSE)
write.csv(search_term, "mymethod_pipeline_list_1.csv", row.names = FALSE)
print("end of Pubmed search")
}


################################################################################################
# Joerg Heintz, Sep.9, 2015
# The scripts converts PMID [Pubmed] into PMCID [Pubmed Central] by using NIH's API. 
# files: software_ranking.csv, this file list the PMID of pubmed
library(httr)
library(stringr)
pmtopmc <- function(st=2, en=267){
    # Call the function with start and end row. 
    #Load PMID
    mypmcid <- as.character()
    pmid <- as.data.frame(read.csv("method_pipeline_publication_collection.csv"))
    sapply(pmid[st:en, 1], function(x) { 
        u <- paste("http://www.ncbi.nlm.nih.gov/pmc/utils/idconv/v1.0/?ids=", x, "&format=xml", sep = "" )
        # Retrieves PMCID and other text, converts into characters, splits, and converts into a data.frame fro subsetting
        a <- as.data.frame(strsplit(as.character(GET(u)), " "))[12,1] 
        # filter
        mypmcid <- gsub('pmcid=', '',a)
        mypmcid <- gsub("\"" ,"",mypmcid)
        if (grepl("pmid", mypmcid)) {
            mypmcid <- "NA"
            pmid[st,2] <<- mypmcid
        }
        else {
            pmid[st,2] <<- mypmcid
        }
        print(paste("=====PMID= ", pmid[st,2], "  PMCID=", pmid[st,1], "  ID=", st))
        st <<- st + 1
    })
    colnames(pmid)[2] <- "PMCID"
    print(head(pmid))
    write.csv(pmid, "method_pipeline_publication_list_Chipseq_RNASeq_HIC.csv", row.names = FALSE)
}

#################################################################################################
# Joerg Heintz, Sep. 9, 2015
# The script downloads full articels in pdf format from pubmed central 
spd <- function(st=2, en=267, ti=5){
    st <<- st
    en <<- en
    mypmc <- read.csv("method_pipeline_publication_list_ChipSeq_RNASeq_HIC.csv")[,c(1:7)]
    sapply(mypmc[st:en,2], function(x){
        print(paste("======== START: ", st, "==== END: ", en, "============"))
        if (st < en) {
            if (grepl("PMC", x)) {
                print(paste("ID= ", mypmc[st,2], "PMC ID =", x))
                urlpdf <- paste("http://www.ncbi.nlm.nih.gov/pmc/articles/", x, "/pdf/", sep = "")
                filename <- paste("01_csv/", "", mypmc[st,7], "_", mypmc[st,3],"_method","_Cit_", mypmc[st,6], "_", substr(mypmc[st,5], 1, 12),  "_", x, ".pdf", sep = "")
                try(download.file(urlpdf, filename , mode = "wb"))
                st <<- st + 1
                Sys.sleep(ti)
            }
            else  st <<- st + 1
        }
    })
}

#################################################################################################
#Combining search results into one file, merging that file with "impact factor" list. 
#Joerg Heintz
#August 20th, 2015
    # collecting file and store then in a vector
    files <- list.files('01_csv', pattern = '*.csv', full.names = F, recursive = FALSE)
    mydata <- data.frame()
    for (i in seq_along(files)) {
        path <- paste('01_csv/', files[i], sep = '')
        data <- read.csv2(path, header = T) # load the file
        print(data)
        mydata <- rbind(data, mydata)
    }
    #colnames(mydata)<-c("pmid", "software", "search_term", "journal","citations", "year", "title")
    write.csv(mydata, "software_ranking.csv")
    mydata <- select(mydata, pmid, software, journal, citations, year)





