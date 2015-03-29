rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    data<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Input state Validation
    if(!state %in% data$State) {
        stop("invalid state")
    }
    
    ## Format the input outcome name
    s<-strsplit(outcome," ")[[1]]
    outcome<-paste(toupper(substring(s,1,1)),substring(s,2),
                   sep="", collapse=" ")
    outcome<-gsub(" ",".",outcome)
    
    outcome<-paste("Hospital.30.Day.Death..Mortality..Rates.from.",
                   outcome,sep='')
    
    ##Input outcome validation
    if(!outcome %in% colnames(data)) {
        stop("invalid outcome")
    }
    
    ## Store the data of specified state, and select the outcome
    data<-data[data$State==state,]
    
    ## Replace the "Not Available" in the columns
    nas<-(data[[outcome]] == "Not Available")
    data[[outcome]][nas]<-NA
    data<-data[complete.cases(data[[outcome]]),]
    
    ## Turn data into numeric and sort them
    data[[outcome]]<-as.numeric(data[[outcome]])
    sorted<-data[order(data[[outcome]], data$Hospital.Name),]
    
    ## Convert input "best" and "worst"
    if(num=="best")
        num=1
    if(num=="worst")
        num<-nrow(data)
    
    sorted$Hospital.Name[num]
    
}