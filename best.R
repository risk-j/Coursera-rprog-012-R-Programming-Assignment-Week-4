best <- function(state, outcome) {
    ## Read outcome data
    data<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Input Validation
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
    
    ## Input outcome validation
    if(!outcome %in% colnames(data)) {
        stop("invalid outcome")
    }
    
    ## Store the data of specified state, and select the outcome
    data<-data[data$State==state,]
    selected<-data[[outcome]]
    
    ## Replace the "Not Available" in the columns, I cant find better solution for this
    selected<-replace(selected, selected=="Not Available",NA)
    min<-min(as.numeric(selected[complete.cases(selected)]))
    
    ## Data in "data" are still in character format, necessary to turn integer result
    ## into string with ".0"
    if(min%%1==0)
        min <-paste(min,".0",sep="")
    
    ## Retrieve the hospital with the ideal value
    result<-data$Hospital.Name[data[[outcome]]==min]
    sort(result)[1]
    
}