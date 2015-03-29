rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
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
    
    ## Clean the "Not Available" in the columns
    nas<-(data[[outcome]] == "Not Available")
    data[[outcome]][nas]<-NA
    data<-data[complete.cases(data[[outcome]]),]
    
    ## To numeric
    data[[outcome]]<-as.numeric(data[[outcome]])
    
    ## Split data by state
    s<-split(data,data$State)
    
    ## Sort out the data
    sorted<-lapply(s,function(x) {
        x[order(x[[outcome]], x$Hospital.Name),]
    })
    
    ## Initialize the result data frame
    result<-data.frame(sort(unique(data$State)))
    
    if(num=="best")
        num=1
    
    ## Store the result hospital name in "hospital"
    hospital<-c()
    for(state in sort(unique(data$State))) {
        if(num=="worst") 
            hospital<-c(hospital,sorted[[state]]$Hospital.Name[nrow(sorted[[state]])])
        else
            hospital<-c(hospital,sorted[[state]]$Hospital.Name[num])
    }
    
    ## Format output
    colnames(result)<-c("state")
    rownames(result)<-sort(unique(data$State))
    sorted$Hospital.Name[num]
    cbind(hospital,result)
}