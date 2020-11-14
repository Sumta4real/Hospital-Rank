rankhospital <- function(state, outcome, num = "best") { 
    ## Read outcome data
    
    outcome_data <- read.csv("outcome-of-care-measures.csv")
    outcome_options <- c("heart attack", "heart failure","pneumonia")
    
    ## Check that state and outcome are valid
    
    if(state %in% outcome_data$State & outcome %in% outcome_options){
        if (outcome == "heart attack"){col <- 11}
        else if (outcome == "heart failure"){col <- 17}
        else if (outcome == "pneumonia") {col <- 23}
        data <- subset(outcome_data,State==state) 
        data[,col] <- as.numeric(data[,col])
        hospital <- data$Hospital.Name[order(data[,col],data$Hospital.Name,na.last = NA)]
        if (num == "best"){num = 1}
        else if (num == "worst"){num = length(hospital)}
        else if (num > length(hospital)){result = NA}
        result <- hospital[num]
    }
    else if (!(state %in% outcome_data$State) & outcome %in% outcome_options)stop("invalid state")
    else if (state %in% outcome_data$State & (!(outcome %in% outcome_options)))stop("invalid outcome")
    else stop("invalid state and invalid outcome")
    ## Return hospital name in that state with the given rank ## 30-day death rate
    result
}