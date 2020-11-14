#This function takes two argument-the 2-character abbreviated name of a state and an
#outcome name and returns a character vector with the name of the hospital that has 
#the least mortality for the specified outcome

#
best <- function(state, outcome) { 
    ## Read outcome data
    outcome_data <- read.csv("outcome-of-care-measures.csv")
    outcome_options <- c("heart attack", "heart failure","pneumonia")
    
    ## Check that state and outcome are valid
    if(state %in% outcome_data$State & outcome %in% outcome_options){
        if (outcome == "heart attack"){col <- 11}
        else if (outcome == "heart failure"){col <- 17}
        else if (outcome == "pneumonia") {col <- 23}
        
        ## subset the outcome_data by the state specified and assign it to variable data
        data <- subset(outcome_data,State==state) 
        data[,col] <- as.numeric(data[,col])
        
        #calculates the minimum value of the 30-day mortality for the outcome specified 
        min_mortality <- min(data[,col],na.rm=TRUE)
        hospital <- data$Hospital.Name[data[,col] == min_mortality]
        result <- sort(hospital)[1]
    }
    else if (!(state %in% outcome_data$State) & outcome %in% outcome_options)stop("invalid state")
    else if (state %in% outcome_data$State & (!(outcome %in% outcome_options)))stop("invalid outcome")
    else stop("invalid state and invalid outcome")
    ## Return hospital name in that state with lowest 30-day death ## rate
    result
}

