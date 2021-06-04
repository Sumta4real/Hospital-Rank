rankall <- function(outcome, num = "best") { 
    """this function ranks all hospitals in a state based on mortality rate and ailment specified 
    """
    ## Read outcome data
    outcome_data <- read.csv("outcome-of-care-measures.csv")
    
    ## Check that outcome is valid
    if (outcome == "heart attack"){col <- 11}
    else if (outcome == "heart failure"){col <- 17}
    else if (outcome == "pneumonia") {col <- 23}
    else stop("invalid outcome")
    
    ## For each state, find the hospital of the given rank
    state_list <- sort(unique(outcome_data$State))
    rank_list <- data.frame("hospital" = character(0),"state" = character(0))
    
    for (i in 1:length(state_list)){ 
        States[i] <- state_list[i]
        data <- subset(outcome_data,State==States[i])
        #N <- sum(!is.na(data$outcome))
        data[,col] <- as.numeric(data[,col])
        hospital <- data$Hospital.Name[order(data[,col],data$Hospital.Name,na.last = NA)]
        if (num == "best"){num = 1}
        else if (num == "worst"){num <- length(hospital)}
        else if (num > length(hospital)){result = NA}
        result <- hospital[num]
        rankall <- data.frame("hospital"=result,"state"=States[i])
        rank_list <- rbind(rank_list,rankall)
        
    }
    
    ## Return a data frame with the hospital names and the ## (abbreviated) state name
    rank_list
    
}
