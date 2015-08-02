# "R programming by Johns Hopkins University on Coursera. 
# By: Frank Murphy
# Uses http://hospitalcompare.hhs.gov outcome-of-care-measures.csv for its dataset
# Demonstrates:
#     Error catching
#     data subsetting.
best <- function(state,outcome) {
    ## Read outcome data 
    outcomeTable <- read.csv("outcome-of-care-measures.csv",
                             colClasses = "character")
    # state argument stored in myState to avoid potential conflicts
    myState <- state
    ## set valid states list and outcomes list
    myStates <- unique(outcomeTable["State"])
    outcome <- tolower(outcome) # remove case sensitivity of outcome arg

    ## Check that state and outcome are valid
    myFoundState <- NULL
    # Loop through the states in the dataset until state is found.
    for (i in 1:nrow(myStates)) {
        if (state == myStates[i,1]) { 
            myFoundState <- TRUE      
            break                     
        }
    }
    #    Attempted myTest <- myState %in% myStates[1, ] without success
   
    # Error out if state called is not in the table
    if (is.null(myFoundState)) stop("invalid state")

    # Subset data based on state and outcome ## Error out if outcome not found
    if (outcome == "heart attack" ) {
        mySubset <- subset(outcomeTable, State == myState, select = c(Hospital.Name, 
                        Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)) 
    }
    else if (outcome == "heart failure") {
        mySubset <- subset(outcomeTable, State == myState, select = c(Hospital.Name, 
                        Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)) 
    } 
    else if (outcome == "pneumonia") {
        mySubset <- subset(outcomeTable, State == myState, select = c(Hospital.Name, 
                        Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)) 
    }
    else {
       stop("invalid outcome")
    }
    #make it easier to access columns
    colnames(mySubset) <- c("name","outcome")
    # "Not available" is string which is not recognized by R as NA
    mySubset <- subset(mySubset, outcome !="Not Available")
    
    ## Sort hospitals by outcome then name produces the best outcome
    mySubset <- mySubset[order(as.numeric(mySubset$outcome), mySubset$name),]
   
    print(mySubset[1,1])
}
