# "R programming by Johns Hopkins University on Coursera. 
# By: Frank Murphy
# Uses http://hospitalcompare.hhs.gov outcome-of-care-measures.csv for its dataset
# Builds on best.R 
# Output: best, worst, or hospital with rank as specified in arguments
#     returns NA if rank does not exist
# Demonstrates:
#     Error catching
#     data subsetting.
rankhospital <- function(state,outcome,num="best") {
    ## Read outcome data 
    outcomeTable <- read.csv("outcome-of-care-measures.csv",
                             colClasses = "character")
    myState <- state
    ## set valid states list and outcomes list
    myStates <- unique(outcomeTable["State"])
    
    ## Check that state and outcome are valid
    myFoundState <- NULL
    for (i in 1:nrow(myStates)) { # loop from record 1 to number of records
        #       tempstate <- myStates[i,1]
        #       if (state == tempstate) {
        if (state == myStates[i,1]) {
            myFoundState <- TRUE
            break
        }
    }
    
    # Error out if state called is not in the table
    if (is.null(myFoundState)) stop("invalid state")
    outcome <- tolower(outcome)
    # Subset data on state and outcome ## Error out if outcome not found
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
    colnames(mySubset) <- c("name","outcome")
    # "Not Available" is a string which is not recognized as NA by R
    mySubset <- subset(mySubset, outcome !="Not Available")
    
    ## Sort hospitals by outcome then name
    mySubset <- mySubset[order(as.numeric(mySubset$outcome), mySubset$name),]
    if (is.numeric(num)) {
        if (num > nrow(mySubset)) {
            return(NA)      # exit early if rank in argument is too high for data
        } 
        else {
            # print hospital at rank in the "num" argument
            print(mySubset[num, 1])
        }
    } 
    # print last good (not NA) hospital 
    else if (tolower(num) == "worst") {
            tail(mySubset$name,1)
    }
    # print first good (not NA) hospital
    else if (tolower(num) == "best") {
            head(mySubset$name,1)
    }
}
