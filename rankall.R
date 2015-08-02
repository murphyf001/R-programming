# "R programming by Johns Hopkins University on Coursera. 
# By: Frank Murphy
# Uses http://hospitalcompare.hhs.gov outcome-of-care-measures.csv for its dataset
# Builds on best.R and rankhospital.R
# Output: best, worst, or hospital with rank as specified in arguments for each state
#     returns NA in state's rank if rank does not exist
# Demonstrates:
#     Error catching
#     data subsetting
#     data output formating
rankall <- function(outcome, num = "best") {
    # used shorter variables ot= outcome table, oc = outcome, v = valid outcome list
    ## Read outcome data
    ot <- read.csv("outcome-of-care-measures.csv", colClasses = "character") 
    
    ## Check that state and outcome are valid
    # Subset data on state and outcome ## Error out if outcome not found
    oc <-tolower(outcome)
    v <- c('heart attack','heart failure','pneumonia')
    # set myTail and myHead variable to print best, worst, 
    # confirm that num is "best", "worst" or a number or errors out
    myTail <- FALSE
    myHead <- FALSE
    if (!(oc %in% v)) {
        stop("invalid outcome")
    }    
    if (is.numeric(num)) {
        myNum <- num
    } else if (tolower(num) == "worst") {
        myTail <- TRUE
    } else if (tolower(num) == "best") {
        myHead <- TRUE
    } else { stop("invalid number") }
    
    # confirm that the outcome is valid   
    if (oc == "heart attack" ) {
        ss <- subset(ot, select = c(Hospital.Name, 
                    Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,State)) 
    }
    else if (oc == "heart failure") {
        ss <- subset(ot, select = c(Hospital.Name, 
                    Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, State)) 
    } 
    else if (oc == "pneumonia") {
        ss <- subset(ot, select = c(Hospital.Name, 
                    Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, State)) 
    } else { stop('invalid outcome') }
    
    # make the column names easier to access
    colnames(ss) <- c("hospital","outcome", "state")
    # ignore outcome text "Not Available" does not convert right with R to NA
    ss <- subset(ss, outcome !="Not Available")
    # order by lowest mortality than lowest state alphabetically
    ss <- ss[order(as.numeric(ss$outcome),ss$hospital), ]

    ## For each state, find the hospital of the given rank
    myStates <- unique(ot["State"])
    myStates <- myStates[order(myStates$State),]
    # ns = number of states, create character vector (hosp) to store ranked 
    # hospital names
    ns <- length(myStates)
    hosp <- rep("",ns)
    # loop through ordered state abbreviations
    for (myC in 1:ns) {
        # take state subset, store best, worst, <NA>, or ranked hospital to th
        ss2 <- subset(ss,state == myStates[myC])
        # store tail or head hospital to th (temp hospital)      
        if (myTail == TRUE) {
            th <- tail(ss2,1)
            th <- th[1,1]
        } else if (myHead == TRUE) {
            th <- head(ss2,1)
            th <- th[1,1]
            # rank argument too high for the number of ranked hospitals
        } else if (myNum > nrow(ss2)) {
            th <- '<NA>'
        } else {
            # store the ranked hospital to th
            th <- ss2[myNum,1]
        }
        # store th in the state's ranked hospital record
        hosp[myC] <- th
    }
    # create dataframe to store hospital names and associated state
    df <- data.frame(hospital=character(),state=character(),stringsAsFactors=FALSE)
    df <- cbind(hosp,myStates)
    colnames(df) <- (c('hospital','state'))
    row.names(df) <- myStates
    
    # right justify output without quotes.
    # puts quotes if you head or tail the function externally
    #df2 <-format(df, justify = 'right')
    #print(df2, quote = FALSE, trim= TRUE)
    #    for (myC in 1:ns) {
    #        print(c(hosp[myC],myStates[myC]))
    #    }
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    
    # assignment just called for returning a data.frame
    return(as.data.frame(df))
    
}

