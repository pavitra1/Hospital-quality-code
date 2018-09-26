# Hospital-quality-code
> outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
> head(outcome)
> outcome[, 11] <- as.numeric(outcome[, 11])
> ## You may get a warning about NAs being introduced; that is okay
> hist(outcome[, 11])
best <- function(state, outcome) {
	## Read outcome data

	## Check that state and outcome are valid

	## Return hospital name in that state with lowest 30-day death
	## rate
}
  > source("best.R")
> best("TX", "heart attack")
[1] "CYPRESS FAIRBANKS MEDICAL CENTER"
> best("TX", "heart failure")
[1] "FORT DUNCAN MEDICAL CENTER"
> best("MD", "heart attack")
[1] "JOHNS HOPKINS HOSPITAL, THE"
> best("MD", "pneumonia")
[1] "GREATER BALTIMORE MEDICAL CENTER"
> best("BB", "heart attack")
Error in best("BB", "heart attack") : invalid state
> best("NY", "hert attack")
Error in best("NY", "hert attack") : invalid outcome
>
> rankhospital("MD", "heart failure", 5)
> head(texas)
                         Hospital.Name Rate Rank
3935       FORT DUNCAN MEDICAL CENTER  8.1   1
4085  TOMBALL REGIONAL MEDICAL CENTER  8.5   2
4103 CYPRESS FAIRBANKS MEDICAL CENTER  8.7   3
3954           DETAR HOSPITAL NAVARRO  8.7   4
4010           METHODIST HOSPITAL,THE  8.8   5
3962  MISSION REGIONAL MEDICAL CENTER  8.8   6
rankhospital <- function(state, outcome, num = "best") {
	## Read outcome data
	
	## Check that state and outcome are valid
	
	## Return hospital name in that state with the given rank
	## 30-day death rate
}> source("rankhospital.R")
> rankhospital("TX", "heart failure", 4)
[1] "DETAR HOSPITAL NAVARRO"
> rankhospital("MD", "heart attack", "worst")
[1] "HARFORD MEMORIAL HOSPITAL"
> rankhospital("MN", "heart attack", 5000)
[1] NA
rankall <- function(outcome, num = "best") {
	## Read outcome data
	
	## Check that outcome is valid
	
	## For each state, find the hospital of the given rank
	
	## Return a data frame with the hospital names and the
	## (abbreviated) state name
}> source("rankall.R")
> head(rankall("heart attack", 20), 10)
                              hospital state
AK                                <NA>   AK
AL      D W MCMILLAN MEMORIAL HOSPITAL   AL
AR   ARKANSAS METHODIST MEDICAL CENTER   AR
AZ JOHN C LINCOLN DEER VALLEY HOSPITAL   AZ
CA               SHERMAN OAKS HOSPITAL   CA
CO            SKY RIDGE MEDICAL CENTER   CO
CT             MIDSTATE MEDICAL CENTER   CT
DC                                <NA>   DC
DE                                <NA>   DE
FL      SOUTH FLORIDA BAPTIST HOSPITAL   FL
> tail(rankall("pneumonia", "worst"), 3)
                                     hospital state
WI MAYO CLINIC HEALTH SYSTEM - NORTHLAND, INC   WI
WV                     PLATEAU MEDICAL CENTER   WV
WY           NORTH BIG HORN HOSPITAL DISTRICT   WY
> tail(rankall("heart failure"), 10)
                                                            hospital state
TN                         WELLMONT HAWKINS COUNTY MEMORIAL HOSPITAL   TN
TX                                        FORT DUNCAN MEDICAL CENTER   TX
UT VA SALT LAKE CITY HEALTHCARE - GEORGE E. WAHLEN VA MEDICAL CENTER   UT
VA                                          SENTARA POTOMAC HOSPITAL   VA
VI                            GOV JUAN F LUIS HOSPITAL & MEDICAL CTR   VI
VT                                              SPRINGFIELD HOSPITAL   VT
WA                                         HARBORVIEW MEDICAL CENTER   WA
WI                                    AURORA ST LUKES MEDICAL CENTER   WI
WV                                         FAIRMONT GENERAL HOSPITAL   WV
WY                                        CHEYENNE VA MEDICAL CENTER   WY
##Part 2: best.R:

best <- function(state, outcome) {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv")
        
        ## Check that state and outcome are valid
        states <- levels(data[, 7])[data[, 7]]
        state_flag <- FALSE
        for (i in 1:length(states)) {
                if (state == states[i]) {
                        state_flag <- TRUE
                }
        }
        if (!state_flag) {
                stop ("invalid state")
        } 
        if (!((outcome == "heart attack") | (outcome == "heart failure")
            | (outcome == "pneumonia"))) {
                stop ("invalid outcome")
        }
        
        ## Return hospital name in that state with lowest 30-day death rate
        col <- if (outcome == "heart attack") {
                11
        } else if (outcome == "heart failure") {
                17
        } else {
                23
        }
        
        data[, col] <- suppressWarnings(as.numeric(levels(data[, col])[data[, col]]))
        data[, 2] <- as.character(data[, 2])
        statedata <- data[grep(state, data$State), ]
        orderdata <- statedata[order(statedata[, col], statedata[, 2], na.last = NA), ]
        orderdata[1, 2]
}

##Part 3: rankhospital.R:

rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv")

        ## Check that state and outcome are valid
        states <- levels(data[, 7])[data[, 7]]
        state_flag <- FALSE
        for (i in 1:length(states)) {
                if (state == states[i]) {
                        state_flag <- TRUE
                }
        }
        if (!state_flag) {
                stop ("invalid state")
        } 
        if (!((outcome == "heart attack") | (outcome == "heart failure")
              | (outcome == "pneumonia"))) {
                stop ("invalid outcome")
        }

        ## Return hospital name in that state with the given rank 30-day death 
        ## rate
        col <- if (outcome == "heart attack") {
                11
        } else if (outcome == "heart failure") {
                17
        } else {
                23
        }
        
        data[, col] <- suppressWarnings(as.numeric(levels(data[, col])[data[, col]]))
        data[, 2] <- as.character(data[, 2])
        statedata <- data[grep(state, data$State), ]
        orderdata <- statedata[order(statedata[, col], statedata[, 2], na.last = NA), ]
        if(num == "best") {
                orderdata[1, 2]
        } else if(num == "worst") {
                orderdata[nrow(orderdata), 2]
        } else{
                orderdata[num, 2]
        }
}

##Part 4: rankall.R:

rankall <- function(outcome, num = "best") {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv")
        
        ## Check that outcome is valid
        if (!((outcome == "heart attack") | (outcome == "heart failure")
              | (outcome == "pneumonia"))) {
                stop ("invalid outcome")
        }

        ## For each state, find the hospital of the given rank
        col <- if (outcome == "heart attack") {
                11
        } else if (outcome == "heart failure") {
                17
        } else {
                23
        }
        
        data[, col] <- suppressWarnings(as.numeric(levels(data[, col])[data[, col]]))
        data[, 2] <- as.character(data[, 2])
        
        # Generate an empty vector that will be filled later, row by row, to 
        # generate the final output.
        output <- vector()
        
        states <- levels(data[, 7])
        
        for(i in 1:length(states)) {
                statedata <- data[grep(states[i], data$State), ]
                orderdata <- statedata[order(statedata[, col], statedata[, 2], 
                                             na.last = NA), ]
                hospital <- if(num == "best") {
                        orderdata[1, 2]
                } else if(num == "worst") {
                        orderdata[nrow(orderdata), 2]
                } else{
                        orderdata[num, 2]
                }
                output <- append(output, c(hospital, states[i]))
        }

        ## Return a data frame with the hospital names and the (abbreviated) 
        ## state name
        output <- as.data.frame(matrix(output, length(states), 2, byrow = TRUE))
        colnames(output) <- c("hospital", "state")
        rownames(output) <- states
        
        output
}

