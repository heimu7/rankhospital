rankhospital
============
## part one
## Write a function called best that take two arguments: the 2-character abbreviated name of a state and an
outcome name. The function reads the outcome-of-care-measures.csv le and returns a character vector
with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specied outcome
in that state. The hospital name is the name provided in the Hospital.Name variable. The outcomes can
be one of \heart attack", \heart failure", or \pneumonia". Hospitals that do not have data on a particular
outcome should be excluded from the set of hospitals when deciding the rankings.
## Handling ties. If there is a tie for the best hospital for a given outcome, then the hospital names should
be sorted in alphabetical order and the rst hospital in that set should be chosen (i.e. if hospitals \b", \c",
and \f" are tied for best, then hospital \b" should be returned).

## The function should check the validity of its arguments. If an invalid state value is passed to best, the
function should throw an error via the stop function with the exact message \invalid state". If an invalid
outcome value is passed to best, the function should throw an error via the stop function with the exact
message \invalid outcome".

best <- function(state, outcome) {
  
    ## Check that state and outcome are valid

    dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    listofstates<-unique(dat$State)
    if (state %in% listofstates == FALSE){
        stop("invalid state")
    }
    else if (outcome %in% c("heart attack", "heart failure", "pneumonia") == FALSE)
    {
        stop("invalid outcome")
    }
  
    ## Read outcome data

    dataneed <- data.frame()
    
    if(outcome == "heart attack"){
        dataneed <- dat[dat$State == state, c(2, 7, 11)]
    }else if(outcome == "heart failure"){
        dataneed <- dat[dat$State == state, c(2, 7, 17)]
    }else{
        dataneed <- dat[dat$State == state, c(2, 7, 23)] 
    }
    colnames(dataneed) <- c("hospitalname", "state", "whatkind")
    dataneed[,3] <- as.numeric(dataneed[,3])
    
    ## Return hospital name in that state with lowest 30-day death rate
    
    sort.dataneed <- dataneed[order(dataneed$whatkind, dataneed$hospitalname,na.last = TRUE), ]
    sort.dataneed[1,1]
}



## Write a function called rankhospital that takes three arguments: the 2-character abbreviated name of a
state (state), an outcome (outcome), and the ranking of a hospital in that state for that outcome (num).
The function reads the outcome-of-care-measures.csv le and returns a character vector with the name
of the hospital that has the ranking specied by the num argument. For example, the call
rankhospital("MD", "heart failure", 5) would return a character vector containing the name of the hospital with the 5th lowest 30-day death rate
for heart failure. The num argument can take values \best", \worst", or an integer indicating the ranking
(smaller numbers are better). If the number given by num is larger than the number of hospitals in that
state, then the function should return NA. Hospitals that do not have data on a particular outcome should
be excluded from the set of hospitals when deciding the rankings.

## Handling ties. It may occur that multiple hospitals have the same 30-day mortality rate for a given cause
of death. In those cases ties should be broken by using the hospital name. For example, in Texas (\TX"),
the hospitals with lowest 30-day mortality rate for heart failure are shown here.

## Note that Cypress Fairbanks Medical Center and Detar Hospital Navarro both have the same 30-day rate
(8.7). However, because Cypress comes before Detar alphabetically, Cypress is ranked number 3 in this
scheme and Detar is ranked number 4. One can use the order function to sort multiple vectors in this
manner (i.e. where one vector is used to break ties in another vector).

## The function should check the validity of its arguments. If an invalid state value is passed to best, the
function should throw an error via the stop function with the exact message \invalid state". If an invalid
outcome value is passed to best, the function should throw an error via the stop function with the exact
message \invalid outcome".


rankhospital <- function(state, outcome, num = "best") {
  
    ## Check that state and outcome are valid
    dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    listofstates<-unique(dat$State)
    if (state %in% listofstates == FALSE){
        stop("invalid state")
    }
    else if (outcome %in% c("heart attack", "heart failure", "pneumonia") == FALSE)
    {
        stop("invalid outcome")
    }
    
    ## Read outcome data
    
    dataneed <- data.frame()
    
    if(outcome == "heart attack"){
        dataneed <- dat[dat$State == state, c(2, 7, 11)]
    }else if(outcome == "heart failure"){
        dataneed <- dat[dat$State == state, c(2, 7, 17)]
    }else{
        dataneed <- dat[dat$State == state, c(2, 7, 23)] 
    }
    colnames(dataneed) <- c("hospitalname", "state", "whatkind")
    dataneed[,3] <- as.numeric(dataneed[,3])
    
    ## Return hospital name in that state with the given rank 30-day death rate
    
    ##关于num的一些检验，best，worst，larger than
    numbernona <- sum(!is.na(dataneed$whatkind))
    
    if(num=="best"){
        num <- 1
    }else if(num=="worst"){
        num <- numbernona
    }
    
    sort.dataneed <- dataneed[order(dataneed$whatkind, dataneed$hospitalname,na.last = TRUE), ]
    sort.dataneed[num,1]
}





