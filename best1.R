best <- function(state, outcome){
  ## Read outcome data
  outcomeData <-read.csv("outcome-of-care-measures.csv")
  bestHospital <- NULL
  
  ## Function for heart attack
  inHeartAttack <- function(state, x){
    filtered <- x[x$State==state & x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack != 'Not Available' ,c("Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")]
    converted<-filtered
    converted$Hospital.Name<-as.character(converted$Hospital.Name)
    converted$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack<-as.numeric(as.character(converted$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
    bestHosp<-converted[converted$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack==min(converted$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),c("Hospital.Name")]
    
    ## Return hospital name in that state with lowest 30-day death
    if (length(bestHosp) > 1) {
      bestHosp<-sort(bestHosp)
      bestHosp[1]
    } else
      bestHosp
  }
  
  ## Function for heart failure
  inHeartFailure <- function(state, x){
    filtered <- x[x$State==state & x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure != 'Not Available' ,c("Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")]
    converted<-filtered
    converted$Hospital.Name<-as.character(converted$Hospital.Name)
    converted$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure<-as.numeric(as.character(converted$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
    bestHosp<-converted[converted$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure==min(converted$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),c("Hospital.Name")]
    
    ## Return hospital name in that state with lowest 30-day death
    if (length(bestHosp) > 1) {
      bestHosp<-sort(bestHosp)
      bestHosp[1]
    } else
      bestHosp
  }
  
  ## Function for pneumonia
  inPneumonia <- function(state, x){
    filtered <- x[x$State==state & x$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia != 'Not Available' ,c("State","Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]
    
    converted<-filtered
    converted$Hospital.Name<-as.character(converted$Hospital.Name)
    converted$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia<-as.numeric(as.character(converted$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
    bestHosp<-converted[converted$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia==min(converted$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),c("Hospital.Name")]
    
    ## Return hospital name in that state with lowest 30-day death
    if (length(bestHosp) > 1) {
      bestHosp<-sort(bestHosp)
      bestHosp[1]
    } else
      bestHosp
  }
  
  ## Main
  ## Check that outcome is valid
  if (outcome == "heart attack"){
    ## Check that state is valid
    if (length(outcomeData[outcomeData$State == state,c("State")])>0){
      bestHospital <- inHeartAttack(state, outcomeData)
      print(bestHospital)
    } else{
      print(paste("Error in best(", state, ", ", outcome,") : invalid state", sep=""))
    }
  } else if (outcome == "heart failure"){
    ## Check that state is valid
    if (length(outcomeData[outcomeData$State == state,c("State")])>0){
      bestHospital <- inHeartFailure(state, outcomeData)
      print(bestHospital)
    } else{
      print(paste("Error in best(", state, ", ", outcome,") : invalid state", sep=""))
    }
  } else if (outcome == "pneumonia"){
    ## Check that state is valid
    if (length(outcomeData[outcomeData$State == state,c("State")])>0){
      bestHospital <- inPneumonia(state, outcomeData)
      print(bestHospital)
    } else{
      print(paste("Error in best(", state, ", ", outcome,") : invalid state", sep=""))
    }
  } else{
    print(paste("Error in best(", state, ", ", outcome,") : invalid outcome", sep=""))
  }
  ## Rate
  
}