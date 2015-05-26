## CKME132 :: Lab 07 :: Amr Malik 
## 

## Task1 :: f1(x) :: Takes an argument x returns 3x+1
f1 <- function(x) { return (3 * x + 1)}

## Task2 :: f2(x) :: returns x/2 

f2 <- function(x) { return (x/2)}

## Task3 :: collatz(x) :: returns collatz values based on that conjecture
## if x is odd retrun 3x+1
## if x is even return x/2

collatz <- function(x) 
{
    if ( (x %% 2) == 0 ) {
        return(x/2)  ## return the even option
    }
    else {
        return(3 * x + 1) ## return the odd option
    }
    
} ## end collatz()


collatzIter <- function(startValue=10, numTimes=3) {
    if (startValue==10 && numTimes==3) 
       {
           print("Called with defaults. Start=10 Numtimes=3 ...")
       }
    
    retVect <- vector(mode="integer", length = 0) ## initvector
    retVect <- c(retVect,startValue) ## insert startValue
    
    ## We iterate numTimes - 1
    for (i in 1:numTimes-1) {
        retVect <- c(retVect,collatz(retVect[length(retVect)]))
    }
    
    return(retVect)
}

## Task5 :: iterate collatz function stop at 1 
T5_collatzIter1 <- function(startValue=10) {
 
    if (startValue==10 ) 
    {
        print("Called with defaults. Start=10 ...")
    }
    
    retVect <- vector(mode="integer", length = 0) ## initvector
    retVect <- c(retVect,startValue) ## insert startValue
    
    ## keep going until the collatz output reaches 1
    while ((retVect[length(retVect)] > 1)) {
        retVect <- c(retVect,collatz(retVect[length(retVect)]))
        ## if(retVect[length(retVect)] == 1) return(retVect)
    }
    
    return(retVect)
}

## Task6 :: for a startValue, return number of tries, orbit by iterating collatz function stop at 1 
T6_collatzIter <- function(startValue=10) {
    
    if (startValue==10 ) 
    {
        print("Called with defaults. Start=10 ...")
    }
    
    retVect <- vector(mode="integer", length = 0) ## initvector
    retVect <- c(retVect,startValue) ## insert startValue
    
    numTries <- 0
    
    ## keep going until the collatz output reaches 1
    while ((retVect[length(retVect)] > 1)) {
        numTries <- numTries + 1
        retVect <- c(retVect,collatz(retVect[length(retVect)]))
        ## if(retVect[length(retVect)] == 1) return(retVect)
    }
    
    ## return a list, first sublist is the number of tries (numTries)
    ## second has the orbit vector (numVect)
    return(list(numTries,retVect))
}

## Task7 :: for a startValue, return number of tries, orbit by iterating collatz function stop at 1 
Task7 <- function(inputVect=c(10,5,4)) {
    
    if (sum(inputVect==c(10,5,4)) == 3 ) 
    {
        print("Called with defaults. c(10,5,4) ...")
    }
    
    retVect <- vector(mode="integer", length = 0) ## initvector
    
    ## T6_collatzIter() returns a list, first sublist has the no. of tries
    ## we call T6_collatzIter() and index into the first sublist
    numTries <- T6_collatzIter(inputVect[1])[[1]]
    retVect <- c(retVect,numTries)) ## insert startValue
    
    
    
    ## keep going until the collatz output reaches 1

    for (i in 1:length(inputVect)-1) {
        numTries <- T6_collatzIter(inputVect[1])[[1]]
        retVect <- c(retVect,numTries)) ## insert startValue
    }
    
    ## construct data frame 

    return(data.frame(inputVect,retVect))
}