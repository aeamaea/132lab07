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
