## 132 :: Lab 07 :: aeamaea
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

## Task 4 :: iterate collatz() with a startval and how many times to iterate
collatzIter <- function(startValue=10, numTimes=3) {
    if (startValue==10 && numTimes==3) 
       {
           print("collatzIter::Called with defaults. Start=10 Numtimes=3 ...")
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
        print("T5::Called with defaults. Start=10 ...")
    }
    
    retVect <- vector(mode="integer", length = 0) ## initvector
    retVect <- c(retVect,startValue) ## insert startValue
    
    ## keep going until the collatz output reaches 1
    while ((retVect[length(retVect)] > 1)) {
        retVect <- c(retVect,collatz(retVect[length(retVect)]))
    }    
    return(retVect)
}

## Task6 :: for a startValue, return number of tries, orbit by iterating collatz function stop at 1 
T6_collatzIter <- function(startValue=10) {
    
    if (startValue==10 ) 
    {
        print("T6::Called with defaults. Start=10 ...")
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
        print("T7::Called with defaults. c(10,5,4) ...")
    }
    
    retVect <- vector(mode="integer", length = 0) ## initvector
    
    ## keep going until the collatz values in inputVect are exhausted

      for (i in 1:(length(inputVect))) {
        ## T6_collatzIter() returns a list, first sublist has the no. of tries
        ## To et i'th element in inputVect subset output of T6 to get first int val
        ## we call T6_collatzIter() and index into the first sublist
        
        numTries <- T6_collatzIter(inputVect[i])[[1]][1]
        retVect <- c(retVect,numTries) ## insert startValue
      }
      
    ## construct data frame 
    return(data.frame(inputVect,retVect))
}

## Task8 :: use task5 to create an fn(x) which returns the max(fn(x))

T8_maxValInOrbit <- function(x) {
  return(max(T5_collatzIter1(x)))
  
}

## Task9 :: 
##      takes a numeric vector as an argument, and returns a data frame with three columns:
##      the original number, the largest element of that number’s trajectory, 
##      and the number of iterations it takes to reach 1
#
## Created by modifying Task7()
# 
# > T9(1:7)
#      number maxinorbit iterations
# 1      1          1          0
# 2      2          2          1
# 3      3         16          7
# 4      4          4          2
# 5      5         16          5
# 6      6         16          8
# 7      7         52         16

T9 <- function(inputVect=c(99,88,77)) {
    
    orbitVect <- vector(mode="integer", length = 0)   ## initvector
    numTriesVect <- vector(mode="integer", length=0)
    maxOrbit <- 0                                   ## holding var for maxOrbit val
    
    ## keep going until the collatz values in inputVect are exhausted
    
    for (i in 1:(length(inputVect))) {
        ## T6_collatzIter() returns a list, first sublist has the no. of tries
        ## To et i'th element in inputVect subset output of T6 to get first int val
        ## we call T6_collatzIter() and index into the first sublist
        
        numTries <- T6_collatzIter(inputVect[i])[[1]][1]
        numTriesVect <- c(numTriesVect,numTries) ## insert startValue
        
        ## T8_maxValInOrbit() returns max value in orbit based on input int
        ## We're passing the current value in the inputVect to T8_maxValInOrbit()
        maxOrbit <- T8_maxValInOrbit(inputVect[i])
        orbitVect <- c(orbitVect,maxOrbit)
    }
    
    ## construct data frame 
    retDF <- data.frame(inputVect,orbitVect,numTriesVect)
    colnames(retDF) <- c("number","maxinorbit", "iterations")
    return(retDF)
}

## T10() -- calls T9 gets top/bottom 5 entries after sorting the data frame on iterations
##          low to high.
# 
# >  T10(1:100)
#      number maxinorbit iterations
# 1       1          1          0
# 2       2          2          1
# 4       4          4          2
# 8       8          8          3
# 16     16         16          4
# 27     27       9232        111
# 54     54       9232        112
# 55     55       9232        112
# 73     73       9232        115
# 97     97       9232        118

T10 <- function(inputVect) {
    
    yy <- T9(inputVect) ## returns DF w/ col names c("number","maxinorbit", "iterations")
    yy <- yy[order(yy$iterations),] ## order the DF on the iterations columns
    
    if(nrow(yy) < 10) {
        return(yy)
    } else {
        ## you could also do zz[c(1:5),(nrow(zz)-4):nrow(zz),] to get 1st/last 5 rows
        ## but head/tail solution below is more elegant. (Credit: Dr. Jensen)
        return(rbind(head(yy,5),tail(yy,5)))
    }
    
} ## End T10

## T11() -- do what T9 does, but also count the number of odd nums in the trajectory
# > T11(c(1:10,500,501,100000,100001))
# number maxinorbit iterations oddnums
# 1       1          1          0       1
# 2       2          2          1       1
# 3       3         16          7       3
# 4       4          4          2       1
# 5       5         16          5       2
# 6       6         16          8       3
# 7       7         52         16       6
# 8       8          8          3       1
# 9       9         52         19       7
# 10     10         16          6       2
# 11    500       9232        110      40
# 12    501       9232        110      40
# 13 100000     100000        128      44
# 14 100001     300004         89      29

T11 <- function(inputVect) {
    
    orbitVect <- vector(mode="integer", length = 0)   ## initvector
    numTriesVect <- vector(mode="integer", length=0)
    oddNumsVect <- vector(mode="integer", length=0)
    maxOrbit <- 0                                   ## holding var for maxOrbit val
    
    ## keep going until the collatz values in inputVect are exhausted
    
    for (i in 1:(length(inputVect))) {
        ## T6_collatzIter() returns a list, first sublist has the no. of tries
        ## To et i'th element in inputVect subset output of T6 to get first int val
        ## we call T6_collatzIter() and index into the first sublist
        
        numTries <- T6_collatzIter(inputVect[i])[[1]][1]
        # output of T6, get sublist that is the vector cotaining values, mod each with 2, check if result 1 (is odd)
        # sum that vector of logicals and you have number of odd entries
        
        oddNums  <- sum(T6_collatzIter(inputVect[i])[[2]] %% 2 == 1)
        numTriesVect <- c(numTriesVect,numTries) ## insert startValue
        
        ## T8_maxValInOrbit() returns max value in orbit based on input int
        ## We're passing the current value in the inputVect to T8_maxValInOrbit()
        maxOrbit <- T8_maxValInOrbit(inputVect[i])
        orbitVect <- c(orbitVect,maxOrbit)
        oddNumsVect <- c(oddNumsVect,oddNums)
    }
    
    ## construct data frame 
    retDF <- data.frame(inputVect,orbitVect,numTriesVect,oddNumsVect)
    colnames(retDF) <- c("number","maxinorbit", "iterations","oddnums")
    return(retDF)
} ## End T11()