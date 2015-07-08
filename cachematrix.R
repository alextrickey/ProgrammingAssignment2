## Cache Functions for the Matrix Inverse
##
## Purpose:
##     Enhances the efficiency of the computation of the matrix inverse within
##     loops (when the data matrix does not change from one iteration to  
##     the next) by storing the previously computed value in a cache. 
##
## Contents: 
##     1. makeCacheMatrix(x): A function to create a "special matrix object" 
##          together with list of functions that allow the matrix inverse 
##          to be stored in a cache. 
##     2. cacheSolve(x): A function to calculate the matrix inverse and/or 
##          retrieve it from the cache if it has already been calculated. 
##
## Usuage:
##     1. First run makeCacheMatrix (outside of the loop) to initialize the 
##        "special vector" of functions:
##            E.g. x = makeCacheMatrix(x)
##     2. Then use the set() function (and attribute) to specify the  
##        matrix to be inverted (outside of the loop):
##           E.g. x$set(MatrixToBeInverted)
##     3. Finally, (within the loop) call cacheSolve to obtain the inverse:
##           E.g. cacheSolve(x)


## This function creates a "special matrix object" that can store cached 
##    values of the matrix inverse and a list of functions that can interact 
##    with the cache
makeCacheMatrix <- function(x = matrix()) {

    #Set the initial value of the inverse to NULL
    MatInv <- NULL

    #This function sets the data matrix and initialzies the inverse as null 
    set <- function(y) {
        x <<- y
        MatInv <<- NULL
    }
  
    #This function gets the data matrix to be inverted
    get <- function() x
  
    #This function sets the cached inverse to a value
    setminv <- function(minv) MatInv <<- minv

    #This function gets the cached value of the inverse
    getminv <- function() MatInv
    
    #Return the above functions a list
    list(set = set, 
         get = get,
         setminv = setminv,
         getminv = getminv)
}


## This function computes the matrix inverse if it has not yet been computed 
##    and stores the value of the inverse in the cache
cacheSolve <- function(x, ...) {

    #Retrieves the cached value of the matrix inverse
    MatInv <- x$getminv()
    
    #Returns the cached inverse if it was previously computed and stored
    if(!is.null(MatInv)) {
        message("getting cached data")
        return(MatInv)
    }
    
    #Otherwise retrieve the data and calculates the matrix inverse
    data <- x$get()
    MatInv <- solve(data, ...)
    
    #Save the value of the matrix inverse to the cache
    x$setminv(MatInv)
    
    #Return the matrix inverse
    MatInv
}

