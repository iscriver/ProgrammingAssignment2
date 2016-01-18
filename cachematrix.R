## This file is my submission for R Progamming Assignment 2

## These functions serve to both cache a matrix 'x' and then, if necessary, when 
## we need to calculate the inverse of 'x' we first check to see if it is 
## already cached. If it is, we return the cached value, otherwise, we 
## calculate the inverse, cache it, and then return it. 

## 'makeCacheMatrix' takes in a matrix 'x' that is invertible, and then creates
## a list of functions which serve to cache the value of 'x', and cache the 
## value of it's inverse, once we calculate it.  

makeCacheMatrix <- function(x = matrix()) {
      
      ## This will be my inverted matrix. Set to NULL until we need to calculate 
      # it and then it will be cached in inverted. 
      inverted <- NULL
      
      get <- function() {x} 
      setinv <- function(inv) {inverted <<- inv}
      getinv <- function() {inverted}
      
      list(get = get, setinv = setinv, getinv = getinv)
      
}


## This function first checks to see if the inverse of a matrix 'x' is already 
## calculated and cached, and if it is, it just returns with that inverse. 
## Otherwise, it calculates the inverse, caches it using the function defined as 
## the 'setinv' element of 'x', and then returns the inverse to the user. 

cacheSolve <- function(x, ...) {
      # Create a variable that gets the value stored in 'x'
      invertedMatrix <- x$getinv()
      # Check to see if that value is 'NULL'
      if(!is.null(invertedMatrix)){
            message("Getting cached inverse")
            return(invertedMatrix)
      }
      # If it was null, continue on here to get the value of 'x'
      mat <- x$get()
      # Calculate the inverse of the matrix 
      inv <- solve(mat, ...)
      # Cache the newly calculate inverse
      x$setinv(inv)
      # Return the inverse to the user
      inv
}
