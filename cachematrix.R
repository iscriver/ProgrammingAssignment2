## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      
      ## This will be my inverted matrix. Set to NULL until we need to calculate 
      # it and then it will be cached in inverted. 
      inverted <- NULL
      
      get <- function() {x} 
      setinv <- function(inv) {inverted <<- inv}
      getinv <- function() {inverted}
      
      list(get = get, setinv = setinv, getinv = getinv)
      
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      
      invertedMatrix <- x$getinv()
      if(!is.null(invertedMatrix)){
            message("Getting cached inverse")
            return(invertedMatrix)
      }
           
      mat <- x$get()
      inv <- solve(mat)
      x$setinv(inv)
      inv
      
      
}
