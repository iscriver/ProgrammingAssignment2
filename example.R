
# This first function "makes" a vector for you. Sort of. It actually takes a 
# vector you give it and makes it a more convenient (albeit more complicated) 
# object that will work for this situation. What it gives you is a 
# list where the input vector is stored as a function under the name 'get'. 
# So, if you want to 'make' this special cached vector, then you write it like:
# v <- makeVector(yourVectorHere)
# ie: 
# v <- makeVector(1:100)

# Then, 'v$get()' will return the value you expect of your vector (in this case 
# '1:100') 

makeVector <- function(x = numeric()) {
      # Define a variable 'm', and give 'm' the value 'NULL' in the environment 
      # of the function 'makeVector'
      m <- NULL
      
      # 'set' is a function that takes a value y, and then gives that value 'y' 
      # to a variable 'x' which is already defined in the parent environment 
      # of 'makeVector'. 
      # It also reassigns 'm' in the parent environment (makeVector's 
      # environment) to 'NULL'. It actually is never used, so don't worry about it.
      set <- function(y) {          
            x <<- y
            m <<- NULL
      }
      
      # 'get' is a function that simply returns back the value of x, which is 
      # not defined in the 'get' environment, so it searches the parent 
      # environment, which is the one from 'makeVector'. 
      # There, it finds whatever you passed to 'makeVector'. 
      
      get <- function() x
      
      # 'setmean' is a function that takes a value 'mean' and gives that value 
      # to a parent or ancestor enviornment
      setmean <- function(mean) m <<- mean
      
      # 'getmean' is a function that takes no arguments, but returns the value 
      # 'm', which it will not find in it's environment so it will search for it
      # in a parent environment. 
      getmean <- function() m
      
      # then we ask 'makeVector' to return a list where the first element is the 
      # function 'set' and is called "set", the second element is the function 
      # 'get' and is called "set", etc... 
      list(set = set, get = get,
           setmean = setmean,
           getmean = getmean)
}

# cachemean is a function that will return the mean of the "special" vector you 
# pass it (which is actually a list). So you call it like this: 
# cachemean(v)
# where previously you had run 
# v <- makeVector(yourVectorHere)

cachemean <- function(x, ...) {
      
      # Define a variable m, this time in the 'cachemean' environment, and 
      # assign it to be the value of the mean stored (also as 'm') in the 
      # 'makeVector' environment. 
      # To do this we pull up the cached value of m using the 'getmean' element 
      # in the list of x. If we haven't run cachemean yet, then we will get the
      # value "NULL". 
      m <- x$getmean()
      
      # Check to see if 'm' is 'NULL'. If 'm' is not 'NULL', then print a 
      # message and then tell 'cachemean' to return a value 'm', which is cached
      if(!is.null(m)) {
            message("getting cached data")
            return(m) # a quick cut-out from 'cachemean' function
      }
      
      # If R runs to this point of the 'cachemean' function, it is because 
      # 'cachemean' did not find a value in the variable 'm' (it was NULL).
      # So we will need to calculate it ourselves. 
      
      # Create a variable 'data' and set it to be the vector you are supposedly 
      # interested in. 
      # Recall that x$get() returns the value of the vector; the function 'get' 
      # is defined in the environment of the makeVector function.
      data <- x$get()
      
      # Next we create a variable 'm' that gets the mean of the data variable, the 
      # normal way, buy using the 'mean' function. This will supposedly be slow
      # but since you will be only doing this once in a while, that's fine. 
      # Once you can do it once, you can just retrieve the cached version (more 
      # on this in a moment) 
      # If you want to pass mean other arguments, you can through the use of '...'
      m <- mean(data, ...)
      # Now that you have the mean calculated, before we exit from the
      # 'cachemean' function, we want to cache the mean value somewhere.  
      # Thankfully, we can do that, by using the 'setmean' function, which will 
      # put the mean in memory as a part of the list. 
      x$setmean(m)
      # Return the value of the mean. 
      m
}