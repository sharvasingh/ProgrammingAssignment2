## The functions written below are written keeping the same template as the original
## assignment to display the usefulness of lexical scoping in r

## makeCacheMatrix is used used to create the matrix that can cache its inverse.

makeCacheMatrix <- function(x = numeric()) {  ##setting the value of the matrix
  m <- NULL
  set <- function(y) { ##new variable which will solve the matrix later 
    x <<- y            ##x is assigned to y -not in the environment of the function makeCacheMatrix
    m <<- NULL
  }
  get <- function() x ## the function gets the value of the inverse matrix
  setsolve <- function(solve) m <<- solve ##matrix inversion solve function is assigned to m not in the env of the function
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function checks if the inverse has already been calculated or not.
##Depending on the outcome of the above the inverse is returned from either the cache memory or calculated.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()     ##Assigning the getsolve from the previous function to m
  ##it can be assigned outside its function because it was not assigned in the function environment  
  
  if(!is.null(m)) {    ##Checking to see if the matrix inverse has already been calculated
    message("getting cached data")
    return(m) ## if inverse has already been calculated the value m from previous function is returned
  }
  data <- x$get()
  m <- solve(data) ##if NULL value is returned in the previous line inverse is calculated
  x$setsolve(m)
  m ##output given
}