## cache the matrix inverse so that matrix inverse is not caluclated every time
## This is done by calculating the inverse once and storing it another environment


## makes a matrix taking as input a matrix whose inverse is now cached
makeCacheMatrix <- function(x = matrix()) {
  # i is the inverse matrix
  i <- NULL
  
  # set a matrix
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  #get the original matrix
  get<-function() x
  #get the inverse
  getinverse <-function() i
  #set the inverse
  setinverse <- function(inv) i <<- inv
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## gets the matrix inverse from the cache if present, 
## else calculates the inverse, caches it and returns the value
cacheSolve <- function(x, ...) {
  i<-x$getinverse()
  if(!is.null(i)){
    # if inverse is cache return it
    message("getting cached data")
    return(i)
  }
  #else get orignal matrix
  data <- x$get()
  #solve the inverse
  i <- solve(data, ...)
  #cache the inverse
  x$setinverse(i)
  #return the inverse
  i
}

