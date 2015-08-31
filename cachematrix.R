## These two functions will together create inverse matrix using caching

## The first function, makeCacheMatrix() will return list of functions, which will be used as an input of 
## for the latter function cacheSolve().
makeCacheMatrix <- function(x = matrix()) {
  # x: input matrix to be inversed
  # returns: list of functions for getting the inverse matrix
  cache <- NULL   #initialise value of the matrix inverse to NULL
  set <- function(y) {  #resetting the cache to null, and replacing x with new input
    x <<- y #assigning the new input, y to x
    cache <<- NULL  #setting the cache as null, because it is new input y
  }
  get <- function() x   #function for getting the matrix stored in the main function
  setinv <- function(inv) cache <<- inv   #function where cache value from cacheSolve() is replaced with new input, inv
  getinv <- function() cache   #function returning cached inverse matrix from cacheSolve()
  #passing functions for getting and setting inverse matrix into a list
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # x: output of function makeCacheMatrix()
  # returns: inverse of the original matrix
  
  cache <- x$getinv()
  if (!is.null(inv)) { #verify if there is a matrix stored in getinv, which is not null
    message("getting cached data . inverse matrix")
    return (cache)
  }
  data <- x$get() #get the matrix stored within makeCacheMatrix()
  cache <- solve(data,...) #get the inverse of the matrix
  x$setinv(cache) #store the inverse matrix within cache // object from makeCacheMatrix()
  return(cache)
}
