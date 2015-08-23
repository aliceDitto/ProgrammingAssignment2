## These two functions will together create inverse matrix using caching
## The first function, makeCacheMatrix() will return list, which will be used as an input
## for the latter function cacheSolve(). More comments on the functions can be found in the functions themselves

makeCacheMatrix <- function(x = matrix()) {
  # x: inversible matrix
  # returns: list containing functions to:
  # 1. get the matrix
  # 2. set the matrix
  # 3. get the inverse matrix
  # 4. set the inverse matrix
  #the list is then used as input to function cacheSolve()
  
  inv <- NULL   #initialise value of the matrix inverse to NULL
  set <- function(y) {  #function to cache
    x <<- y
    inv <<- NULL
  }
  get <- function() x   #getting value of the inverse
  setinv <- function(solve) inv <<- solve   #calculating the inverse of matrix  using function solve()
  getinv <- function() inv   #getting the inverse matrix
  #passing the value of the function to list
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # x: output of function makeCacheMatrix()
  # returns: inverse of the original matrix
  
  inv <- x$getinv()
  if (!is.null(inv)) { #if inverse has been cached, then return the inverse matrix
    message("getting cached data . inverse matrix")
    return (inv)
  }
  #if inverse not cached, calculate , cache and return the inverse matrix
  data <- x$get()
  inv <- solve(data,...)
  x$setinv(inv)
  return(inv)
}