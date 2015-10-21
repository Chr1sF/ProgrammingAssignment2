## These two functions make use of the Superassign operator to assign
## a value to an object in an environment that is different from the current environment.
## The result is that the inversing function, called scope, is only called if it hasn't
## already been cached


## This function provides a list of functions which can get or set
## the cached inversed matrix. It uses the superassign operator to do ths.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    ##use the superassign for variables  
    x <<- y
    i <<- NULL
  } 
  get <- function() x
  setinv <- function(inv) i <<- inv  
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function delivers the inversed matrix. It does it efficiently by
## testing to see if the first function has already created a cached version
##If so then it just uses that. If not it performs the inverse then calls the
## setinverse to create a cached  version

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ##call getinverse of the makeCacheMatrix functio to see if cached
  i <- x$getinv()
  if(!is.null(i)) {
    ##A cache has been created
    message("getting cache of inversed matrix")
    return(i)
  }
  ##Inverse not cached. Get data and perform inverse
  data <- x$get()
  i <- solve(data, ...)
  ##Call the setinverse function of the makeCacheMatrix function
  x$setinv(i)
  i
}


