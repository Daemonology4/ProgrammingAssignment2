##below are two functions that, between them calculate an inverse of a matrix
##provided by the user. It will cache the inverse and recall it instead of recalculating.


##initial code to make an inverse of a matrix with solve() and output a 
##list with 4 functions

makeCacheMatrix<- function(x = matrix()) {
  m <- NULL
  #sets x to the value of it's stored inverse
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #gets x by being called
  get <- function() x
  #sets value of inverse
  setinv <- function(x) m <<- solve(x)
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


#code for working with the above function to work with inverse
#via the list output of the makeCacheMatrix function. TAKES makeCacheMatrix(x) AS ARGUMENT!
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  #if there is a stored list in Global it is retrieved
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  #if there is not a stored list in Global environment calc inverse
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}