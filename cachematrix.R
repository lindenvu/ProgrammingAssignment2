##  makeCacheMatrix creates a special "vector/list of functions"
##  cacheSolve takes the vector from above function, it gets the solved matrix if its already been cached, otherwise it calculates the value of the solved matrix and caches the value

#The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the solved matrix
#get the value of the solved matrix

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


#The following function calculates the solved matrix of the special "vector" created with the above function. 
#However, it first checks to see if the solved matrix has already been calculated. 
#If so, it gets the solved matrix from the cache and skips the computation. 
#Otherwise, it calculates the solved matrix of the data and sets the value of the solved matrix in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
