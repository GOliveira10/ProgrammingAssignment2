## These functions relate to matrix inversion. The first function inverts a given matrix
## and caches the result:

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
set <- function(y){
  x<<-y
  m<<-NULL
}
get <- function() x
setmatrix <- function(solve) m<<- solve
getmatrix <- function() m
list(set = set, get = get,
     setmatrix = setmatrix,
     getmatrix = getmatrix)
}


## The second function checks to see if there is a matrix inversion already cached
## and if so returns it

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}

