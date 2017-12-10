## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(a = matrix()) {
  i <- NULL
  set <- function(b) {
    a <<- b
     i <<- NULL
  }
  get <- function() a
  setSolve <- function() i <<- solve(a) #calculate the inverse
  getSolve <- function() i
  list(set = set,
       get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}

## Write a short comment describing this function

cacheSolve <- function(a, ...) {
        ## Return a matrix that is the inverse of 'a'
        i <- a$getSolve()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        mat <- x$get()
        i <- solve(mat, ...)
        a$setSolve(i)
        i
}
