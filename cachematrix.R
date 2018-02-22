# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(p = matrix()) {
  q <- NULL
  set <- function(r) {
    p <<- r
    q <<- NULL
  }
  get <- function() p
  set_solve <- function(solve) q <<- solve # calculating inverse
  get_solve <- function() q
  list(set = set, get = get, set_solve = set_solve, get_solve = get_solve)
}

# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(p, ...) {
  q <- p$get_solve()
  if(!is.null(q)) {
    message("getting cached data")
    return(q)
  }
  data <- p$get()
  q <- solve(data, ...)
  p$set_solve(q)
  q
}

# Sample output

#> m = matrix(c(2,4,6,8),nrow=2,ncol=2)
#> m1 = makeCacheMatrix(m)
#> cacheSolve(m1)
#     [,1]  [,2]
# [1,] -1.0  0.75
# [2,]  0.5 -0.25
#> cacheSolve(m1)
# getting cached data
#     [,1]  [,2]
# [1,] -1.0  0.75
# [2,]  0.5 -0.25
