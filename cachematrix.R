## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      vInv <- NULL
      set <- function(y) {
            x <<- y
            vInv <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) vInv <<- solve
      getinverse <- function() vInv
      list(set = set,
           get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#    If the inverse has already been calculated (and the matrix has not changed), then the cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      vInv <- x$getinverse()
      if (!is.null(vInv)) {
            message("getting cached data")
            return(vInv)
      }
      data <- x$get()
      vInv <- solve(data, ...)
      x$setinverse(vInv)
      vInv
}

#> m = makeCacheMatrix(matrix(8:12, 2, 2))
#> m$get()
#      [,1] [,2]
#[1,]    8   10
#[2,]    9   11



#> cacheSolve(m)
#      [,1] [,2]
#[1,] -5.5    5
#[2,]  4.5   -4


