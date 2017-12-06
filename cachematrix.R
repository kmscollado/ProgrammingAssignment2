## The following functions cache the inverse of a matrix.

## The first function creates a special "matrix" 
## object that can cache its inverse.

## The second function computes the inverse of the
## special "matrix" returned by makeCacheMatrix above.

## Create a special "matrix"

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y){
          x <<- y
          inv <<- NULL
      }
      get <- function() x
      setinv <- function(solve) inv <<- solve
      getinv <- function() inv
      list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Compute the inverse of the special "matrix"

cacheSolve <- function(x, ...) {
      cm <- makeCacheMatrix(x)
      inv <- cm$getinv()
      if(!is.null(inv)){
          message("getting cached data")
          return(inv)
      }
      data <- cm$get()
      inv <- solve(data, ...)
      cm$setinv(inv)
      inv
}

