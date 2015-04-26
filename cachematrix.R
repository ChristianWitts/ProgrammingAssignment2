## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it 
## repeatedly. Because of this, we have 2 functions that help us with 
## computation by caching the inverted matrix, using the 
## `[set|get]inverse` functions contained in `makeCacheMatrix`


## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverted matrix
## 4. Get the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() {
    x
  }
  setinverse <- function(inverted) {
    inverse <<- inverted
  }
  getinverse <- function() {
    inverse
  }
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## With the assumption that the matrix is always invertible, the cacheSolve
## function will first check if the inverted matrix has already been computed
## by calling the `getinverse` function and if so, it will return those results
## else, it will compute the inverted matrix, and set the value with
## `setinverse` before returning the inverted matrix.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
