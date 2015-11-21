##Function to take an inverse of a matrix and to cache the inverse so that there is no need 
##of computing it repeatedly



##This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix(NULL,2,2)) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##This function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve the
##inverse from the cache.


cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}



##TEST
a <- makeCacheMatrix(matrix(c(9,2,3,4,5,6,7,8,9), 3,3))
a$get()
cacheSolve(a)