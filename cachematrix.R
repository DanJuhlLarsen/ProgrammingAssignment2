## The combination of the two functions can take a matrix and calculate the inverse matrix, or
## if it has already been calculated, then it can get the inverted matrix from the cache.

## The function create a list with a function to set/get the values of the matrix and the 
## value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(Y) {
    x <<- Y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function returns the inverse matrix from above. If it has already been calculated,
## it will return the result from the cache. If it has not been calculated, then it 
## calculates the inverse, and adds the result to the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("Cached data.")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
  ## Return a matrix that is the inverse of 'x'
}

##First we get the inverse matrix, no cache.. Second time the resulte is received from the cache.

## Run the functions.
##> x <- diag(4,2)
##> x
##[,1] [,2]
##[1,]    4    0
##[2,]    0    4
##> CacheMatrix <- makeCacheMatrix(x)
##> cacheSolve(CacheMatrix)
##[,1] [,2]
##[1,] 0.25 0.00
##[2,] 0.00 0.25
##> cacheSolve(CacheMatrix)
##Cached data.
##[,1] [,2]
##[1,] 0.25 0.00
##[2,] 0.00 0.25
> 