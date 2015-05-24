## This function creates a special matrix object that can cache its inverse.
## The returned special matrix is a list of 4 functions.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL  ## i is the inverse of the matrix  
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  
  ## return a list of 4 functions: set, get, setinverse, getinverse
}


## This function computes the inverse of the special matrix returned by makeCacheMatrix function. 
## If the inverse has already been calculated, then it retrieves the inverse from the cache. 

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     ## i is the inverse of the matrix
  
     i <- x$getinverse()
     if(!is.null(i)) {  #check if i has been calculated, i.e. not null 
          message("getting cached data")
          return(i)
     }
     
     ## not cached before, solve it and cache it
     
     data <- x$get()
     i <- solve(data, ...)
     x$setinverse(i)
     i
}
