## Below are two functions that are used to create a special object that stores a matrix and cache's its inverse
## It is assumed for these functions that the matrix being stored is invertible.

## The following function takes a matrix as an argument and creates a special version of that matrix represented by
## a list of four functions. The four functions set the value of this special matrix, retrieve the value of the
## matrix, set the value of the matrix's inverse, and retrieve the value of the matrix's inverse, respectively.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y){
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) inv <<- inverse
      getinverse <- function() inv
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The following function takes as an argument a special matrix create via the makeCacheMatrix function.
## It computes the inverse of this special matrix. It first checks to see if the inverse has already been 
## computed. If so, the inverse is retrieved from the cache and computation is skipped. Otherwise, it 
## computes the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
      inv <- x$getinverse()
      if(!is.null(inv)){
            message("getting cached matrix")
            return(inv)
      }
      matrix <- x$get()
      inv <- solve(matrix, ...)
      x$setinverse(inv)
      inv
}
