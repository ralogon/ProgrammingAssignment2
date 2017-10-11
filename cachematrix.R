# The functions makeCacheMatrix and cacheSolve are going to be defined.

# makeCacheMatrix returns a list of functions which performs the following tasks
      # 1- Set value of the matrix
      # 2- Get the value of the matrix
      # 3- Set the value of the inverse
      # 4- Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
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
}


## cacheSolve calculates the inverse of the special 'matrix' if it was not
#  calculated previously

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      i <- x$getinverse()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setmean(i)
      i
}
