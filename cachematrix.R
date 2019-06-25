## HOW TO DO ##

## Step 1: create new var with makeCacheMatrix, e.g.
## m1 <- makeCacheMatrix('YOUR MATRIX')
## Step 2: cacheSolve(m1)

## creates special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(matrix){
            # sets the matrix
            x <<- matrix
            i <<- NULL
      }
      
      get <- function() x
      # gets the matrix 
      
      setinverse <- function(inverse) i <<- inverse
      #  sets the inverse of matrix
      
      getinverse <- function() i
      # gets the inverse
      
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
      # returns list of methods
}


## if inverse calculated before, retrieves inverse from cache

cacheSolve <- function(x, ...) {
      i <- x$getinverse()
      # returns inverse of x
      
      if(!is.null(i)){
            # if inverse of x set, returns inverse of x 
            message('getting cached data')
            return(i)
      }
      data <- x$get()
      # get x
      
      i <- solve(data,...)
      # calculate inverse of data
      
      x$setinverse(i)
      # set inverse to object
      
      i
      # return object
}