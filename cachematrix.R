## This function creates a special "matrix" object that can cache its inverse.

## The below function creates a special "matrix", which is really a list containing a function to
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the inverse of the matrix
## 4.get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      inversemtrx <- NULL
      
      ## 1.set the value of the matrix
      set <- function(y) {
            x <<- y
            inversemtrx <<- NULL
      }
      
      ## 2.get the value of the matrix
      get <- function() x
      
      ## 3.set the inverse of the matrix
      setinversemtrx <- function(matrix) inversemtrx <<- matrix
      
      ## 4.get the inverse of the matrix
      getinversemtrx <- function() inversemtrx
      
      ## list all the inner functions
      list(set = set, get = get,
           setinversemtrx = setinversemtrx,
           getinversemtrx = getinversemtrx)
}

##-----------------------------------------------------------------------------------------------------------

## This belwo function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
      inversemtrx <- x$getinversemtrx()
      
      ## If the inverse has already been calculated (and the matrix has not changed),
      ## then the cachesolve should retrieve the inverse from the cache.
      if(!is.null(inversemtrx)) {
            message("getting cached Matrix")
            return(inversemtrx)
      }
      
      ## if not then, directly compute the inverse from the input data
      data <- x$get()
      inversemtrx <- solve(data, ...)
      x$setinversemtrx(inversemtrx)
      inversemtrx
      ## Return the inverse matrix of 'x'
}

# proviede a test matrix: T<- matrix(c(3,1,2,-1,0,1,4,0,-5),3,3)
# it's inverse matrix is as below:
#       [,1] [,2] [,3]
# [1,]    0    1    0
# [2,]   -5   23   -4
# [3,]   -1    5   -1
