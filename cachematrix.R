# These codes are for the R Programming course programming assignment 2
# This function is used for caching the inverse of a matrix
# Refering to the examples from
#https://www.coursera.org/learn/r-programming/peer/tNy8H/programming-assignment-2-lexical-scoping

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverseMatrix <<- inverse
  getinverse <- function() inverseMatrix
  list(set=set, get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
}

#This function is used to calculate the inverse of a matrix

cacheSolve <- function(x, ...) {
  inverseMatrix <- x$getinverse()
  if(!is.null(inverseMatrix)) {
    message("getting cached data of matrix.")
    return(inverseMatrix)
  }
  data <- x$get()
  inverseMatrix <- solve(data) #  if x is a square invertible matrix, then solve(X) returns its inverse
  x$setinverse(inverseMatrix)
  inverseMatrix
}

## Experimental example
##> x <- matrix(1: 4, 2, 2)
##> m = makeCacheMatrix(x)

##> m$get()
##       [,1] [,2]
##[1,]    1    3
##[2,]    2    4

##> cacheSolve(m)
##       [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

##> x <- matrix(1: 4, 2, 2)

##> cached = makeCacheMatrix(x)

##> cached$get()
##       [,1] [,2]
##[1,]    1    3
##[2,]    2    4

##> cached$getinverse()
##       [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

##> cacheSolve(cached)
##      [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
