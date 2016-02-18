## Based upon the examples from the Programming Assignment 2
## Where that would return a "vector" object that could then create
## a cached mean value for the input vector
## e.g. a <- as.numeric(c(1:50))
##       b <- makeVector(a) returns a list of 4
##       cachemean(b) returns the mean value (25.5)

## As before the following creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInv <- function(solve) i <<- solve
    getInv <- function() i
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## Takes the list "matrix" created by makeCacheMatrix
## Returns the inverse matrix

cacheSolve <- function(x, ...) {
    i <- x$getInv()
    data <- x$get()
    i <- solve(data, ...)
    x$setInv(i)
    i
}


## Example : works
## a <- matrix(c(1,-1,1,2),2,2)
## b <- makeCacheMatrix(a)
## cacheSolve(b)
## [,1]       [,2]
## [1,] 0.6666667 -0.3333333
## [2,] 0.3333333  0.3333333

## Example : error
## a <- matrix(1:16,4,4)
## b <- makeCacheMatrix(a)
## cacheSolve(b)