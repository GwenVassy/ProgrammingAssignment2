## The two functions below used together allow the result of Inverse operation 
## to be stored in a Cache and accessed to avoid repeating the calculation
## The correct way to use these functions is
## 1) Define a matrix m
## 2) Run c <- makeCacheMatrix(m)
## 3) Run cacheSolve(c) as many times as required

## This function creates a list containing a function to 
## set the value of the Matrix, 
## get the value of the Matrix
## set the value of the Inverse
## get the value of the Inverse

makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL
        setMatrix <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        getMatrix <- function() x
        setInverse <- function(solve) i <<- solve
        getInverse <- function() i
        
        list(setMatrix = setMatrix, getMatrix = getMatrix, 
             setInverse = setInverse, getInverse = getInverse)
}


## This function first checks whether the cache contains the inverse of x with "getInverse"
## then if yes (!is.null) returns it, 
## if not then calculates (solve) and returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$getMatrix()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}
