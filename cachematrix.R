

## This function creates a matrix 

makeCacheMatrix <- function(x = matrix()) { ## creates a function called makeCacheMatrix
    s <- NULL # initializes a variable "s" as NULL so that any value calculated previously is erased.
    set <- function(y){ # creates another function (within makeCacheMatrix function) which takes argument "y"
        x <<- y # sets "x" to "y" in the global function makeCacheMatrix and not confined to set function
        s <<- NULL # same as above
    }
    get <- function() x # creates another subfunction which doesn't take any argument and reports the value of x.
    setmatrix <- function(solve) s <<- solve # creates another subfunction which uses built-in function solve to inverse the matrix
    getmatrix <- function() s # creates anothe subfunction that doesn't take any argument and reports the value of s
    list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)

}


## This function inverses the matrix created by above function. 
## Before computation, this function checks if the inverse is already calculated 
## and stored in cache. If it is already calculated, it takes from the cache,
## otherwise it will compute the inverse of the matrix and sets the value of
## the inverse matrix in the cache through the setmean function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    s <- x$getmatrix()
    if(!is.null(s)){ # this subfunction checks if the inverse is already calculated
        message("getting cached data")
        return(s)
    }
    data_matrix <- x$get()
    s <- solve(data_matrix,...)
    x$setmatrix(s)
    s
}
