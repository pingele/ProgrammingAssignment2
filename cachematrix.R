## Assignment for Week 3.  Looking at Lexical Scoping
## Objective is to write 2 functions that create a matrix and store it, then inverse 
## the matrix, but checking for its precense of a cached matrix first, before creating one.

## The first function will create an invertible matrix, get the inverse and store the inverse for later use.

makeCacheMatrix <- function(x = matrix()) { ## setting the argument with mode of matrix
    inv <- NULL                             ## establishing variable to hold the inverse of the matrix
    set <- function(y){                     ## giving the matrix in the parent environment a value
        x <<- y                             
        inv <<- NULL
    }
    get <- function() x                   ## returning the matrix value
    setInverse <- function(solveMatrix) inv <<- solveMatrix ## establishing the inverse value
    getInverse <- function() inv            ##  calls or gets the inverse value
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)  ## making the functions available to be called later
}


## determining the inverse of the matrix created in the MakeCacheMatrix function.
## If the inverse is in the cache, then use that value, otherwise get the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()          ## populate the inv variable with the value from the previous function,.
    if(!is.null(inv)){             ## if the variable is not null then retrieve the cache value.
        message("getting cached data")
        return(inv)
    }
    data <- x$get()             ## If the inv variable is null, generate the inverse and set to inv variable.
    inv <- solve(data)
    x$setInverse(inv)
    inv
}
