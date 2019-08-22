## Assuming we want to inverse our matrix. We would want to save the inverse in a cache first
## and later read the cached matrix to get the inversed matrix 

## makeCacheMatrix will get an inverse of a preset matrix and save it in cache

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(solveMatrix) inv <<- solveMatrix
        getInverse <- function() inv
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve will read the cache inversed matrix created in makeCacheMatrix and return that matrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv      
}
