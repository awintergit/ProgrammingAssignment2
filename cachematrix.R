## This program computes the inverse of a matrix and stores it in cache.

## The function named makeCacheMatrix takes a matrix for input, 
## defines 4 functions inside of it
## and returns a list, where each element is one of the four functions.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function(){
                x
        }
        
        setInverse <- function(inverse){
                inv <<- inverse
        }
        getInverse <- function() {
                inv
        }
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The function named cacheSolve takes a list
## returned from the above function, makeCacheMatrix,
## and outputs the inverse of the matrix created above.  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
