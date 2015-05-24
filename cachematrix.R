## It contains two functions, one that creates an object that stores a matrix and 
## its inverse and another function that takes this object, 
## and calculates the inverse matrix, if it does not exist. 
## The objective is to store the inverse of a matrix in a special object, 
## so that if necessary, the inverse of vaor be obtained without the need 
## for recalculation, saving computational time.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(w) s <<- w
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Receives a parameter related to an matrix, calculates its inverse if 
## it does not already have and return inverse. Where the matrix has an inverse already calculated, 
## the inverse stored is returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
     m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setsolve(m)
        m		
}
