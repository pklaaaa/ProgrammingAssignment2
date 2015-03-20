## These two functions below are used to create an object to store a matrix 
## and caches its inverse
## The function below creates a matrix and contains a list of functions

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## the function below first checks the inverse is calculated, if yes, return
## the result, if not, calculate it then return the result

cacheSolve <- function(x, ...) {
         i <- x$getinverse()
         if(!is.null(i)) {
                 message("getting cached data")
                 return(i)
         }
         data <- x$get()
         i <- solve(data,...)
         x$setinverse(i)
         i
        ## Return a matrix that is the inverse of 'x'
}