## Put comments here that give an overall description of what your
## functions do

## makeCache creates a list containing a function to 
##	1. set the value of the matrix
##	2. get the value of the matrix
##	2. set the value of the inverse
##	4. get the value of the inverse	

makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL
		set <- function(y){
			x <<- y
			inv <<- NULL
		}
		get <- function() x
		setinverse <- function(inverse) inv <<- inverse
		getinverse <- function() inv
		list(set = set, get = get,
			 setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve function calculates the inverse of the matrix created 
## with the above function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
        	message("getting cached data")
        	return(inv)
        ## first checks to see if the inverse has already been 
        ## calculated.If so, it get the inverse from the cache
        ## and skips computation.
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinverse(inv)
        inv
        ## calculates the inverse of the matrix and sets the value of
        ## the inverse in the cache via setinverse function
}
