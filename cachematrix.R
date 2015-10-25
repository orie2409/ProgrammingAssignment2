## The two functions below allow the inverse of a function to be cached, so that it can
## be recalled for future use, and so avoids repeating the calculation.

## makeCacheMatrix is a function which takes a matrix as an input and returns a list
## of 4 functions which set/get the input matrix/inverse of input matrix.

makeCacheMatrix <- function(x = matrix()) {
i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse # Does NOT calculate inverse, just stores input to variable i
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## cacheSolve return the inverse of a matrix. It first checks if it has already been
## calculated and returns the cached value; else it performs the calculation via Solve()

cacheSolve <- function(x, ...) { 			## Input will be the output list from makeCacheMatrix
      	  
	      i <- x$getinverse()
            if(!is.null(i)) {
                message("getting cached data")  
                return(i)
             }
             data <- x$get()				 
             i <- solve(data, ...)			
             x$setinverse(i)				
             i			   
}
