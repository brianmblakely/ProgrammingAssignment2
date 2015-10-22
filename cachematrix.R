

##  The two functions makeCacheMatrix and CacheSolve are used to create a special object that stores a numeric matrix and caches its inverse.

## Function Description: makeCacheMatrix

## The first function, makeCacheMatrix creates a special vector , which is really a list containing functions to do the following:
##	1. set the value of the matrix
##	2. get the value of the matrix
##	3. set the value of the inverse
##	4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
## x: the input needs to be a square invertible matrix  
    	i = NULL			 
	set = function(y) { 
                x <<- y
                i <<- NULL
        }      

	get = function() x
        setinverse = function(inverse) i <<- inverse
        getinverse = function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

## Function Description: cacheSolve
## The cacheSolve function calculates the inverse of the special matrix created with the makeCache function. 
## However, it first checks to see if the inverse has already been calculated. 
## If the inverse already exists, it gets the inverse from the cache and skips the computation. 
## If the inverse does not exist, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inverse = x$getinverse()
	 
	## if the inverse is already there, get it
	if (!is.null(inverse)) { 
		message ("Retrieving cached data") 		
		return(inverse)
	} 
	
	## else calculate the inverse
	matrix = x$get()
	inverse = solve(matrix, ...)
	x$setinverse(inverse)			
	return(inverse)
}					