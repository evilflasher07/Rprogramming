## This function creates a special "matrix" object.
## class: list contaning functions
## 1) set the value of matrix
## 2) get the value of matrix
## 3) set the inverse of a matrix
## 4) get the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
	  
	#initialize the value of inverse
	 	inverse <- NULL

	#set the value of matrix
        	set <- function(y) {
                x <<- y
                inverse <<- NULL
        	}

	#get the value of the matrix
        	get <- function() x

	#set the inverse of the matrix
        	setInverse <- function(inv) inverse <<- inv

	#get the inverse of the matrix
        	getInverse <- function() inverse

	#return a list of functions in makeCacheMatrix
        	list(set = set, get = get,
             	setInverse = setInverse,
             	getInverse = getInverse)
	
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'

	#check inverse if cached
    		inverse <- x$getInverse()
    		if(!is.null(inverse)) {
        	message("getting cached data")
        	return(inverse)
    		}
	# get the matrix into data
    		data <- x$get()

	# compute inverse
    		inverse <- solve(data, ...)
	# cache inverse
    		x$setInverse(inverse)
	# return the value of inverse
    		inverse
}
