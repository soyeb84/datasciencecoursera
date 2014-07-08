## The makeCacheMatrix function is stuctured the same way the makeVector was structured in the example
## It contains a list of functions, each function does tasks such as setting the matrix, getting the matrix, setting and getting the inverse. 
## to avoid redundant computation of the inverse of same matrix, we use another function cacheSolve to get the inverse if it is already calculated/

## makeCacheMatrix optionally takes argument a matrix, have function to set and get the matrix and also to get and set the inverse

makeCacheMatrix <- function(x = matrix()) {
	## i will be used to hold the inverse
	i <- NULL 
	
	## set() to assign values to the matrix
	set <- function(y){
    x <<- y 
    i <<- NULL
    
	}
	
	## getting the matrix x
	get <- function() x
		
	## setting the inverse of the matrix explicitly	
	setInverse <- function(inverse) i<<- inverse
	
	## gets the inverse of the matrix
	getInverse <- function() i
  
	## list of all the functions.
	list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
  
}


## cacheSolve can be used to check if the inverse is already computed, if yes then we will simply yield the cached value.

cacheSolve <- function(x, ...) {
	i <- x$getInverse()
	data <- x$get()
	if(!is.null(i))
	{
		
		message("getting cached result")
		return(i)
    
	}
	
	## the next line of code calculated the determinant of the matrix.
	data <- x$get()
	
	## if the determinant is zero, it means that the matrix is not invertible. This is just to avoid an exception.
	if(det(data)==0)
	{
		message("Sorry the matrix is not invertible")
		
	}
	else
	{
		i<- solve(data)
		x$setInverse(i)
		i
		
	}
}
