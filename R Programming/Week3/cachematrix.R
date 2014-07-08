makeCacheMatrix <- function (x=matrix())
{
  i <- NULL
  set <- function(y){
    x <<- y 
    i <<- NULL
    
  }
  get <- function() x
    
  setInverse <- function(inverse) i<<- inverse
  getInverse <- function() i
  
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
  
}

cacheSolve <- function(x,...)
{
  i <- x$getInverse()
  data <- x$get()
  if(!is.null(i))
  {
    
    message("getting cached result")
    return(i)
    
  }
  data <- x$get()
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