## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  #initializing the inverse as null
  inverse <- NULL 
  
  #setting the matrix
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  #getting the matrix
  get <- function() {
    x
  }
  
  #setting the inverse
  setinversematrix <- function(inverse){
  inversematrix <<- inverse}

  #getting the inverse
  getinversematrix <- function(){
    inverse
  }
  
  #all defined functions
  list(set = set,
       get = get,
       setinversematrix = setinversematrix,
       getinversematrix = getinversematrix)
  }
  
  
}


## This function computes the inverse of the special "matrix" created by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  #getting cached inverse  
  inverse <- x$getinversematrix()
  
  #returing inverse if already cached 
  
  if (!is.null(inverse)){
  if ( identical( x$get() %*% inverse, inverse %*% x$get() ) ){
    print("getting cached data")
    return(inverse)
  }

  
  #calculating inverse if not already cached
  data <- x$get()  
  inverse <- solve(data, ...)  
  x$setinverse(inverse)  
  inverse  
 
  #returing a matrix that is the inverse of x 
  print("getting new computated data")
  return(inverse)
}
}
