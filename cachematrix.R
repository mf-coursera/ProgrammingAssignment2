## Put comments here that give an overall description of what your
## functions do

#The makeCacheMatrix function takes a matrix and returns a list of the following functions:
#set the value of the matrix
#get the value of the matrix
#set the value of the matrix inverse
#get the value of the matrix inverse


makeCacheMatrix <- function(x = matrix()) {
  
  #i - Inverted matrix
  i <- NULL

  #set the value of the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  #returns value of the matrix
  get <- function() x
  
  #sets the value of the inverted matrix
  setinverse <- function(solve) i <<- solve
  
  #retruns the value of the inverted matrix
  getinverse <- function() i
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## The cacheSolve  function calculates the inverse of the matrix from the list created with the makeCacheMatrix function. 
## It first checks to see if the inverse has already been calculated, and if it exists, returns it from the cache and skips the calculation.
## Otherwise it calculates and sets the value of the inverted function in the cache using the setinverse function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  #Attempt to retrieve the inverted matrix from the cache.
  i <- x$getinverse()
  
  # Inverted Matrix exists in the cache - return this matrix
  if(!is.null(i)) {
    message("getting cached data")
    #Return the inverted matrix
    return(i)
  }
  
  #Inverted matrix does not exist in the cache. 
  data <- x$get()
  
  #Calculate the inverted matrix.
  i <- solve(data, ...)
  
  #Store the inverted matrix in the cache
  x$setinverse(i)
  
  #Return the inverted matrix
  i
  
  
}
