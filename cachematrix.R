## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The first function "makeCacheMatrix" creates a special matrix object that can cache the input matrix and its inverse 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL # provides a default if cacheSolve has not yet been used
    setmatrix <- function(y) { #set the value of the matrix
      x <<- y ## caches the inputted matrix so that cacheSolve can check whether it has changed
      m <<- NULL ## sets the value of matrix inverse if cacheSolve is used to NULL
}
  getmatrix <- function() x #gets the value of the matrix
  setinverse <- function(solve) m <<- solve  #sets the value of the inverse
  getinverse <- function() m #gets the value of the inverse
  list (setmatrix = setmatrix, getmatrix=getmatrix, setinverse=setinverse, getinverse=getinverse)
#creates a list to hosue the four functions
}


## Write a short comment describing this function
## The second function "cacheSolve" calls functions stored in the special "matrix" retunred by makeCacheMatrix (above).
##If the inverse has already been calculated, then cacheSolve retrieves the inverse from the cache. If the input is new, 
##it calculates the inverse of the data and sets the inverse in the cache via the setinverse function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse() #if an inverse has already been calculated this gets it
  if(!is.null(m)){ #checks to see if cache "m" contains any value 
      message("getting cached data")
      return(m)
  }
  data <- x$getmatrix() #run getmatrix function to get get the value of input matrix  
  m <- solve(data, ...) #compute the value of the inverse of the input matrix 
  x$setinverse(m) #run the set inverse function on inverse to cache inverse
  m #return the inverse
}

