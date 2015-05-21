## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##             If the inverse has already been calculated (and the matrix has not changed), then cacheSolve will
##             retrieve the inverse from the cache. 

## The following function receives a matrix as the argument x. It returns a list containing fuctions to:
## 1. set the value of the matrix
## 2. get the value of the vector
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      
      m <- NULL
      set <- function(y){
            x <<- y
            m <<- NULL
      }
      get <- function(){
            x
      }
      setinverse <- function(meaning){
            m <<- meaning
      }
      getinverse <- function(){
            m
      }
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of a square matrix that has been cached and set by makeCacheMatrix

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      i <- x$getinverse()
      
      if(is.null(i)){
      ## if inverse has not been cached:
      ## a) get the original cached matrix
      ## b) calculate the inverse
      ## c) cache the calculate inverse
            
            getit <- x$get()
            i <- solve(getit, ...)
            x$setinverse(i)
            i
      } else{
            # Check to see if cached variable already contains inverse
            message("getting cached data")
      }
      i
}
