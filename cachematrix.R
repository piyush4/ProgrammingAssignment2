## makeCacheMatrix: This function creates a special "matrix" object
## that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

## The function makeCacheMatrix creates a special vector,
## which really is a list containing a function to 
## 1.) set the value of the matrix
## 2.) get the value of the matrix
## 3.) set the inverse of the matrix
## 4.) get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverseMat <- NULL
  set<-function(y){
    x <<- y
    inverseMat <<- NULL
  }
  get<- function() x
  setinverse <- function(inverse) inverseMat <<- inverse
  getinverse <- function() inverseMat
  list(set = set, get = get, setinverse = setinverse, 
       getinverse = getinverse)
  
}


## Calculates the inverse of the special matrix created in the function
## above. However, it first checks if the inverse has already been
## calculated. If so, it gets the inverse from the cache and skips the 
## computation. Otherwise, it calculates the inverse of the matrix 
## and sets the value of the matrix in the cache via the setmean function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverseMat <- x$getinverse()
  if(!is.null(inverseMat)){
    message("getting cached data")
    return(inverseMat)
  }
  data <- x$get()
  inverseMat <- solve(data)
  x$setinverse(inverseMat)
  inverseMat
}
