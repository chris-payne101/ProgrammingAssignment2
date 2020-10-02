## How to test these functions
## Create a 2x2 matrix: myMatrix <- matrix(1:4,2,2)
## Create the 'special' vector: myMatrixVector <- makeCacheMatrix(myMatrix)
## This creates an object containing the original matrix and functions to set/get the inverse
## Pass myMatrixVector as an argument to cacheSolve: cacheSolve(myMatrixVector)
## which should return the inverse of the original matrix (myMatrix) and save the inverse in myMatrixVector
## Subsequent calls to cacheSolve(myMatrixVector) should display the message "getting cached data"
## and returned the cached matrix inverse

## makeCacheMatrix creates a special "vector" which contains the
## functions to: 
## 1. set the value of the input matrix
## 2. get the value of the  input matrix
## 3. set the value of the matrix inverse
## 4. get the value of the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve calculates the inverse of the special "vector" 
## created with the makeCacheMatrix function. However, it first 
## checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the 
## computation. Otherwise, it calculates the inverse of the data and 
## sets the value of the inverse in the cache via the setsolve function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
