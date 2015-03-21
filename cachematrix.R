## Below are two functions that are used to create a special object 
## that stores a matrix and caches its inversed matrix.

## The first function, makeCacheMatrix, creates a special "vector", 
## which is really a list containing a function to
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse matrix
## 4. get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m<- NULL
  set<- function(y){
    x<<- y
    m<<- NULL
  }
  get<- function()x
  setInverse<- function(inverse) m<<- inverse
  getInverse<- function() m
  list(set=set, get=get,
       setInverse=setInverse,
       getInverse=getInverse)
}

## The following function calculates the inverse the matrix 
## created with the above function. However, it first checks to see 
## if the inversed matrix has already been calculated. If so, 
## it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrixand sets the value 
## of the mean in the cache via the setInverse function.

cacheSolve <- function(x) {
  m<- x$getInverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<- x$get()
  m<- solve(data)
  x$setInverse(m)
  m      
}

## Testing the functions ##
test_matrix<- matrix(c(1,2,3,4), nrow=2, ncol=2)
test<- makeCacheMatrix(test_matrix)
cacheSolve(test)
cacheSolve(test)
