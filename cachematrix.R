## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
  set <- function(y) {
          x <<- y
          i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          i <- x$getinverse()
  if (!is.null(i)) {
          message("getting cached data")
          return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}



##Let us test our functions
my_matrix <- makeCacheMatrix(matrix(c(1,3,2,4), 2, 2))
my_matrix$get()
#[,1] [,2]
#[1,]    1    2
#[2,]    3    4
my_matrix$getInverse()
#NULL
cacheSolve(my_matrix)
#[,1] [,2]
#[1,] -2.0  1.0
#[2,]  1.5 -0.5
cacheSolve(my_matrix)
#getting cached data
#[,1] [,2]
#[1,] -2.0  1.0
#[2,]  1.5 -0.5
my_matrix$getInverse()
#[,1] [,2]
#[1,] -2.0  1.0
#[2,]  1.5 -0.5
