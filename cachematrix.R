## These functions cache in potentially time consuming computations such as calculations of the inverse of a function
## These functions are similar to the sample given in the class. 

##  a special vector which is a list containg a function that a) sets a matrix b) gets a matrix contd...
## ... c) sets the inverse of a matrix and d) gets the  inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
                m <- NULL
                set <- function(y) {
                  x <<- y
                  m <<- NULL
                }
                get <- function() x
                setinverse <- function(solve) m <<- solve
                getinverse <- function() m
                list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)

}



##  calculates the inverse of the matrix created by the above function. The function checks in case the contd...
## ... the inverse has been calculated

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
# Test case
# X <-  matrix(data = c(4,2,2,3),nrow=2,ncol=2)
# a = makeCacheMatrix(X)
# cacheSolve(a)