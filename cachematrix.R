## Put comments here that give an overall description of what your functions do

## Write a short comment describing this function
# This function creates a special "matrix" object that can cache its inverse- 
#below function sets and gets value of matrix and the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
             i <- NULL            ##i is free variable
             set <- function (y) {     ##function to set value of matrix
                   x <<- y        ## Assign a value to object in enviornment different than current enviornment
                   i <<- NULL     ## clear the cache
             }
             get <- function (inverse) x       ##Define function to get value of matrix
             setinverse <- function(inverse) i <<- inverse     ##function to Set inverse
             getinverse <- function(inverse) i                 ##function to Get inverse
                                              ## below returns list with the four above defined functions    
              list(set = set, get = get,       ## Set and get value of matrix
                  setinverse = setinverse,     ## Set value of matrix inverse
                  getinverse = getinverse)     ## Get value of matrix inverse 
}


## Write a short comment describing this function
# This function will compute inverse of matrix from the first function, does not have to recalculate because is cached from above

cacheMatrix <- function(x, ...) {
  i <- x$getinverse()     ##gets cached value for inverse
  if(!is.null(i)) {       ##returns inverse i from cache
       message("this matrix works")
       return(i)
  }
  data <-x$get()        ##get value of matrix
  i <- solve(data,...)  ##Calculate inverse
  x$setInverse(i)       ##Cache inverse
  return(i)             ##Return inverse
}

## Return a matrix that is the inverse of 'M1'
M <- matrix(c(1:4), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("X","Y"), c("A","B")))
M2 <- makeCacheMatrix(M)
cacheSolve(M2)

