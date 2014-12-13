# ********************************************************************************************
# Course Era: Assignment 2 - Lexical
# FileName: cachematrix.R
# Author: Blandine Meillon
# Date: 12 DEC 2014
# ********************************************************************************************

## This "makeCacheMatrix" function creates a special "matrix" object on type list 
# The matrix object stores the original matrix value and what will be the cached value
# initially set to NULL. There are 2 read functions also called get(getter) to get the value
# of the two items that get stored and 2 functions, called set(setter) to change them.
#that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL     # This sets the value of m to NULL everytime the function is called
  
  # This set the value of the matrix
  set <- function(y) { 
    x <<- y       # This caches the inputted matrix 
    m <<- NULL    # This sets the value of m to NULL
  }
  
  get <- function() x      # This get the value of the original matrix
  setinverse <- function(solve) m <<- solve      # This is called by the cacheSolve() during the first
                                           #called using the superassignmnet, it will store the value.  
  getinverse <- function() m      # This get/return the cached value of the cacheSolve()
          #can also be re-written as: getinverse <- function() { m }
  
  # This is a list of methods that tell the function how to access them. Each time a new object is 
  # created while calling makeCacheMatrix (), these get accessed.
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)

}


## The cacheSolve function computes the inverse of the special "matrix" returned 
# by makeCacheMatrix above. If the inverse has already been calculated (and the 
# matrix has not changed), then the cachesolve should retrieve the inverse from 
# the cache. But is the x$getinverse() value is NULL, the if() statement is skippend and 

cacheSolve <- function(x, ...) {     # makeCacheMatrix created an object: an input x
        
  m <- x$getinverse()       # Return a matrix that is the inverse of 'x'
  
  # Checks if an inverse has already been calculated (exist in the cache or still NULL)
  if(!is.null(m)) { 
    message("Getting cached data")      # This message is sent to the console while getting cache
    return(m)      # This returns the inverse value
  }
  
  data <- x$get()    # if x$getinverse() is NULL, this part runs
  m <- solve(data, ...)     # This inverse the input value of the matrix by using the solve function
  x$setinverse(m)          # This stores the inversed value in x 
  m              # This returns the inverse value
}
