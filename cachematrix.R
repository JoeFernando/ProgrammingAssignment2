## Put comments here that give an overall description of what your
## functions do

## The Function code below is for Programming Assignment 2 for R Programming conducted by Coursera

## There are two finctions defined below "makeCacheMatrix" and "cacheSolve". The objective of these 
## two functions is to calculate the inverse of a square matrix and to store it in cache, 
## so that the inverse calculation is done only once





# makeCacheMatrix creates a list containing a function to:
# 1) set the value of the vector
# 2) get the value of the vector
# 3) set the value of the mean
# 4) get the value of the mean



makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL                    #empty vector to store the inverse of the square matrix
  
  set <- function(y) {         #re-initialises "x" and "m"
    
    x <<- y
    
    m <<- NULL
    
  }
  
  get <- function() x                         # Original square matrix is stored in "get"
  
  setinverse <- function(solve) m <<- solve   # Calculates the inverse of a square matrix
  
  getinverse <- function() m                  # stores the calcualted value into "getinverse"
  
  list(set = set, get = get,                  # List to hold the 4 objects in this function
       
       setinverse = setinverse,
       
       getinverse = getinverse)
  
}



## Write a short comment describing this function




cacheSolve <- function(x, ...) {
    
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
  
  
        ## Return a matrix that is the inverse of 'x'

