## The Function code below is for Programming Assignment 2 for R Programming conducted by Coursera

## There are two functions defined below "makeCacheMatrix" and "cacheSolve". The objective of these 
## two functions is to calculate the inverse of a square matrix and to store it in cache, 
## so that the inverse calculation is done only once




# makeCacheMatrix creates a list containing a function to:
# 1) set the value of the vector
# 2) get the value of the vector
# 3) set the value of the mean
# 4) get the value of the mean


makeCacheMatrix <- function(sm = matrix ()) {   # input "sm" will be a square matrix that is invertible
  
  iv <- NULL  	#  "iv" will be the 'inverse matrix' and it's reset to NULL               
               #  every time "makeCacheMatrix" is called
  
  
  
  # note these next four functions are not run when makeCacheMatrix is called
  # instead, they will be used by "cacheSolve"  to get values for "sm" or for
  # "iv" and for setting the inverse matrix  
  # These are usually called object 'methods'
  
  
  setnewsm <- function(y) {     #" setnewsm " function gives the user the ability to
    sm <<- y                    # to insert a new matrix without re-running "makeCacheMatrix"
    iv <<- NULL                 # essentially, the new matrix "y" over-writes "sm" and re-initiates "iv" 
  }                             # note the "<<" superassignment updates objects outside of the "setnewsm" function
  
  
  
  getfun <- function() sm       #stores the "sm" matrix in function "getfun"
  
  
  # setinversefun below is called by cacheSolve during the first cacheSolve access  
  # and it will store the inverse matrix using superassignment
  # another syntax: setinverse <- function(storeValue)  iv <<- storeValue
  
  setinversefun <- function(storeValue){  
    iv <<- storeValue                     
  }
  
  
  getinversefun <- function() iv  #this is the cache into which the calculated inverse matrix is held
  
  
  # The "list" below is initiated via the "makeCacheMatrix" function and used to hold the four functions
  # It is from this list cacheSolve and users pull functions
  # if you take out the list, then it returns an error "object of type 'closure' is not subsettable"
  
  list(set = setnewsm,  get = getfun,  setinverse = setinversefun,   getinverse = getinversefun)
  
  
}


###########################################################################################################

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve will 
## retrieve the inverse from the cache.




cacheSolve <- function(sm, ...) {   # the input is an object created by makeCacheMatrix
  
  iv <- sm$getinverse()     # accesses the object 'sm' and gets the inverse matrix stored in "getinverse"
  
  if(!is.null(iv)) {        # checks if the inverse matrix has already been cached
    
    
    message("getting cached data")       # if "yes" then displays a message and returns the 
    return(iv)                           # inverse matrix and exits the function
    
  }
  
  
  #If the cache does not hold the data, then it is computed, stored in cache and then value returned 
  
  data <- sm$get()            #get the value of the square matrix and stores in "data"
  iv <- solve(data, ...)      # calculates the inverse matrix and stores it to "iv"
  sm$setinverse(iv)           #store the calculated inverse matrix 
  iv                          # return the value of the inverse matrix and also exits the function
}


  
############################################################################################################  


## the input below runs the above two functions to check for accuracy
## taken from "Hints for Programming Assignment 2 - Lexical Scoping"


amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
amatrix$get()

cacheSolve(amatrix)
amatrix$getinverse()
cacheSolve(amatrix)

amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2))
cacheSolve(amatrix)

amatrix$get()
amatrix$getinverse()


##################################################################################


### Output Example

>    source("cachematrix.R")

>    amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
>    amatrix$get()         # Returns original matrix
[,1] [,2]
[1,]    1    3
[2,]    2    4

>   cacheSolve(amatrix)   # Computes, caches, and returns    matrix inverse
[,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5

>  amatrix$getinverse()  # Returns matrix inverse
[,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5

>  cacheSolve(amatrix)   # Returns cached matrix inverse using previously computed matrix inverse
getting cached data
[,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5

>    amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2)) # Modify existing matrix
>    cacheSolve(amatrix)   # Computes, caches, and returns new matrix inverse
[,1] [,2]
[1,] -0.13333333  0.2
[2,]  0.01010101  0.0

>    amatrix$get()         # Returns matrix
[,1] [,2]
[1,]    0   99
[2,]    5   66

>    amatrix$getinverse()  # Returns matrix inverse
[,1] [,2]
[1,] -0.13333333  0.2
[2,]  0.01010101  0.0

