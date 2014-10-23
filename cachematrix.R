## Code by Carlos Martinez - last updated 10/23/2014
## Assignment 2 of "Introduction to R" (Oct2014) from Coursera

## makeCacheMatrix creates a "matrix" function that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {            #creates matrix_name as a list of functions for a matrix
     inv <- NULL
     set <- function(y) {                              #matrix_name$set caches the matrix within matrix_name
          x <<- y
          inv <<- NULL
     }
     get <- function() x                               #matrix_name$get pulls the matrix from matrix_name and displays it
     setinverse <- function(inverse) inv <<- inverse   #matrix_name$setinverse caches the inverse of the matrix
     getinverse <- function() inv                      #matrix_name$getinverse pulls the cached inverse from matrix_name and displays it
     list(set = set, get = get,                        #creates matrix_name as a list of functions for the matrix
          setinverse = setinverse,
          getinverse = getinverse)
}


## cacheSolve gives the inverse of the matrix within matrix_name created by makeCacheMatrix

cacheSolve <- function(x, ...) {
     inv <- x$getinverse()
     if(!is.null(inv)) {                         #If inverse is already cached:
          message("getting cached data")         #-informs the user it's using cached data
          return(inv)                            #-displays the already cached inverse
     }
     data <- x$get()                             #If inverse is not already cached:
     message("calculating and caching inverse")  #-informs the user it's calculating and caching the inverse
     inv <- solve(data, ...)                     #-calculates the inverse
     x$setinverse(inv)                           #-caches the inverse
     inv                                         #-displays the inverse
}
