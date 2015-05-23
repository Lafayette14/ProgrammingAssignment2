## The purpose of the following code is computing and caching the inverse of a matrix.
##
## In order to do so there are two functions: makeCacheMatrix and cacheSolve.
## They will be discussed in the code below. 


## makeCacheMatrix creates a "matrix" object that is able to cache its inverse,  
## and this function stores a list of four subset functions so that they can be called individually.
## makeCacheMatrix has five operations:
##
## set has two operations:
##  "x<<-y" substitutes the matrix x with y in the main function, which is makeCacheMatrix. 
##  In addition, "s<<-NULL" restores to NULL the value of the inverse. 
## 
## get simply returns the matrix x stored in the main function.
##
## setSolve is a function that stores the value of the input inverse in a variable s into the main function.
##
## getSolve returns s.
##
## list(set = set, get = get, setSolve = setSolve, getSolve = getSolve) stores the four subset function.


makeCacheMatrix <- function(x = matrix()) {
  s<- NULL
  set <- function(y) {
    x<<- y
    s<<- NULL
  }      
  get <- function() x
  
  setSolve<- function(inverse) s<<-inverse
  
  getSolve <- function() s
  
  list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}

## cacheSolve calculates the inverse of the matrix returned by makeCacheMatrix function.
## The cacheSolve function has several stages:
##
## If the inverse exists, a message will be shown and the inverse will be returned.
##
## If the inverse does not exist, three operations will be executed: 
##
## data gets the matrix stored with makeCacheMatrix.
## s <- solve(data, ...) computes the inverse of the matrix "data".
## x$setSolve(s) stores the inverse in s created assigned with makeCacheMatrix, the inverse of the matrix is cached.
## In the end, a matrix that is the inverse of x is returned.

cacheSolve <- function(x, ...) {
  
  s <- x$getSolve()
  
  if (!is.null(s)) {
    
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  
  s<-solve(data, ...)
  
  x$setSolve(s)
  
  s
}
          
     

