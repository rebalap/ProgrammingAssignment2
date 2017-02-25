## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#programming assignment

#setting set get functions for matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  ##create inverse matrix variable
  invmatrix<-NULL
  
  setmatrix<- function(y){
    x<<-y
    invmatrix<<-NULL  #set invserse matrix  null when the matrix is being initialized
  }
  ##get matrix function
  getmatrix<-function() x
  
  ##set inverse matrix function
  setmatrixinverse<-function(mat_rix) invmatrix<<-mat_rix
  
  ##get inverse matrix function
  getmatrixinverse<-function()invmatrix
  
  #create list of functions for get and set
  list(setmatrix=setmatrix,getmatrix=getmatrix,setmatrixinverse=setmatrixinverse,getmatrixinverse=getmatrixinverse)
}


## this function checks for cache and if unavailable calls the solve function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invmatrix<-x$getmatrixinverse()
  ## check if the inverse is cached 
  if(!is.null(invmatrix))
  {
    message("getting cached data")
    return(invmatrix)
  }
  ##if theres no cached data execute solve function to get the inverse
  data<-x$getmatrix()
  invmatrix<-solve(data,...)
  x$setmatrixinverse(invmatrix)
  invmatrix
  
}
