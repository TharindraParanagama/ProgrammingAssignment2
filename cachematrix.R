## makecacheMatrix function passes the initial matrix for which the inverse is needed and retreives the inverse of the matrix.

makeCacheMatrix <- function(x=matrix()) {
  
  #This global variable,which is used in the setInverse function is initialized to NULL,
  #since it's value will be determined in the cacheSolve function.
  
  inv<-NULL
  
  #setMat function is used to retreive the matrix passed as a parameter to the makeCacheMatrix function
  #and stores it in a local variable y
  
  setMat<-function(y){
    x<<-y
    inv<<-NULL
  }
  
  #getMat function is used to simply access the value returned by the setMat function
  
  getMat<-function() x
  
  #This function simply obtains the inverse of the matrix computed in the cacheSolve function and stores 
  #it in the variable inv.
  
  setInverse<-function(inverse) inv<<-inverse
  
  #This function is simply used to access the value returned by the setInverse function
  
  getInverse<-function() inv
  
  #this stores the functions defined above as a list of objects
  
  list(setMat=setMat,getMat=getMat,setInverse=setInverse,getInverse=getInverse)
  
}


## The function cacheSolve computes the inverse of a given matrix if not already available in cache. 

cacheSolve <- function(x, ...) {
  
  #The if statement checks whether the inverse of the given matrix exist in the cache,if so it returns the inverse,
  #if not the else block computes the inverse of the given matrix via the solve function and returns the value.
  
  if(!is.null(x$getInverse())){
    
    print("Inverse of your matrix")
    return(x$getInverse)
    
  }
  else{
    
    print("Inverse of your matrix")
    print(x$setInverse(solve(x$getMat())))
    
  }
  
  
}

