## 2 functions are used to create a matrix object and get the inverse
#   The 2 functions will set a special matrix and then return the inverse
#   The inverse will use the cached value if available, otherwise it will calculate it


## makeCacheMatrix function does the following:
#   1. Set the value of the matrix
#   2. Get the value of the matrix
#   2. Set the value of the inverse matrix
#   2. Get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  
  mInv <- NULL
  set <- function(y){
      x<<-y
      mInv<<-NULL
  }
  get<-function() x
  setInverse <- function(inverse) mInv <<-inverse
  getInverse <- function() mInv
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)

}


## cacheSolve does the following:
#   1. look to see if the inverse is cached
#   2. If it is, return the cached inverse
#   3. If is is not cached, calculate it and return the inverse

cacheSolve <- function(x, ...) {
    mInv<-x$getInverse()
    if ( !is.null(mInv)){
      print("Using Cached Data")
      return(mInv)
    }
    else {
      mInv<-solve(x$get())
      x$setInverse(mInv)
      return(mInv)
    }
    
}
