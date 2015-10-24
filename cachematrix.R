## makeCacheMatrix creates the special vector list where its elements are set
## of functions which perform following operation
## 1. Create and set the matrix - set function
## 2. Get the matrix -- get function
## 3. Set the Inverse Matrix in cache- setmatrix
## 4. Get the Inverse Cache Matrix value - getmatrix

## Create the Matrix and also cached the Inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## cacheSolve checks if there is cached inverse matrix, if yes, it will retrieve
## or else it will compute the inverse matrix and cache it 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}