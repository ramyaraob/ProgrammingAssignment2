This project exhibits the data storing persistance of R

This is achieved by creating a cachable matrix object as shown is the code below

## create a matrix with operations to store and retrieve stored inverse

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solved) m<<- solved # use the super assignment operator to store the value in global env
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## for a give cachable matrix x get the inverse if exists or compute the inverse and store it and return the computed value

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  #if matrix inverse is available in the cache/global env, return it
  if(!is.null(m)){
    print("getting cached data")
    
  }else
  {
  print("No matrix inverse cached, computing inverse")
  matrix<-x$get()
  #compute the inverse of the matrix
  m<-solve(matrix, ...)
  #cache the matrix
  x$setmatrix(m) 
  }
  return (m)
}

