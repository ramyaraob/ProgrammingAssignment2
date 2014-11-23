## The goal of this project is to store the inverse of a matrix and retrieve it, if it is available in the cache.
## It is achieved by the << operator which stores the matrix in the memory.
## It can be any datatype and not just matrix that can be stored in cache

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


# Sample Run
#z=matrix(c(2,3,1,4),nrow=2,ncol=2)
# b=makeCacheMatrix(z)
#b$get()
#[,1] [,2]
#[1,]    2    1
#[2,]    3    4
# cacheSolve(b)
#[1] "No matrix inverse cached, computing inverse"
#[,1] [,2]
#[1,]  0.8 -0.2
#[2,] -0.6  0.4
# cacheSolve(b)
#[1] "getting cached data"
#[,1] [,2]
#[1,]  0.8 -0.2
#[2,] -0.6  0.4
