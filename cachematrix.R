###This code relies heavily on the example code presented with the assignment
###There has been little modification except where necessary to change the function from mean to 
###solve. Good excercise in higher-level functions and the <<- operator. Tests with
###system.time() verify that the caching is signigficantly faster





makeCacheMatrix <- function(x = numeric()) {
 ###makeCacheMatrix: This function creates a special "matrix" 
 ###object that can cache its inverse.
  
  

  ###creating new matrix set m to NULL
  m <- NULL
  
  ###return the matrix and make sure m is null
  ###both m and x promoted to the parent environment
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ###function to return the matrix
  get <- function() x
  
  ###function to set inverse matrix
  setsolv <- function(sol) m <<- sol
  
  
  ###Function to get the inverted matrix
  getsolv <- function() m
  
  ###list the higher order function names 
  list(set = set, get = get, setsolv = setsolv, getsolv = getsolv)
  
  

  
}

cacheSolve <- function(x){
# cacheSolve: This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse t has already been 
#calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
  
  
  #print("incachsolve")
  
  #### get the value of m
  m <- x$getsolv()
  
  #print("after  get")
  
  
  #print(m)
  
  ###if m is NOT null we've already calculated its inverse, 
  ###return the cached version
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ###if m is undefined(NULL) find the inverse 
  ###and set the value in x using x$setsolve
  
  m <- solve(x$get())
  #print("printing m")
  #print(m)
  x$setsolv(m)
  m
  
}
