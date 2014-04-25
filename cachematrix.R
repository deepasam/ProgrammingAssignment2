## This function 

makeCacheMatrix <- function(x = matrix()) 
{
  m <- NULL
  set <- function(y) 
  {
    x <<- y
    m <<- NULL
  }
  
  get <- function() { x }
  
  setmatrix <- function(sm=matrix ()) { m <<- sm }
  
  getmatrix<- function() { m }
  
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)

}
## This function returns a matrix that is the inverse of the argument 'x'
cacheSolve <- function(x) {

  m <- x$getmatrix()          # query the x vector's cache         
  if(!is.null(m))             # if there is a cache
  {           
     message("getting cached data") 
     return(m)                #just return the cache, no computation needed
  }
  data <- x$get()             # if there's no cache
  m <- solve(data)            # find inverset of the matrix
  x$setmatrix(m)              # save the result back to x's cache
  m                           # return the result
}
