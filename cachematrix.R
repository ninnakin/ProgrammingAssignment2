## Two functions that allows the user to cache the inverse of a matrix 
## and then retrieve it rather than recomputing it

#  Creates a matrix of the type makeCacheMatrix from the input matrix x
#  makeCacheMatrix has four built in functions:
#  set : sets the values of the matrix x  
#  get : returns the matrix x
#  setinv : sets the value of the variable inv that represents the inverse of x
#  getinv : returns inv
makeCacheMatrix <- function(x = matrix()) 
{
  inv <-NULL;
  set <- function(y) 
  {
    x <<- y  
    inv <<- NULL  # set inv to NULL if x is updated
  }
  get <- function() x  # return x
  setinv <- function(inverse) inv <<- inverse  # set inv to the input value inverse
  getinv <- function() inv  # return inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


#  Computes the inverse for matrix x if it is not cached. If so, gets the 
#  inverse from memory.
#
#  input:  x,   the matrix for which the inverse is to be retrieved
#  output: inv, the inverse of x
cacheSolve <- function(x, ...) 
{
  ## check if the inverse of x is stored
  ## return the cached inv if it exists
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## If no inverse in cache - compute it and return
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
