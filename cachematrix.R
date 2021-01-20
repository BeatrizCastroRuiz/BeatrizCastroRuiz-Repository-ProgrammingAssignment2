
##The first function called "makeCacheMatrix" (which is inversable) will apply a function to a vector 'x'. Then, we set the value of the matriz with a new function containing as first argument 'z' as well as I assing the value NULL to the variable 'inv' (from inverse);
##in both cases, the operator '<<-' will help to keep these functions described inside another function, to preserve their value even when we are not inside the parent function.
##After the set function, we ask R to get our function, as well as we set the Inversefuncion and get this one too. 
##finally, a list is created containing as arguments the four specifications of set and get for both function (the initial and the inverse ones).

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(z) {
    x <<- z
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse =getInverse)
}


##The following function will take as arguments the initial matrix (x) and the rest by default and will return the inverse values of it which has been assigned to 'inv', with some specifications: if the inverse is getting retrieved from the cache, a message will be sent
##Then it's getting the computation of the inverse function by the function 'solve'
##Finally, it's set the value of the inverse matrix in the cacheSolve function.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}


