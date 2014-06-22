## makeCacheMatrix creates a variable which is type of List of 4. The List Items' environment are different
## from GlobalEnvironment and that is why this works perfectly but I couldn't figure it out completely yet
## and unfortunetly I must submit this...
## I read most of the http://adv-r.had.co.nz/Environments.html
## It seems that the deep assignment arrow does all the magic
## and there is no magic about function environments

makeCacheMatrix <- function(x = matrix()) { # Binding a function to a name with <- defines a binding environment.
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  environment(setInverse)
  getInverse <- function() m
  environment(getInverse)
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## There is nothing special about this function. Just checks "m" (somewhere in parent/s environment). If it finds it, returns it
## If it doesn't find it, it gets supplied matrix,calculates its inverse and assigns it  (somewhere in parent/s environment).

cacheSolve <- function(x, ...) { # Binding a function to a name with <- defines a binding environment.
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getInverse() # Calling a function creates an ephemeral execution environment that stores variables created during execution.
  # Every execution environment is associated with a calling environment, which tells you where the function was called.
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}

## SAMPLE RUN
# > c=rbind(c(1, -1/4), c(-1/4, 1)) 
# > z <- makeCacheMatrix(c)
# > cacheSolve(z)
# [,1]      [,2]
# [1,] 1.0666667 0.2666667
# [2,] 0.2666667 1.0666667
# > cacheSolve(z)
# getting cached data
# [,1]      [,2]
# [1,] 1.0666667 0.2666667
# [2,] 0.2666667 1.0666667
# > 
