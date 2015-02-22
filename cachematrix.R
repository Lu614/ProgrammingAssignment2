## vector
makeVector <- function(x = matrix(data= mat)) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setm <- function(solve) m <<- solve
    getm <- function() m
    list(set = set, get = get,
         setm = setm,
         getm = getm)
}


  ## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  m <- x$getm()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get
  m <- solve(mat, ...)
  x$setm(m)
  m
  
}

