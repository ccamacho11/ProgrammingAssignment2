## This function creates a especial vector

library (MASS)
makeCacheMatrix <- function(x = matrix()) {
 rev <- NULL
 set <- function (y) {
    x <<-y
    rev <- NULL
 }
 get <- function()x
 setrev <- function(reverse) rev <<- reverse
 getrev <- function(){
   rever <- grev(x)    
   rever%*%x
}
  list (set = set, get = get,
        setrev = setrev,
        getrev = getrev)
}


## The  function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        rev <- x$getrev()
        if (!is.null(rev)) {
          message("getting cached data!")
          return(rev) ## Return a matrix that is the reverse of 'x'
        }
        data <- x$get()
        rev <- solve(data,...)
        x$setrev(rev)
        rev
      }
