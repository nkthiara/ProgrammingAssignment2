# Matrix inversion is usually a costly computation and there 
# may be some benefit to caching the inverse of a matrix rather 
# than computing it repeatedly. The two functions will cache the 
# inverse of a matrix.

# makeCacheMatrix will create a list that contains the 
# function to:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
            i <- NULL
            set<- function(y){
              x<<-y
              i<<-NULL
            }
            get <- function()x
            set_inverse <- function(inverse) i <<- inverse
            get_inverse <- function() i
            list(set = set, get=get,
                  set_inverse = set_inverse,
                  get_inverse = get_inverse)
}


# The function cacheSolve "computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already been
# calculated (and the matrix has not changed), then cacheSolve 
# retrieves the inverse from the cache."

cacheSolve <- function(x, ...) {
      i <- x$get_inverse()
      if(!is.null(i)){
        message("Getting cached data.")
        return(i)
      }
      data<- x$get()
      i <- solve(data)
      x$set_inverse(i)
      i
}

