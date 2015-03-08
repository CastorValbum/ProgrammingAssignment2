makeCacheMatrix <- function(x = numeric()) {
      #
      # This function defines 4 ghost-functions needed in order to cache the inverse       
      # of the inputmatrix. First the I matrix, which will later be used as a "result-
      # matrix", is defined.
      
      I <- NULL
      
      # The set-function transfers the 'y' matrix to the global environment x, thus
      # making the input available to all other functions.
      
      set <- function(y) {
            x <<- y
            I <<- NULL
      }
      # The get-function simply delivers the original matrix from the global environment
      
      get <- function() x
      
      # Somewhat similar to the former, the setinverse transfers the calculated inverse
      # matrix to the global environment.
      
      setinverse <- function(inverse) I <<- inverse
      
      # the getinverse-function pulls the calculated inverse matrix from the global
      # environment.
      
      getinverse <- function() I
      
      # The pruduct of this overall function is to produce a list with all of the 
      # ghost-functions which can be called from another function (cacheSolve).
      
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}
###############################################################################


cacheSolve <- function(x, ...) {
      
      # Firstly, cacheSolve pulls the inverse matrix from the global environment.
      
      I <- x$getinverse()
      
      # ...It then tests if I is populated with values ("!is.null(I)"). If so, it 
      # writes a message and returns the inverse matrix (and exits the function).
      
      if(!is.null(I)) {
            message("Getting cached data...")
            return(I)
      }
      
      # If the I matrix is not populated, cacheSolve pulls the original inputmatrix
      # to a new variable called data.
      
      data <- x$get()
      
      # It then finds the inverse of the original inputmatrix and defines this as the
      # inverse (I)- matrix.
      message("Calculating inverse of matrix...")
      I <- solve(data, ...)
      
      # cacheSolve then calls the setinverse function from "makeCacheMatrix", defining
      # the I matrix to the global environment.
      
      x$setinverse(I)
      
      # Finally, makeCacheMatrix returns the result (the inverse matrix)
      
      I
}