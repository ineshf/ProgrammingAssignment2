## Function makeCacheMatrix(x=matrix())
## This function creates a special "matrix", which is really a list containing
#a function to:
# set the value of matrix
# get the value of matrix
# set the value of the inverse matrix
# get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  # init matrix default
  matrix <- NULL
  
  # set function set matrix value
  set <- function(newmatrix) {
    x <<- newmatrix
    matrix<<- NULL
  }
  
  # get function get matrix value
  get <- function() x
  
  # setmatrix function set matrix inverse values
  setmatrix <- function(matrix_inverse) matrix <<- matrix_inverse
  
  # get matrix function get matrix inverse values
  getmatrix <- function() matrix
  
  # create list with the params of matrix
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

## Function cacheSolve(x,...)
# x is matrix to calculates it inverse matrix
# return the inverse matrix of x
#The following function calculates the inverse matrix of the special "matrix" created with the above function. 

cacheSolve <- function(x, ...) {

  # Check if the matrix is in cache
  m <- x$getmatrix()
  if(!is.null(m)) {
    
        message("getting cached inverse matrix")
        #First checks to see if the mean has already been calculated. If so, it gets the inverse from the cache 
        #and skips the computation.
        # Return the inverse matrix stored in cache
        return(m)
      }
  
      #It calculates the inverse matrix of the data and sets the value of
      #the inverse matrix in the cache via the setmatrix function.
  
      data <-x$get()
      m <- solve(data)
      x$setmatrix(m)
  
  # Return de inverse matrix calculated
  m
}
