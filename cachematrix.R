#CREATING A FUNCTION TO INVERT THE MATRIX

makeCacheMatrix <- function(my_matrix = matrix()) {
  
  my_matrix_inv <- NULL #The variable my_matrix_inv get NULL
  
  #Set the data (values) of the Matrix
  set <- function(vdata) {
    my_matrix <<- vdata
    my_matrix_inv <<- NULL
  }
  
  get <- function() my_matrix
  
  setinverse <- function(inverse) my_matrix_inv <<- inverse
  getinverse <- function() my_matrix_inv
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

#FUNCTION TO READ THE MATRIX INVERSE iN THE CACHE

cacheSolve <- function(my_matrix, ...) {
  
  my_matrix_inv <- my_matrix$getinverse()
  
  if(!is.null(my_matrix_inv)) {
    message("The function Get the Cached Matrix.")
    return(my_matrix_inv)
  }
  
  inverse <- my_matrix$get()
  
  my_matrix_inv <- solve(inverse)
  my_matrix$setinverse(my_matrix_inv)
  
  #SHOW THE MATRIX INVERSE
  my_matrix_inv
}