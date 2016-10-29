#CREATING A FUNCTION TO INVERT THE MATRIX

makeCacheMatrix <- function(my_matrix = matrix()) {
  
  my_matrix_inv <- NULL #The variable my_matrix_inv get NULL
  
  #Set the data (values) of the Matrix
  set <- function(vdata) {
    my_matrix <<- vdata
    my_matrix_inv <<- NULL
  }
  
  #Get the matrix
  get <- function() my_matrix
  
  #Set the inverse of the matrix
  setinverse <- function(inverse) my_matrix_inv <<- inverse
  getinverse <- function() my_matrix_inv
  
  #Return a list
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

#FUNCTION TO READ THE MATRIX INVERSE iN THE CACHE

cacheSolve <- function(my_matrix, ...) {
  
  #Return the Matrix Inverse
  my_matrix_inv <- my_matrix$getinverse()
  
  #Return the inverse if its already set
  if(!is.null(my_matrix_inv)) {
    message("The function Get the Cached Matrix.")
    return(my_matrix_inv)
  }
  
  #Get the Matrix
  inverse <- my_matrix$get()
  
  ## Use solve() to Inverse
  my_matrix_inv <- solve(inverse)
  my_matrix$setinverse(my_matrix_inv)
  
  #SHOW THE MATRIX INVERSE
  my_matrix_inv
}
