## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  #' @x is a matrix, it should not be sigular matrix
  #' @return list of functions: set, get, setinvmat, getinvmat
  #' set: intialize/set-up passed matrix value
  #' get: return last set-up value
  #' setinvmat: set inverse of matrix when calculated
  #' getinvmat: get cached inverse of matrix 
  inv <- NULL
  
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinvmat <- function(inverse) inv <<- inverse 
  
  getinvmat <- function() inv
  
  return(list(set = set,get = get,setinvmat = setinvmat,getinvmat = getinvmat))
  

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
   
  #' @description This function computes inverse of the matrix which is returned
  #' by makeCacheMatrix above. if inverse has already been calculated and matrix has 
  #' not been changed then it will retrieve the inverse from cache. 
  
  #' @x  m is a list of functions, which is made from makeCacheMatrix
  #' @return inverse of matrix, which is taken out from x
  #' It also assumes passed matrix is not singular 
  
  inv <- x$getinvmat() 
  
  if(!is.null(inv)){
    message("getting cached inverse matrix")
    return(inv)
  }
  
  # get matrix data
  mat <- x$get()
  

  # Assume determinant is always computationally greater than zero 
  # Matrix is inverable
  # calculate inverse of matrix
  inv <- solve(mat,...)
    
  # sets the value of inverse in cache via setinvmat function
  x$setinvmat(inv)
    
  return(inv)

}

