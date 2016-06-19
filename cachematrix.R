## There are two functions defined in this file.
## 1. makeCacheMatrix - This function creates a list of functions to be used to set and get an inverse of a matrix
## 2. cacheSolve - This function  either gets value of matrix from cache or calcuales it if value is not available


## makeCacheMatrix - This function is defined to 
## 1. set value of a matrix
## 2. get value of a matrix
## 3. set value of an inverse of a matrix
## 4. get value of an inverse of matrix
## 5. Return list of all above functions


makeCacheMatrix <- function(x = matrix()) {

## set value of a matrix
  i <- NULL 
  
  set <- function(y) { 
    x <<- y
    i <<- NULL
  }
  
  
## get value of a matrix
  
  get <- function() x 
  
## set value of an inverse of a matrix
  setinv <- function(inv) i <<- inv 
  
## get value of an inverse of matrix
  
  getinv <- function() i 
 
## return list  
  list(set = set, get = get, 
       setinv = setinv,
       getinv = getinv)
  
}


## cacheSolve - This function is defined to
## return an inverse of a matrix if already cached or
## calculate an inverse of a matrix and return an inverse


cacheSolve <- function(x, ...) {

## Return a matrix that is the inverse of 'x'
## get the value of a inverse of matrix and assign to a new variable
  i <- x$getinv() 
  
## check if inverse is present if yes, get it from cache and return
  if(!is.null(i)) {
    message("getting cached data")
    return(i) 
  }
  
## Calculate inverse if it is not present
  
##calculate inverse of a matrix and return inverse
  
  data <- x$get() 
  i <- solve(data) 
  x$setinv(i) 
  i 
  }
