## Below 2 functions ("makeCacheMatrix" and "cacheSolve") will be used to
## calculate the inverse of a matrix and put it to cache.
## If the inverse of a matrix is already calculated before and is in cache, 
## it will be directly called from cache to avoid recalculation.

## For a square matrix A, the inverse is written A-1. 
## When A is multiplied by A-1 the result is the identity matrix I. (I: Identity Matrix) 
## Non-square matrices do not have inverses.

## for more information, you may refer to below link
## http://www.mathwords.com/i/inverse_of_a_matrix.htm



## "makeCachematrix" function creates a special "vector", which is really a list containing a function to
##   1. set the value of the matrix
##   2. get the value of the matrix
##   3. set the value of the inverse of the matrix
##   4. get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {                     # top level function definition 
  inverse_m <- NULL                                             # we set the inverse_m to null for initialization
  set <- function(y) {                                          # sets the value of a matrix
    x <<- y                                                      
    inverse_m <<- NULL
  }
  get <- function() x                                           # gets the value of a matrix
  setinverse <- function(inv) inverse_m <<- inv                 # set the value of the inverse of the matrix
  getinverse <- function() inverse_m                            # get the value of the inverse of the matrix
  
  list(set = set, get = get,
       setinverse = setinverse, getinverse = getinverse)        # assign the functions created to the elements in the list
}

## "cacheSolve" function is used to calculate the inverse of matrix using the special list created above.
## If the inverse of a matrix is already calculated and in cache, it gets the inverse from cache and skips calculation
## Otherwise, it calculates the inverse of the matrix and sets the value into cache via the setinverse function, 

cacheSolve <- function(x, ...) {                                # top level function definition 
  m <- x$getinverse()                                           # get the inverse of the matrix
  if(!is.null(m)) {                                             # if inverse of the matrix is present (not null) then
    message("getting cached matrix inverse")                    # generate the message that the inverse is taken from cache 
    return(m)                                                   # and return the inverse of the matrix
  }
  data <- x$get()                                               # if the inverse of matrix is not present in cache, get the matrix
  m <- solve(data)                                              # find the inverse of the matrix with "solve" function 
  x$setinverse(m)                                               # set the inverse of the matrix to the cache
  m                                                             # return the inverse of the matrix as a result
}
