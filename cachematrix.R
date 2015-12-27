## This function creates a special "matrix" object (list) that first stores the matrix itself and can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  matr <- NULL
  ##Set and get matrix
  set <- function(y) {
    x <<- y
    matr <<- NULL
  }
  get <- function() x
  ##set and get inverse matrix
  setinverse <- function(inverse) { 
                  matr <<- inverse 
                }
  getinverse <- function() { 
                  matr 
                }
  ##final list object 
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  matr <- x$getinverse()
  if(!is.null(matr)) {
    message("getting cached data - inverse matrix")
    return(matr)
  }
  data <- x$get()
  matr <- solve(data)
  x$setinverse(matr)
  matr
}



##Create matrix / matrices
matr_data_2 = c(2,5,1,3)
matrix2 <- matrix(data = matr_data_2, nrow = 2, ncol = 2, byrow = TRUE)

matr_data_4 = c(5,2,0,0,3,-1,-5,3,0,1,3,-3,-1,-5,0,3)
matrix4 <- matrix(data = matr_data_4, nrow = 4, ncol = 4, byrow = TRUE)

##Test makeCacheMatrix function (m$get() prints matrix, whereas m$getinverse() should print NULL at first)
m = makeCacheMatrix(matrix4) ##edit according to matrix you want to test (in this example "matrix2" or "matrix4")
m$get()
m$getinverse()

##Test cacheSolve function (run twice - 1st run gets inverse of matrix by using solve() function, 2nd gets inverse from cache)
cacheSolve(m)
##Function getinverse() gets inverse matrix from m list
m$getinverse()
