## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


# generateEye <- function(numRow) {
#   m<-matrix(0,numRow, numRow)
#   c<-1:numRow
#   for (index in c) {
#     m[index,index]=1
#   }
#   m
# }

makeCacheMatrix <- function(x = matrix()) {
  
  matrixInv <- NULL
  set <- function(y) {
    x <<- y
    matrixInv <<- NULL
  }
  get <- function() x
  setMatrixInv <- function(mInv) matrixInv <<- mInv
  getMatrixInv <- function() matrixInv
  list(set = set, get = get,
       setMatrixInv = setMatrixInv,
       getMatrixInv = getMatrixInv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getMatrixInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  numRow<- nrow(data)
  m <- solve(data,diag(numRow))
  x$setMatrixInv(m)
  m
}
