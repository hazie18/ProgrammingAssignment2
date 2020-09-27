# the overall function takes an 

makeCacheMatrix <- function(x = matrix()){
  # 1)
  #the makeCacheMatrix function caches the 
  #inverse of a special matrix object.
makeCacheMatrix <- function(x = matrix()){
  hZ <- NULL
  set <- function(y){
    x <<- y
    hZ <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) hZ <<- inverse
  getInverse <- function () hZ
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse
       )
}
# 2)
#"cacheSolve" solves the inverse of the 
#special "matrix" that is returned by "makeCacheMatrix".
#if it is found that the inverse has previously been
#calculated and the matrix has not changed, then "cacheSolve"
#will return the inverse from the cache.

cachSolve <- function(x, ...) {
#return a matrix that is the invers of 'x'
  hZ <- x$getInverse() 
  if (is.null(hZ)) {
    message("getting cached data")
    return(hZ)
  }
  mat <- x$get()
  hZ <- solve(mat, ...)
  x$setInverse(hZ)
  hZ
}
  APmatrix<- makeCacheMatrix(matrix(c(2,3,1,4), ))
  APmatrix$get() #return the value of x
  Apmatrix$getInverse() #return the value of hZ
  cacheSolve(APmatrix) #return the inverse Matrix
  APmatrix#getInverse()
  