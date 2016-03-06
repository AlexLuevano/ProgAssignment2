## (First of all sorry for my deficient english)
## These functions basically do the same as the ones shown to us on the example. The first one (makeCacheMatrix) is a
## function that stores a list of functions (which i actually used!) that are intended to keep stored
## the value of a matrix on a previously designed variable. This list with the stored matrix is then kept
## inside another variable. The second function (cacheSolve) is fed then with the second variable and returns
## the inverse matrix of the original matrix. If the original matrix changes, the inverted matrix changes too.

#
# This function stores a matrix inside the variable "makeCacheMatrix" which sets itself to a default variable class matrix()
## The "invMatrix" variable is set to NULL so it can be overwritten later, the "setMatrix" function stores 
## the value of the fed variable on the parent environment.The "getMatrix" and "getInvMatrix" functions just
## print the value on the cache of each matrix respectively, and the "setMatrix" and "setInvMatrix" are used to store a new 
## value inside the temporary variables.

makeCacheMatrix <- function(x = matrix()){
              
        invMatrix <- NULL
        setMatrix <- function(y){
                    y <<-x
                    invMatrix <<- NULL
                   }
        getMatrix <- function() x
        setInvMatrix <- function(solve=x) invMatrix <<- solve
        getInvMatrix <- function() invMatrix
        list(setMatrix = setMatrix, getMatrix = getMatrix, 
             setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix)

}


## This function just takes the value stored on the previous special function (that´s why it is fed with the
## variable that stores the makeCacheMatrix already with the matrix inside) and automatically returns the inverse of
## the original matrix. If there is already an inverse matrix kept inside the first function, "cacheSolve" will 
## only call the stored matrix in that spot.

cacheSolve <- function(x = matrix(), ...) {
    invMatrix<-x$getInvMatrix()
    if(!is.null(invMatrix)){
      message("cached matrix")
      return(invMatrix)
    }
    data <- x$getMatrix()
    invMatrix<-solve(data,...)
    x$setInvMatrix(invMatrix)
    invMatrix
  }
       



