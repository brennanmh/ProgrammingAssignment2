
# this code contains a pair of functions for
# storing a matrix and a cache of it's inverse
# to improve performace for inverting large
# invertible matrices.


# makeCacheMatrix:
#   create a cached matrix "object"
#   takes a matrix as an argument
makeCacheMatrix <- function(mat = matrix()) {

    # initialize inverse to NULL
    cminv <- NULL

    # set (reset) the matrix stored in object to y
    set <- function(y) {
        mat <<- y
        cminv <<- NULL
    }

    # pull the matrix out
    get <- function() mat

    # set the inverse
    setinv <- function(inv) cminv <<- inv

    # get the inverse
    getinv <- function() cminv

    # create the list of "methods"
    list(set = set, get = get, setinv = setinv, getinv = getinv)

}


# cacheSolve:
#   this function first checks to see if the inverse of
#   the matrix in the cacheMatrix object x has been
#   calculated. If so, it returns the cached inverse
#   if not, it calculates, stores, and returns the inverse
cacheSolve <- function(x, ...) {

    # pull the cached inverse
    inv <- x$getinv()

    # if the cached inverse has value (has been previously calculated)
    # return it
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }

    # otherwise, first pull the actual matrix
    mat <- x$get()
    # calculate it's inverse
    inv <- solve(mat, ...)
    # store the inverse in the cacheMatrix object
    x$setinv(inv)

    # return the newly calculated inverse
    inv
}


#
# quick little test
#
test <- function() {

  # init an invertible matrix
  mat <- matrix(c(1,3,5,7,11,13,17,19,23),3,3)

  # create a cached version of it
  cmat <- makeCacheMatrix(mat)

  # calculate the inverse
  inv <- cacheSolve(cmat)

  print(mat)
  print(inv)
  print(round(mat%*%inv, 2))

  # calculate the inverse again, note the message
  inv2 <- cacheSolve(cmat)
  print(inv2)
  print(round(mat%*%inv2, 2))

}


