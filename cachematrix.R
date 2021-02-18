# A note on what the inverse of a matrix is and why you do it.
# The inverse of a matrix is used when you wish to do division with a matrix
# which is not possible, but division is possible by the inverse

# If you have to divide 10 pens to 2 people you can simply do 10/2=5 or you can 
# find the inverse of 2 i.e 0.5 and multiply it with 10 which gives (10*0.5 =5)

# inverse matrices are widely used in cryptography applications where you 
# encrypt messages by multiplying them with a matrix and you need to know the 
# inverse to decode

# inverse matrices are used in geometry (graphics, games, navigation), to 
# modelling (weather simulation, fluid dynamics, chemical reactions), to 
# statistical analysis and so on. 

################################################################################





## This function creates a matrix object that can cache its inverse
## This save computation

makeCacheMatrix <- function(x = matrix()) {
  # we can assume that the matrix is invertible
  # NULL is used to represent the NULL object. 
  inv <- NULL
  # now we will set the value of the matrix using another function
  set <- function(y) {
    # the double <<- is used to assign an object in an environment that
    # is different from the current environment
    # it allows us to manage variables at different levels
    x <<- y
    inv <<- NULL
  }
  # now we want a function to get the value of the matrix
  get <- function() {x}
  # now we set the value of the inverse
  setInverse <- function(inverse) {
    inv <<- inverse}
  # now we get the value of the inverse
  getInverse <- function() {
      inv }
  # produce a list at the end of the function that produces the following
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
    }



## cachesolve is a function that computes the inverse of the matrix object
## produced by makeCacheMatrix. Where the inverse is already calculated (and 
## the matrix has not been changed), cachesolve retrives the inverse from the 
## cache

cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  # now we want to see if the has already been calculated. if so then it can get
  # the inverse from the cache 
  if(!is.null(inv)) {
    # if the inverse is going to be retrieve from the cache then a message will
    # be produced/displayed
    message("getting cached data")
    # and the inverse will be returned
    return(inv)
  }
  # to compute the inverse of a matrix solve is the standard
  # function
  mat <- x$get()
  inv <- solve(mat, ...)
  # set the value of the inverse in the cache use the setInverse() function
  x$setInverse(inv)
  #then produce the inv result
  inv
}


################################################################################

# Now let's test the functions
  
  # Create a test matrix

test_matrix <- makeCacheMatrix(matrix(1:4, nrow = 2, ncol = 2))

  # use get() to print the matrix to see what it looks like

test_matrix$get()

  # use getInverse() to test whether the inverse has been cached
    # it hasn't, so it will produce a NULL message

test_matrix$getInverse()
    

  # use cacheSolve() to get the inverse of the matrix

cacheSolve(test_matrix)

  # now if you use the cacheSolve() function again it will retrive it from the 
  # cache. You know will know this because it has printed a message telling you
  # this.

  # if you use the getInverse() function again, it will now retrieve the inverse
  # matrix from the cache
test_matrix$getInverse()





