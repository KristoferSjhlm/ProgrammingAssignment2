## The "makeCacheMatrix" creates matrix object that can be stored and fetch from the cache
## using the following functions. In this case we can get the cached inversed matrix withou calculations

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inversed matrix
## 4. set the value of the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
  
          m <- NULL
          set <- function(y) {
                      x <<- y
                      m <<- NULL
          }
          get <- function() x
          setmatrix <- function(matrix) m <<- matrix # assigns the matrix to be stored in the cache
          getmatrix <- function() m
          
          # returns the functions as a list
          list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}

## This function checkes if the inverse matrix already has been stores in th cache
## If it has, it fetches the inverse matrix from the cache.
## If it's a new matrix that is passed in to the function, it computes the inverse matrix 
## using the solve() function

cacheSolve <- function(x, ...) {
          
          m <- x$getmatrix() # get the matrix object from the getmatrix function
          
          if(!is.null(m)) { # checks if the calculation is stores in cache 
                    message("getting cached data")
                    return(m) # return the cached matrix that is the inverse of 'x'
          }
          data <- x$get() # if not, get the data
          m <- solve(data, ...) # calculate the inverse matrix using solve() 
          x$setmatrix(m) # store the new data in in the cache 
          m # Return the new matrix that is the inverse of 'x'
}

## To try the solution try for example:
## > m1 <- makeCacheMatrix(matrix(21:24, nrow=2, ncol=2))
## > cacheSolve(m1)
