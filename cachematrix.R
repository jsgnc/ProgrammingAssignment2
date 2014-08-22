## The following function creates a list object that stores functions to
## access and change the values of a matrix and its inverse stored in memory.

makeCacheMatrix <- function(x = matrix()) {
      #Initiates the inv value with a NULL.
      inv <- NULL
      
      #Function to set a new matrix.
      set <- function(y) {
            x <<- y
            inv <<- NULL  #Clears the value of the inverse.
      }
      
      #Function to get the stored matrix.
      get <- function() x
      
      #Function to set manually the inverse of the matrix.
      setinverse <- function(inverse) inv <<- inverse
      
      #Function to get the value stored in the inv variable.
      getinverse <- function() inv
      
      #creates a list object with the functions described above.
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## The following function returns the inverse of a matrix created with the
## previous function. If there inverse of the matrix is already computed the
## function returns that stored value. If not, the function computes the inverse.

cacheSolve <- function(x, ...) {
      # Getting the value stored in inv variable. 
      inv <- x$getinverse()
      
      # If there is already a stored matrix in the inv variable, the function
      # returns that value
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      
      # If the inverse has not been computed earlier, the following lines are
      # executed.
      mat <- x$get()          #Retrieves the stored matrix to compute its inverse.
      inv <- solve(mat, ...)  #Computing of the inverse of the matrix.
      x$setinverse(inv)       #Stores the value of the inverse in the special list.
      inv                     #Returns the value of the inverse.
}
