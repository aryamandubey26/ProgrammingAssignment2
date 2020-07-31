## The functions below will take a matrix from the user and will compute
##the inverse of the matrix. However if the inverse of the matrix has already
##been calculated, it will display the cached inverse of the matrix instead of
##computing it once again.

## This function takes a matrix from the user and performs 4 functions
## 1) sets the value of the matrix, 
## 2) gets(display) the value of the matrix
## 3) sets the value of the inverse of the matrix so that if the inverse has 
##    calculated it will not calculate it again
## 4) gets (display) the value of inverse of the matrix
## It returns a "list of functions".

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL    #Intialising with NULL
        set<-function(y){
          x<<-y                     #set the matrix
          inv<<-NULL
        }
        get<-function() x          #get the matrix
        setInverse<-function(inverse) inv<<-inverse   #setting the inverse 
        getInverse<-function() inv
        list(set=set,get=get,setInverse=setInverse,
             getInverse=getInverse)                   #list returning functions
        
}
        



##This function checks whether the inverse of the matrix has been calculated 
## before. If yes, then inverse is returned without computation, otherwise
## the inverse is computed and stored again with "setInverse" function.

cacheSolve <- function(t,...) {
        ## Return a matrix that is the inverse of 'x'
      inv<-t$getInverse()          #getting the inverse
      if (!is.null(inv)){         #checking if inverse exists
        print("getting cached data")     
        return(inv)
      }  
      MatX<-t$get()               # getting the matrix
      inv<-solve(MatX,...)       #finding inverse
      t$setInverse(inv)          #storing the inverse,can be used again if matrix is same
      inv
}
