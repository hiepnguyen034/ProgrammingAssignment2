## The functions create an object that store the inverse of a matrix and compute that inverse
## 

## a function creating an object that stores the inverse of a matrix

makeCacheMatrix<-function(x=matrix()){
        i<-NULL
        set<-function(y){
                x<<-y
                i<<-NULL
        }
        get<-function() x
        setinverse<-function(inverse) i<<-inverse
        getinverse<-function() i
        list(set=set, get=get,
             setinverse=setinverse,
             getinverse=getinverse)
        
}


## a function computing the inverse stored in the matrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i<-x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data<-x$get()
        i<-solve(data,...)
        x$setinverse(i)
        i
}
