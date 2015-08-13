### For alltoall and basic types.
spmd.alltoall.integer <- function(x, send.count, comm = .SPMD.CT$comm){
  .comm.size <- comm.size(comm=comm)
  if ((.comm.size * send.count) %% length(x) != 0)
    comm.warning("TODO")
  
  .Call("spmd_alltoall_integer", x, as.integer(send.count), 
        as.integer(.comm.size), as.integer(comm), PACKAGE = "pbdMPI")
} # End of spmd.alltoall.integer().

spmd.alltoall.double <- function(x, send.count, comm = .SPMD.CT$comm){
  .comm.size <- comm.size(comm=comm)
  if ((.comm.size * send.count) %% length(x) != 0)
    comm.warning("TODO")
  
  .Call("spmd_alltoall_double", x, as.integer(send.count), 
        as.integer(.comm.size), as.integer(comm), PACKAGE = "pbdMPI")
} # End of spmd.alltoall.double().

spmd.alltoall.raw <- function(x, x.recv, send.count, recv.count,
    comm = .SPMD.CT$comm){
  .Call("spmd_alltoall_raw", x, x.recv, send.count, recv.count,
        as.integer(comm), PACKAGE = "pbdMPI")
} # End of spmd.alltoall.raw().


### For alltoallv and basic types.
spmd.alltoallv.integer <- function(x, x.recv, send.count, recv.count,
    sdispls, rdispls, comm = .SPMD.CT$comm){
  .Call("spmd_alltoallv_integer", x, x.recv, send.count, recv.count,
        sdispls, rdispls, as.integer(comm), PACKAGE = "pbdMPI")
} # End of spmd.alltoallv.integer().

spmd.alltoallv.double <- function(x, x.recv, send.count, recv.count,
    sdispls, rdispls, comm = .SPMD.CT$comm){
  .Call("spmd_alltoallv_double", x, x.recv, send.count, recv.count,
        sdispls, rdispls, as.integer(comm), PACKAGE = "pbdMPI")
} # End of spmd.alltoallv.double().

spmd.alltoallv.raw <- function(x, x.recv, send.count, recv.count,
    sdispls, rdispls, comm = .SPMD.CT$comm){
  .Call("spmd_alltoallv_raw", x, x.recv, send.count, recv.count,
        sdispls, rdispls, as.integer(comm), PACKAGE = "pbdMPI")
} # End of spmd.alltoallv.raw().



### Default method.
spmd.alltoall.default <- function(x, send.count, comm = .SPMD.CT$comm){
  x <- serialize(x, NULL)
  
  spmd.alltoall.raw(x=x, send.count=send.count, comm=comm)
  invisible()
} # End of spmd.alltoall.default().


### S4 methods.
setGeneric(
  name = "alltoall",
  useAsDefault = spmd.alltoall.default
)

### For alltoall.
setMethod(
  f = "alltoall",
  signature = signature(x = "ANY"),
  definition = spmd.alltoall.default
)
setMethod(
  f = "alltoall",
  signature = signature(x = "integer"),
  definition = spmd.alltoall.integer
)
setMethod(
  f = "alltoall",
  signature = signature(x = "numeric"),
  definition = spmd.alltoall.double
)

