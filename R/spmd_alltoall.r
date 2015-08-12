### For alltoall and basic types.
spmd.alltoall.integer <- function(x.send, send.count, comm = .SPMD.CT$comm){
  .comm.size <- comm.size(comm=comm)
  if ((.comm.size * send.count) %% length(x.send) != 0)
    comm.warning("TODO")
  
  .Call("spmd_alltoall_integer", x.send, as.integer(send.count), as.integer(.comm.size), as.integer(comm), PACKAGE = "pbdMPI")
} # End of spmd.alltoall.double().

spmd.alltoall.double <- function(x.send, x.recv, send.count, recv.count,
    comm = .SPMD.CT$comm){
  .Call("spmd_alltoall_double", x.send, x.recv, send.count, recv.count,
        as.integer(comm), PACKAGE = "pbdMPI")
} # End of spmd.alltoall.double().

spmd.alltoall.raw <- function(x.send, x.recv, send.count, recv.count,
    comm = .SPMD.CT$comm){
  .Call("spmd_alltoall_raw", x.send, x.recv, send.count, recv.count,
        as.integer(comm), PACKAGE = "pbdMPI")
} # End of spmd.alltoall.raw().


### For alltoallv and basic types.
spmd.alltoallv.integer <- function(x.send, x.recv, send.count, recv.count,
    sdispls, rdispls, comm = .SPMD.CT$comm){
  .Call("spmd_alltoallv_integer", x.send, x.recv, send.count, recv.count,
        sdispls, rdispls, as.integer(comm), PACKAGE = "pbdMPI")
} # End of spmd.alltoallv.integer().

spmd.alltoallv.double <- function(x.send, x.recv, send.count, recv.count,
    sdispls, rdispls, comm = .SPMD.CT$comm){
  .Call("spmd_alltoallv_double", x.send, x.recv, send.count, recv.count,
        sdispls, rdispls, as.integer(comm), PACKAGE = "pbdMPI")
} # End of spmd.alltoallv.double().

spmd.alltoallv.raw <- function(x.send, x.recv, send.count, recv.count,
    sdispls, rdispls, comm = .SPMD.CT$comm){
  .Call("spmd_alltoallv_raw", x.send, x.recv, send.count, recv.count,
        sdispls, rdispls, as.integer(comm), PACKAGE = "pbdMPI")
} # End of spmd.alltoallv.raw().


#### Default method.
#spmd.alltoall.default <- function(x.send, x.recv, recv.count,
#    comm = .SPMD.CT$comm){
#  x.send <- serialize(x, NULL)
#  send.count <- length(x)
#  recv.count <- allreduce(send.count)
#  
#  spmd.alltoall.raw(x.send=x.send, send.count=send.count, recv.count=recv.count)
#  invisible()
#} # End of spmd.alltoall.default().


#### S4 methods.
#setGeneric(
#  name = "alltoall",
#  useAsDefault = spmd.alltoall.default
#)

#### For allreduce.
#setMethod(
#  f = "alltoall",
#  signature = signature(x = "ANY", x.buffer = "missing"),
#  definition = spmd.allreduce.default
#)
#setMethod(
#  f = "alltoall",
#  signature = signature(x = "integer", x.buffer = "integer"),
#  definition = spmd.allreduce.integer
#)
#setMethod(
#  f = "alltoall",
#  signature = signature(x = "numeric", x.buffer = "numeric"),
#  definition = spmd.allreduce.double
#)

