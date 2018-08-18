### 
send.check.type <- function(x, rank.dest, tag, comm){
  if (!is.matrix(x) && (is.integer(x) || is.double(x) || is.raw(x))){
    if (is.integer(x)){
      type <- RTYPE_INTEGER
    } else if (is.double(x)){
      type <- RTYPE_DOUBLE
    } else{
      type <- RTYPE_RAW
    }
  } else{
    type <- RTYPE_OTHER
  }
  
  spmd.send.integer(type, rank.dest = rank.dest, tag = tag, comm = comm)
  return(type)
}


### S4 functions.

### Default method.
spmd.send.default <- function(x,
    rank.dest = .pbd_env$SPMD.CT$rank.dest, tag = .pbd_env$SPMD.CT$tag,
    comm = .pbd_env$SPMD.CT$comm){
  type <- send.check.type(x = x, rank.dest = rank.dest, tag = tag, comm = comm)
  if (type == RTYPE_INTEGER){
    spmd.send.integer(x, rank.dest = rank.dest, tag = tag, comm = comm)
  } else if (type == RTYPE_DOUBLE){
    spmd.send.double(x, rank.dest = rank.dest, tag = tag, comm = comm)
  } else if (type == RTYPE_RAW){
    spmd.send.raw(x, rank.dest = rank.dest, tag = tag, comm = comm)
  } else{
    spmd.send.raw(serialize(x, NULL), rank.dest = rank.dest, tag = tag, comm = comm)
  }
  invisible()
} # End of spmd.send.default().


### For send.
spmd.send.integer <- function(x,
    rank.dest = .pbd_env$SPMD.CT$rank.dest, tag = .pbd_env$SPMD.CT$tag,
    comm = .pbd_env$SPMD.CT$comm){
  .Call("spmd_send_integer", x, as.integer(rank.dest), as.integer(tag),
        as.integer(comm), PACKAGE = "pbdMPI")
  invisible()
} # End of spmd.send.integer().

spmd.send.double <- function(x,
    rank.dest = .pbd_env$SPMD.CT$rank.dest, tag = .pbd_env$SPMD.CT$tag,
    comm = .pbd_env$SPMD.CT$comm){
  .Call("spmd_send_double", x, as.integer(rank.dest), as.integer(tag),
        as.integer(comm), PACKAGE = "pbdMPI")
  invisible()
} # End of spmd.send.double().

spmd.send.raw <- function(x,
    rank.dest = .pbd_env$SPMD.CT$rank.dest, tag = .pbd_env$SPMD.CT$tag,
    comm = .pbd_env$SPMD.CT$comm){
  .Call("spmd_send_raw", x, as.integer(rank.dest), as.integer(tag),
        as.integer(comm), PACKAGE = "pbdMPI")
  invisible()
} # End of spmd.send.raw().


### S4 methods.
setGeneric(
  name = "send",
  useAsDefault = spmd.send.default
)

### For send.
setMethod(
  f = "send",
  signature = signature(x = "ANY"),
  definition = spmd.send.default
)
