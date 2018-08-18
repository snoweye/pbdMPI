### 
recv.check.type <- function(rank.source, tag, comm, status){
  spmd.recv.integer(x.buffer = integer(1), rank.source = rank.source, tag = tag,
    comm = comm, status = status)
}


### S4 functions.

### Default method.
spmd.recv.default <- function(x.buffer = NULL,
    rank.source = .pbd_env$SPMD.CT$rank.source, tag = .pbd_env$SPMD.CT$tag,
    comm = .pbd_env$SPMD.CT$comm, status = .pbd_env$SPMD.CT$status){
  type <- recv.check.type(rank.source = rank.source, tag = tag, comm = comm,
      status = status)
  spmd.probe(rank.source, tag, comm, status)
  source.tag <- spmd.get.sourcetag(status)
  total.length <- spmd.get.count(type, status)
  if (type == RTYPE_INTEGER){
    buf <- integer(total.length)
    recv.fun <- spmd.recv.integer
  } else if (type == RTYPE_DOUBLE){
    buf <- double(total.length)
    recv.fun <- spmd.recv.double
  } else if (type == RTYPE_RAW || type == RTYPE_OTHER){
    buf <- raw(total.length)
    recv.fun <- spmd.recv.raw
  }
  
  ret <- recv.fun(buf,
    rank.source = source.tag[1],
    tag = source.tag[2],
    comm = comm,
    status = status)
  
  if (type == RTYPE_OTHER){
    unserialize(ret)
  } else{
    ret
  }
} # End of spmd.recv.default().


### For recv.
spmd.recv.integer <- function(x.buffer,
    rank.source = .pbd_env$SPMD.CT$rank.source, tag = .pbd_env$SPMD.CT$tag,
    comm = .pbd_env$SPMD.CT$comm, status = .pbd_env$SPMD.CT$status){
  .Call("spmd_recv_integer", x.buffer, as.integer(rank.source),
        as.integer(tag), as.integer(comm), as.integer(status),
        PACKAGE = "pbdMPI")
} # End of spmd.recv.integer().

spmd.recv.double <- function(x.buffer,
    rank.source = .pbd_env$SPMD.CT$rank.source, tag = .pbd_env$SPMD.CT$tag,
    comm = .pbd_env$SPMD.CT$comm, status = .pbd_env$SPMD.CT$status){
  .Call("spmd_recv_double", x.buffer, as.integer(rank.source),
        as.integer(tag), as.integer(comm), as.integer(status),
        PACKAGE = "pbdMPI")
} # End of spmd.recv.double().

spmd.recv.raw <- function(x.buffer,
    rank.source = .pbd_env$SPMD.CT$rank.source, tag = .pbd_env$SPMD.CT$tag,
    comm = .pbd_env$SPMD.CT$comm, status = .pbd_env$SPMD.CT$status){
  .Call("spmd_recv_raw", x.buffer, as.integer(rank.source),
        as.integer(tag), as.integer(comm), as.integer(status),
        PACKAGE = "pbdMPI")
} # End of spmd.recv.raw().


### S4 methods.
setGeneric(
  name = "recv",
  useAsDefault = spmd.recv.default
)

### For recv.
setMethod(
  f = "recv",
  signature = signature(x.buffer = "ANY"),
  definition = spmd.recv.default
)
setMethod(
  f = "recv",
  signature = signature(x.buffer = "integer"),
  definition = spmd.recv.integer
)
setMethod(
  f = "recv",
  signature = signature(x.buffer = "numeric"),
  definition = spmd.recv.double
)
setMethod(
  f = "recv",
  signature = signature(x.buffer = "raw"),
  definition = spmd.recv.raw
)
