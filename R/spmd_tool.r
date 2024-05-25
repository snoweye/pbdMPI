### Tool functions.

spmd.hostinfo <- function(comm = .pbd_env$SPMD.CT$comm){
  if(spmd.comm.size(comm) == 0){
    stop(paste("It seems no members running on comm", comm))
  }
  HOST.NAME <- spmd.get.processor.name()
  COMM.RANK <- spmd.comm.rank(comm)
  COMM.SIZE <- spmd.comm.size(comm)
  cat("\tHost:", HOST.NAME, "\tRank(ID):", COMM.RANK, "\tof Size:", COMM.SIZE,
      "on comm", comm, "\n")
  invisible()
} # End of spmd.hostinfo().

spmd.comm.print <- function(x, all.rank = .pbd_env$SPMD.CT$print.all.rank,
    rank.print = .pbd_env$SPMD.CT$rank.source, comm = .pbd_env$SPMD.CT$comm,
    quiet = .pbd_env$SPMD.CT$print.quiet,
    flush = .pbd_env$SPMD.CT$msg.flush,
    barrier = .pbd_env$SPMD.CT$msg.barrier, con = stdout(), ...){
  COMM.RANK <- spmd.comm.rank(comm)

  # Don't print "COMM.RANK = " even if verbose=TRUE in the case 'x' is invalid
  if (!exists(deparse(substitute(x))))
    quiet <- TRUE

  if(barrier){
    spmd.barrier(comm)
  }

  if(all.rank){
    for(i.rank in 0:(spmd.comm.size(comm) - 1)){
      if(i.rank == COMM.RANK){
        if(! quiet){
          cat("COMM.RANK = ", COMM.RANK, "\n", sep = "")
          if(flush){
            flush(con)
          }
        }
        print(x, ...)
        if(flush){
          flush(con)
        }
      }
      if(barrier){
        spmd.barrier(comm)
      }
    }
  } else{
    for(i.rank in rank.print){
      if(i.rank == COMM.RANK){
        if(! quiet){
          cat("COMM.RANK = ", COMM.RANK, "\n", sep = "")
          if(flush){
            flush(con)
          }
        }
        print(x, ...)
        if(flush){
          flush(con)
        }
      }
      if(barrier){
        spmd.barrier(comm)
      }
    }
  }

  invisible()
} # End of spmd.comm.print().


## Constructs text decorations for spmd.comm.cat().
## Includes debugging rank-coloring option quiet = "color".
##
decor <- function(quiet, comm) {
  if(is.logical(quiet)) {
    postfix <- ""
    if(quiet) {
      prefix <- ""
    } else {
      prefix <- paste0("COMM.RANK = ", comm.rank(comm), "\n")
    }
  } else if(is.integer(quiet)) {
    rank <- comm.rank(quiet) # color by comm specified by quiet integer
    col <- 90 + rank %% 8  # set ANSI text color codes (platform-dependent)
    prefix <- paste0("\033[1;", col, "m")
    postfix <- "\033[0m" # reset color
  }
  c(prefix, postfix)
}

spmd.comm.print <- function(x, all.rank = .pbd_env$SPMD.CT$print.all.rank,
                rank.print = .pbd_env$SPMD.CT$rank.source,
                comm = .pbd_env$SPMD.CT$comm, quiet = .pbd_env$SPMD.CT$print.quiet,
                flush = .pbd_env$SPMD.CT$msg.flush,
                barrier = .pbd.env$SPMD.CT$msg.barrier, con = stdout(), sleep = 0, ...) {
  COMM.RANK <- spmd.comm.rank(comm)
  COMM.SIZE <- spmd.comm.size(comm)

  ## Barrier is released by rank 0 after COMM.SIZE check-ins regardless of who
  ## posts them! Seems this might break in sub-comm grids?
  ## Prints are handed off to other system components so overprint can still
  ## occur - use a .01 or shorter sleep to avoid this.
  if(COMM.RANK == 0L && sleep > 0) Sys.sleep(sleep) # give last cat time to land
  if(barrier) spmd.barrier(comm)

  ## If several ranks print, use distributed tag-team
  rank.print <- unique(rank.print) # duplicates would deadlock!
  if(all.rank) rank.print <- 0L:(COMM.SIZE - 1L)
  if(COMM.RANK %in% rank.print) {
    next.rank <- match(COMM.RANK, rank.print) + 1L
    prev.rank <- next.rank - 2L

    if(prev.rank > 0L) # post a blocking receive
      recv(rank.source = rank.print[prev.rank], comm = comm)

    d <- decor(quiet, comm)
    cat(d[1L], sep = "")
    print(x, ...)
    cat(d[2L], sep = "")
    if(flush) flush(con)

    if(next.rank <= length(rank.print)) # release next print rank
      send(integer(0L), rank.dest = rank.print[next.rank], comm = comm)
  }
} # End of spmd.comm.print().

comm.print <- spmd.comm.print

spmd.comm.cat <- function(..., all.rank = .pbd_env$SPMD.CT$print.all.rank,
    rank.print = .pbd_env$SPMD.CT$rank.source, comm = .pbd_env$SPMD.CT$comm,
    quiet = .pbd_env$SPMD.CT$print.quiet, sep = " ", fill = FALSE,
    labels = NULL, append = FALSE, flush = .pbd_env$SPMD.CT$msg.flush,
    barrier = .pbd_env$SPMD.CT$msg.barrier, con = stdout(), sleep = 0) {
  COMM.RANK <- spmd.comm.rank(comm)
  COMM.SIZE <- spmd.comm.size(comm)

  ## Barrier is released by rank 0 after COMM.SIZE check-ins regardless of who
  ## posts them! Seems this might break in sub-comm grids?
  ## Prints are handed off to other system components so overprint can still
  ## occur - use a .01 or shorter sleep to avoid this.
  if(COMM.RANK == 0L && sleep > 0) Sys.sleep(sleep) # give last cat time to land
  if(barrier) spmd.barrier(comm)

  ## If several ranks print, use distributed tag-team
  rank.print <- unique(rank.print) # duplicates would deadlock!
  if(all.rank) rank.print <- 0L:(COMM.SIZE - 1L)
  if(COMM.RANK %in% rank.print) {
    next.rank <- match(COMM.RANK, rank.print) + 1L
    prev.rank <- next.rank - 2L

    if(prev.rank > 0L) # post a blocking receive
      recv(rank.source = rank.print[prev.rank], comm = comm)

    d <- decor(quiet, comm)
    cat(d[1L], sep = "", fill = fill, labels = labels, append = append)
    cat(..., sep = sep, fill = fill, labels = labels, append = append)
    cat(d[2L], sep = "", fill = fill, labels = labels, append = append)
    if(flush) flush(con)

    if(next.rank <= length(rank.print)) # release next print rank
      send(integer(0L), rank.dest = rank.print[next.rank], comm = comm)
  }

  invisible()
} # End of spmd.comm.cat().

comm.cat <- spmd.comm.cat
