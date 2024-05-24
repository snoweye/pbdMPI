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

comm.print <- spmd.comm.print

## Constructs text decorations for spmd.comm.cat(). Includes debugging 
## rank-coloring option `quiet = "color"`.
## 
## Rewrite all.rank (and include specific rank)
## - set specific ranks to all ranks so only one function is needed
## - do a hand-off communication between the ranks instead of barrier for
##   possible additional speedup and additional fractional delay between ranks.
##   Currently, next rank prints immediately after barrier. Look for Drew's 
##   version - he may have already done this!
##   
decor <- function(quiet, sep, rank) {
  if(is.logical(quiet)) {
    if(! quiet) {
      prefix = paste0("COMM.RANK = ", rank, "\n")
      postfix = ""
    } else {
      prefix = postfix = ""
    }
  } else if(quiet == "color") {
    col = 90 + rank %% 8  # set ANSI text color codes (platform dependent)
    prefix = paste0("\033[1;", col, "m")
    postfix = "\033[0m" # reset color
  }
  c(prefix, postfix)
}

spmd.comm.cat <- function(..., all.rank = .pbd_env$SPMD.CT$print.all.rank,
    rank.print = .pbd_env$SPMD.CT$rank.source, comm = .pbd_env$SPMD.CT$comm,
    quiet = .pbd_env$SPMD.CT$print.quiet, sep = " ", fill = FALSE,
    labels = NULL, append = FALSE, flush = .pbd_env$SPMD.CT$msg.flush,
    barrier = .pbd_env$SPMD.CT$msg.barrier, con = stdout()){
  COMM.RANK <- spmd.comm.rank(comm)
  COMM.SIZE <- spmd.comm.size(comm)
  
  if(barrier){
    spmd.barrier(comm)
  }

  ## If several ranks print, use tag-team ring
  rank.print <- unique(rank.print)
  if(all.rank) rank.print = 0L:(COMM.SIZE - 1L)
  if(COMM.RANK %in% rank.print) {
    next.rank = match(COMM.RANK, rank.print) + 1L
    prev.rank = next.rank - 2L
    if(prev.rank > 0L) recv(rank.source = rank.print[prev.rank], comm = comm)
    d = decor(quiet, sep, COMM.RANK)
    cat(d[1L], sep = "", fill = fill, labels = labels, append = append)
    cat(..., sep = sep, fill = fill, labels = labels, append = append)
    cat(d[2L], sep = "", fill = fill, labels = labels, append = append)
    if(flush) flush(con)
    if(next.rank <= length(rank.print)) 
      send(1L, rank.dest = rank.print[next.rank], comm = comm)
  }

  invisible()
} # End of spmd.comm.cat().

comm.cat <- spmd.comm.cat

