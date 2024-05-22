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

## Constructs the text for spmd.comm.cat(). Includes debugging rank-coloring
## option `quiet = "color"`.
cat_text <- function(..., quiet, sep, rank) {
  m1 = paste(..., sep = sep)
  if(is.logical(quiet)) {
    if(! quiet) {
      m0 = paste0("COMM.RANK = ", rank, "\n")
      m2 = ""
    } else {
      m0 = m2 = ""
    }
  } else if(quiet == "color") {
    col = 30 + COMM.RANK %% 8  # uses ANSI color codes
    m0 = paste0("\033[1;", col, "m")
    m2 = "\033[0m" # reset color
  }
  paste0(m0, m1, m2)
}

spmd.comm.cat <- function(..., all.rank = .pbd_env$SPMD.CT$print.all.rank,
    rank.print = .pbd_env$SPMD.CT$rank.source, comm = .pbd_env$SPMD.CT$comm,
    quiet = .pbd_env$SPMD.CT$print.quiet, sep = " ", fill = FALSE,
    labels = NULL, append = FALSE, flush = .pbd_env$SPMD.CT$msg.flush,
    barrier = .pbd_env$SPMD.CT$msg.barrier, con = stdout()){
  COMM.RANK <- spmd.comm.rank(comm)

  if(barrier){
    spmd.barrier(comm)
  }

  if(all.rank){
    for(i.rank in 0:(spmd.comm.size(comm) - 1)){
      if(i.rank == COMM.RANK){
        cat(cat_text(..., quiet = quiet, sep = sep, rank = COMM.RANK), 
                     fill = fill, labels = labels, append = append)
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
        cat(cat_text(..., quiet = quiet, sep = sep, rank = COMM.RANK), 
                     fill = fill, labels = labels, append = append)
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
} # End of spmd.comm.cat().

comm.cat <- spmd.comm.cat

