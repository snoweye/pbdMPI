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

#' Constructs text color decorations for spmd.comm.cat() and spmd.comm.print().
#'
#' @param quiet
#' Logical or integer to control rank-coloring. If logical, no
#' coloring is done. If integer, up to 8 colors are used to distinguish ranks.
#' If there are more ranks than colors, colors are recycled.
#' This is accomplished by prepending ANSI text color codes and post-pending
#' color reset. This is particularly useful in debugging situatons using
#' `all.rank = TRUE`.
#' @param comm
#' The communicator to be used for coloring.
#'
#' @return A character vector of length 2, giving the prefix and postfix.
spmd.comm.decor <- function(quiet, comm = .pbd_env$SPMD.CT$comm) {
  if (is.logical(quiet)) {
    prefix <- ""
    postfix <- ""
    if (!quiet) prefix <- paste0("COMM.RANK = ", comm.rank(comm), "\n")
  } else if (is.integer(quiet)) {
    rank <- comm.rank(quiet) # color by comm specified by quiet integer
    col <- 90 + rank %% 8 # set ANSI text color codes (platform-dependent)
    prefix <- paste0("\033[1;", col, "m")
    postfix <- "\033[0m" # reset color
  }
  c(prefix, postfix)
}

spmd.comm.print <- function(x, all.rank = .pbd_env$SPMD.CT$print.all.rank,
    rank.print = .pbd_env$SPMD.CT$rank.source, comm = .pbd_env$SPMD.CT$comm,
    quiet = .pbd_env$SPMD.CT$print.quiet,
    flush = .pbd_env$SPMD.CT$msg.flush,
    barrier = .pbd_env$SPMD.CT$msg.barrier, con = stdout(), sleep = 0, ...){
  COMM.RANK <- spmd.comm.rank(comm)
  COMM.SIZE <- spmd.comm.size(comm)

  # Don't print "COMM.RANK = " even if verbose=TRUE in the case 'x' is invalid
  if (!exists(deparse(substitute(x))))
    quiet <- TRUE

  ## Barrier released by rank 0 after COMM.SIZE check-ins. Will this break in
  ## sub-comm grids? No, because sub-comms are disjoint.
  ## Prints are handed off to other system components so overprint can still
  ## occur - use a .01 or shorter sleep to avoid this.
  if (COMM.RANK == 0L && sleep > 0) {
    Sys.sleep(sleep)
  } # give last cat time to land
  if (barrier){
    spmd.barrier(comm)
  }

  ## If several ranks print, use distributed tag-team
  rank.print <- unique(rank.print) # duplicates would deadlock!
  if(all.rank){
    rank.print <- 0L:(COMM.SIZE - 1L)
  }
  rank.pos <- match(COMM.RANK, rank.print)
  if(!is.na(rank.pos)){
    # my rank prints

    if(rank.pos > 1L){
      # not first, so post a blocking receive from previous
      recv(rank.source = rank.print[rank.pos - 1L], comm = comm)
    }

    ## Print with decorations if requested
    d <- spmd.comm.decor(quiet, comm)
    cat(d[1L], sep = "")
    print(x, ...)
    cat(d[2L], sep = "")
    if(flush){
      flush(con)
    }

    if(rank.pos < length(rank.print)){
      # not last, so release next print rank
      send(integer(0L), rank.dest = rank.print[rank.pos + 1L], comm = comm)
    }
  }
} # End of spmd.comm.print().

comm.print <- spmd.comm.print

spmd.comm.cat <- function(..., all.rank = .pbd_env$SPMD.CT$print.all.rank,
    rank.print = .pbd_env$SPMD.CT$rank.source, comm = .pbd_env$SPMD.CT$comm,
    quiet = .pbd_env$SPMD.CT$print.quiet, sep = " ", fill = FALSE,
    labels = NULL, append = FALSE, flush = .pbd_env$SPMD.CT$msg.flush,
    barrier = .pbd_env$SPMD.CT$msg.barrier, con = stdout(), sleep = 0){
    COMM.RANK <- spmd.comm.rank(comm)
    COMM.SIZE <- spmd.comm.size(comm)

    if(COMM.RANK == 0L && sleep > 0){
      Sys.sleep(sleep)
    } # give last cat time to land
    if(barrier){
      spmd.barrier(comm)
    }

    ## If several ranks print, use distributed tag-team
    rank.print <- unique(rank.print) # duplicates would deadlock!
    if(all.rank){
      rank.print <- 0L:(COMM.SIZE - 1L)
    }
    rank.pos <- match(COMM.RANK, rank.print)
    if(!is.na(rank.pos)){
      # my rank prints

      if(rank.pos > 1L){
        # not first, so post a blocking receive from previous
        recv(rank.source = rank.print[rank.pos - 1L], comm = comm)
      }

      ## Print with decorations if requested
      d <- spmd.comm.decor(quiet, comm)
      cat(d[1L], sep = "", fill = fill, labels = labels, append = append)
      cat(..., sep = sep, fill = fill, labels = labels, append = append)
      cat(d[2L], sep = "", fill = fill, labels = labels, append = append)
      if(flush){
        flush(con)
      }

      if(rank.pos < length(rank.print)){
        # not last, so release next print rank
        send(integer(0L), rank.dest = rank.print[rank.pos + 1L], comm = comm)
      }
    }

    invisible()
} # End of spmd.comm.cat().

comm.cat <- spmd.comm.cat
