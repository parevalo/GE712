#' @title Parallel processing of raster stack objects
#' @name apply_stack_parallel
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#'
#' @param x raster stack
#' 
#' @param fun is a function to be applied to pixel location of the raster stack object and can
#' be defined by the user. The function must have two arguments: x is numeric vector and 
#' args.list is a list of other arguments used in the processing. The function must return
#' a vector lenght equal to \code{nl}, number of layers in the output stack.
#' 
#' @param args.list a list of arguments to pass to \code{fun}
#' 
#' @param nl integer > 0. How many layers should the output RasterBrick have? 
#' Default is \code{nlayers(x)}
#' 
#' @param progress a character. See \code{\link[raster]{pbCreate}}. Default is \code{'text'}
#' 
#' @param filename a character with the file name. Optional
#' 
#' @param ... other arguments to pass to \code{\link[raster]{beginCluster}} and 
#' \code{\link[raster]{writeStart}}
#'
#' @description This function performs the parallel processing of raster stack objects.
#'
#' @noRd
apply_stack_parallel <- function(x, fun, args.list = list(), nl = nlayers(x), progress = 'text', filename = "", ...) {

     # Create output raster
     out <- raster::brick(x, nl = nl, values = FALSE)
     #names(out) <- names(x)
     args.list <- c(args.list, nl = nl)

     # Create cluster
     raster::beginCluster(...)
     cl <- raster::getCluster()
     nodes <- length(cl)

     # Compute raster tiles
     bs <- raster::blockSize(x, minblocks = nodes * 4)
     bs$array_rows <- cumsum(c(1, bs$nrows * out@ncols))
     pb <- raster::pbCreate(bs$n, progress = progress)

     # Creat cluster function 
     cl_fun <- function(k, x, bs, fun, args.list){
       v <- raster::getValues(x, bs$row[k], bs$nrows[k])
       t(apply(v, 1, fun, args.list))
     }

     # Get all nodes going
     for (k in 1:nodes) {
          snow::sendCall(cl[[k]], cl_fun, list(k, x, bs, fun, args.list), tag = k)
     }

     # If needed create raster files
     filename <- raster::trim(filename)
     if (!raster::canProcessInMemory(out) & filename == "") {
          filename <- raster::rasterTmpFile()
     }

     # Start writing the output 
     if (filename != "") {
          out <- raster::writeStart(out, filename = filename, ... )
     } else {
          vv <- matrix(out, ncol = nlayers(out))
     }

     # Process raster tiles
     for (k in 1:bs$n) {
          # receive results from a node
          d <- snow::recvOneData(cl)

          # error?
          if (! d$value$success) {
            print(d$value) 
            stop('cluster error')
          }

          # which block is this?
          b <- d$value$tag

          # Write chunk results to output 
          if (filename != "") {
               out <- raster::writeValues(out, d$value$value, bs$row[b])
          } else {
               rows <- seq(from = bs$array_rows[b], by = 1, length.out = bs$nrows[b]*out@ncols)
               vv[rows,] <- d$value$value
          }

          # need to send more data?
          ni <- nodes + k
          if (ni <= bs$n) {
               snow::sendCall(cl[[d$node]], cl_fun, list(ni, x, bs, fun, args.list), tag = ni)
          }

          # Progess bar
          raster::pbStep(pb, k)

     }

     # Create output raster
     if (filename != "") {
          out <- raster::writeStop(out)
     } else {
          out <- raster::setValues(out, as.vector(vv))
     }

     # Close cluster
     raster::pbClose(pb)

     # Assign layers names
     #names(out) <- names(x)
     raster::endCluster()

     return(out)
}


