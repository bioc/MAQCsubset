
setClass("proboStruct", representation(call="call"),
  contains="list")
setMethod("show", "proboStruct", function(object) {
  cat("proboStruct instance created by:\n")
  print(object@call)
})
setMethod("plot", "proboStruct", function(x, y, xlim=c(-3,3),
   col="black", ...) {
 plot(x[[1]][x$leftinds], x[[2]][x$leftinds], xlab=names(x)[1],
  ylab=names(x)[2], type="l", col=col, xlim=xlim, ...)
 lines(x[[1]][-x$leftinds], x[[2]][-x$leftinds], col=col, ...)
})

setMethod("lines", "proboStruct", function(x, y, 
   col="black", ...) {
 lines(x[[1]][x$leftinds], x[[2]][x$leftinds], 
  col=col, ...)
 lines(x[[1]][-x$leftinds], x[[2]][-x$leftinds], col=col, ...)
})

