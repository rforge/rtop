
gDist.rtop = function(object, ...) {
  params = object$params
  if (params$debug.level > 1) debug.level = params$debug.level else debug.level = 0
  if (!"dObs" %in% names(object)) object$dObs = rtopDisc(object$observations, ...)
  if (!"dPred" %in% names(object)) object$dPred = rtopDisc(object$predictionLocations, ...)
  object$gDistObs = gDist(object$dObs, object$dObs, debug.level = debug.level, params = params, ...)
  object$gDistPredObs = gDist(object$dObs, object$dPred, debug.level = debug.level, params = params, ...)
  object$gDistPred = gDist(object$dPred, object$dPred, diag=TRUE, debug.level = debug.level, params = params,...)
  object
}


gDist.SpatialLinesDataFrame = function(object, object2=NULL, ...) {
  if (is(object2,"SpatialLinesDataFrame")) object2 = as(object2,"SpatialLines")
  gDist(as(object,"SpatialLines"),object2)
}


gDist.SpatialLines = function(object, object2=NULL, ...) {
  dObs = rtopDisc(object, ...)
  gDistObs = gDist(dObs, ...)
  if (!is.null(object2)) {
    dPred = rtopDisc(object2, ...)
    gDistPredObs = gDist(dObs, dPred, ...)
    gDistPred = gDist(dPred, dPred, diag=TRUE,...)
    list(gDistObs = gDistObs, gDistPred = gDistPred, gDistPredObs = gDistPredObs)
  } else list(gDistObs = gDistObs)
}



gDist.SpatialPolygonsDataFrame = function(object, object2=NULL, ...) {
  if (is(object2,"SpatialPolygonsDataFrame")) object2 = as(object2,"SpatialPolygons")
  gDist(as(object,"SpatialPolygons"),object2)
}


gDist.SpatialPolygons = function(object, object2=NULL, ...) {
  dObs = rtopDisc(object, ...)
  gDistObs = gDist(dObs, ...)
  if (!is.null(object2)) {
    dPred = rtopDisc(object2, ...)
    gDistPredObs = gDist(dObs, dPred, ...)
    gDistPred = gDist(dPred, dPred, diag=TRUE,...)
    list(gDistObs = gDistObs, gDistPred = gDistPred, gDistPredObs = gDistPredObs)
  } else list(gDistObs = gDistObs)
}


gDist.list = function(object,object2=NULL,diag = FALSE, debug.level = 0, ...) {
  variogramModel=list(model = "Gho",params = 0)
  if (length(object[[1]]) ==2) {
  # These are the discretized points for hypotetical areas from binned variograms
    gDist = data.frame(c1 = c(rep(0,length(object))),c2 = 0,cb=0)
    lAreas = lapply(object,FUN=function(aa) aa[[1]])
    gDist[,1] = mapply(vred,lAreas, MoreArgs = list(vredTyp="ind",variogramModel = variogramModel))
    lAreas = lapply(object,FUN=function(aa) aa[[2]])
    gDist[,2] = mapply(vred,lAreas, MoreArgs = list(vredTyp="ind",variogramModel = variogramModel))
    gDist[,3] = mapply(vred,object, MoreArgs = list(vredTyp="ind",variogramModel = variogramModel))
  } else {
    gDist = varMat(object, object2,diag = diag,variogramModel = variogramModel, 
             debug.level = debug.level, ...)
  }
  return(as.matrix(gDist))
}


