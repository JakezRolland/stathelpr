#' drawRegressionPlot
#'
#' @param paramY String with Y parameter name
#' @param paramX String with X parameter name
#' @param data Dataframe with datas
#'
#' @return
#' @export
#' @import ggplot2
#' @examples
drawRegressionPlot<-function(paramY,paramX,data){
  functionName<-match.call()[[1]]
  step<-"Start"
  tryCatch({
    resPlot = ggplot()+geom_point(data = data,aes(y=data[,paramY],x=data[,paramX]))+xlab(paramX)+ylab(paramY)

    return(resPlot)
  }, error = function(err) {
    message_traceback <- err$message
    errormessage = paste(paste("at ", functionName, " :: ", step, sep = ""), message_traceback, sep = "\n")
    stop(errormessage)
  })
}


#' drawQuantiVsQualiQuanti
#'
#' @param Xquanti Name of quantitative parameter
#' @param Xquali Name of qualitative parameter
#' @param paramY Name of response quantitative parameter
#' @param data Dataframe with datas
#'
#' @return
#' @export
#' @import ggplot2
#' @examples
drawQuantiVsQualiQuanti<-function(data,paramY,Xquanti,Xquali){
  functionName<-match.call()[[1]]
  step<-"Start"
  tryCatch({
    resPlot = ggplot()+geom_point(data = data,aes(y=data[,paramY],x=data[,Xquanti],colour=data[,Xquali]))+xlab(Xquanti)+ylab(paramY)+labs(color = Xquali)

    return(resPlot)
  }, error = function(err) {
    message_traceback <- err$message
    errormessage = paste(paste("at ", functionName, " :: ", step, sep = ""), message_traceback, sep = "\n")
    stop(errormessage)
  })
}

#' drawQualiAndQuanti
#'
#' @param paramY String with Y parameter name
#' @param paramX String with Xs parameter name
#' @param data Dataframe with datas
#'
#' @return
#' @export
#' @import ggplot2
#' @examples
drawAnovaPlot<-function(data,paramY,paramX){
  functionName<-match.call()[[1]]
  step<-"Start"
  tryCatch({
    resPlot <- ggplot(data, aes(x=data[,paramX], y=data[,paramY])) +
      geom_boxplot()+xlab(paramX)+ylab(paramY)
    return(resPlot)
  }, error = function(err) {
    message_traceback <- err$message
    errormessage = paste(paste("at ", functionName, " :: ", step, sep = ""), message_traceback, sep = "\n")
    stop(errormessage)
  })
}


