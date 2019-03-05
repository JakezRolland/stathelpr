#' modelIsSignificant
#' @param model Model linear to test significativity
#' @return TRUE if model is significant, FALSE else
#' @export
#'
#' @examples
modelIsSignificant<-function(model){
  functionName<-match.call()[[1]]
  step<-"Start"
  tryCatch({

    if(class(model)=='lm'){
      summary = summary(model)
      pvalues = summary$coefficients[,'Pr(>|t|)']
      onePvaluesUnderTreshold = (FALSE%in%(pvalues<0.05))
      return(!onePvaluesUnderTreshold)
    }
    if(class(model)=="AovSum"){
      summary = model$Ftest;
      fvalues = summary$`Pr(>F)`;
      fvalues<-fvalues[!is.na(fvalues)]
      onePvaluesUnderTreshold = (FALSE%in%(fvalues<0.05))
      return(!onePvaluesUnderTreshold)

    }

  }, error = function(err) {
    message_traceback <- err$message
    errormessage = paste(paste("at ", functionName, " :: ", step, sep = ""), message_traceback, sep = "\n")
    stop(errormessage)
  })
}
