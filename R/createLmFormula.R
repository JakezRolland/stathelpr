#' createLmFormula
#'
#' @param response Responsable variable in string
#' @param explicatives  Explicative variables in string array
#' @import stats
#' @return
#' @export
#' @examples
createLmFormula<-function(response,explicatives){
  functionName<-match.call()[[1]]
  step<-"Start"
  tryCatch({
    StringFormula = paste("`",response,"`~`",paste(explicatives,collapse="`+`"),"`",sep="")
    return(as.formula(StringFormula))
  }, error = function(err) {
    message_traceback <- err$message
    errormessage = paste(paste("at ", functionName, " :: ", step, sep = ""), message_traceback, sep = "\n")
    stop(errormessage)
    })
}
