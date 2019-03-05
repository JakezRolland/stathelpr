#' getModelVariablesFromName
#'
#' @param modelName Modele name to extract informations from
#'
#' @return
#' @export
#'
#' @examples
getModelParametersFromModelName <- function(modelName) {
  functionName<-match.call()[[1]]
  step<-"Start"
  tryCatch({
    if(length(modelName)==1){
      response = strsplit(modelName,"~")[[1]][1]
      explicatives = strsplit(modelName,"~")[[1]][2]
      explicatives = strsplit(explicatives,"+",fixed=TRUE)[[1]]
      return(list(response=response,explicatives=explicatives))
    }
    if(length(modelName)>1){
      print("data.frame")
      responses = c()
      response = strsplit(modelName,"~")
      for (k in response){
        responses=c(responses,k[1])
      }

      return(list(response=responses,explicatives=explicatives))
    }

  }, error = function(err) {
    message_traceback <- err$message
    errormessage = paste(paste("at ", functionName, " :: ", step, sep = ""), message_traceback, sep = "\n")
    stop(errormessage)
  })
}
