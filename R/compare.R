#' compare_source_code
#' Compare source code files
#'
#' @param f1 Vector of paths to files containing code chunks from solution 1 (e.g. student submissions)
#' @param f2 Vector of paths to files containing code chunks from solution 2 (e.g. example solutions)
#'
#' @return A vector of similarity measures (between 0 and 1) for each pair of files in the list.
#'
#' @export
#' @importFrom magrittr %>%
#'
#' @importFrom rscc sourcecode
#' @importFrom rscc documents
#' @importFrom rscc similarities
#' @importFrom rscc matrix2dataframe
#'
#' @importFrom utils capture.output
compare_source_code <- function(f1, f2)
{
  # check f1 and f2
  if(length(f1) != length(f2))
    stop("Expecting lengths of f1 and f2 to be equal")

  if(!all(file.exists(f1) | f1 == '') |
     !all(file.exists(f2) | f2 == ''))
    stop("Some file paths do not exist on the system")

  # calculate results for each file pair
  retval <- numeric(length(f1))
  for(i in 1:length(f1))
  {
    # if both solutions contain only text answers, code portion is a perfect match
    if(f1[i] == '' & f2[i] == '')
    {
      retval[i] <- 1
      next
    }

    # if only one pair contains code, return 0
    if((f1[i] == '' & f2[i] != '') |
       (f1[i] != '' & f2[i] == ''))
    {
      retval[i] <- 0
      next
    }

    # check vars
    invisible(capture.output( # deal with annoying print statements in rscc package
      vars <- withCallingHandlers(tryCatch({
        sourcecode(c(f1[i], f2[i])) %>%
          documents(type = 'vars') %>%
          similarities() %>%
          matrix2dataframe()
        },
        warning = function(w){
          # a warning will be thrown if there isn't anything to compare - return NA
          return(data.frame(row = 'f1', col = 'f2', jaccard = NA))
        }))$jaccard
    ))

    # check funs
    invisible(capture.output( # deal with annoying print statements in rscc package
      funs <- withCallingHandlers(tryCatch({
        sourcecode(c(f1[i], f2[i])) %>%
          documents(type = 'funs') %>%
          similarities() %>%
          matrix2dataframe()
      },
      warning = function(w){
        # a warning will be thrown if there isn't anything to compare - return NA
        return(data.frame(row = 'f1', col = 'f2', jaccard = NA))
      }))$jaccard
    ))

    # check names
    invisible(capture.output( # deal with annoying print statements in rscc package
      nms <- withCallingHandlers(tryCatch({
        sourcecode(c(f1[i], f2[i])) %>%
          documents(type = 'names') %>%
          similarities() %>%
          matrix2dataframe()
      },
      warning = function(w){
        # a warning will be thrown if there isn't anything to compare - return NA
        return(data.frame(row = 'f1', col = 'f2', jaccard = NA))
      }))$jaccard
    ))

    retval[i] <- mean(c(vars, funs, nms), na.rm = TRUE)
  }

  return(retval)
}