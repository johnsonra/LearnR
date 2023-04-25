#' compare_source_code
#' Compare source code files
#'
#' @param f1 Vector of paths to files containing code chunks from solution 1 (e.g. student submissions)
#' @param f2 Vector of paths to files containing code chunks from solution 2 (e.g. example solutions)
#'
#' @return A vector of similarity measures (between 0 and 1) for each pair of files in the list.
#'
#' @references https://cran.r-project.org/web/packages/rscc/vignettes/rscc.html
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
  check_files(f1, f2)

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

#' compare_text
#' Compare text files
#'
#' @param f1 Vector of paths to files containing code chunks from solution 1 (e.g. student submissions)
#' @param f2 Vector of paths to files containing code chunks from solution 2 (e.g. example solutions)
#'
#' @return A vector of distances for each pair of files in the list.
#'
#' @details Provided a sufficient sample of distances from other students, the results of this function can
#' be used to estimate the percentile of the provided answer compared to answers provided by other students.
#'
#' @references http://fredgibbs.net/tutorials/document-similarity-with-r.html
#'
#' @export
#' @import tm
#' @importFrom stats dist
compare_text <- function(f1, f2)
{
  check_files(f1, f2)

  # make sure environmental variables are properly set
  Sys.setenv(NOAWT=TRUE)

  # calculate result for each file pair
  retval <- numeric(length(f1))
  for(i in 1:length(f1))
  {
    # if both solutions contain only source code, text portion is a perfect match (distance = 0)
    if(f1[i] == '' & f2[i] == '')
    {
      retval[i] <- 0
      next
    }

    # if only one pair contains text, return distance of Inf
    if((f1[i] == '' & f2[i] != '') |
       (f1[i] != '' & f2[i] == ''))
    {
      retval[i] <- Inf
      next
    }

    # split paths
    paths <- list(strsplit(f1[i], '/')[[1]],
                  strsplit(f2[i], '/')[[1]])

    # set up corpus (grab paths to each file, minus the file names on the end)
    corp_dirs <- sapply(paths, function(.x) paste(.x[-length(.x)], collapse = '/')) %>%
      unique()

    # regular expression that will only pull the desired files out of corp_dirs
    files_to_pick <- sapply(paths, function(.x) .x[length(.x)]) %>%
      paste(collapse = '|')

    invisible(capture.output({
      retval[i] <-
        # set up corpus
        tm::DirSource(directory = corp_dirs,
                      pattern = files_to_pick) %>%
        tm::Corpus() %>%

        # process text
        tm::tm_map(tm::removePunctuation) %>%
        tm::tm_map(tm::removeWords, tm::stopwords("english")) %>%
        tm::tm_map(tm::stemDocument) %>%
        tm::DocumentTermMatrix() %>%

        # create and scale term matrix
        tm::inspect() %>%
        as.data.frame() %>%
        #scale() %>% # leaving this out for now - short answers sometimes don't scale well (lots of NAs - and we are dealing with a lot of short answers)

        # cluster terms
        dist(method = 'euclidean')
    }))
  }

  return(retval)
}


#' check_files
#' Run basic checks on files for compare_source_code and compare_text
#'
#' @param f1 Vector of paths to files containing code chunks from solution 1 (e.g. student submissions)
#' @param f2 Vector of paths to files containing code chunks from solution 2 (e.g. example solutions)
#'
#' @return TRUE if it passes - throw an error otherwise
check_files <- function(f1, f2)
{
  if(length(f1) != length(f2))
    stop("Expecting lengths of f1 and f2 to be equal")

  if(!all(file.exists(f1) | f1 == '') |
     !all(file.exists(f2) | f2 == ''))
    stop("Some file paths do not exist on the system")

  invisible(TRUE)
}