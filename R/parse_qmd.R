#' parse_qmd
#' Extract R code and text from qmd file and export to temporary .R and .txt files
#'
#' @param fin Path to an input file
#' @return Returns a table with paths to the temporary .R and .txt files, with one
#'  row per challenge problem.
#'
#' @details This assumes submitted answers to challenge problems are encapsulated
#'  with tags starting with "<!-- Challenge", and that R code chunks start and end
#'   with "```". All other lines in the submitted answer are assumed to be text.
#'
#' @export
parse_qmd <- function(fin)
{
  # variables for keeping track of portions to keep and which lines are code
  parse_text <- data.frame(qmd = readLines(fin, warn = FALSE), # read in source code
                           code = logical(1),                  # these lines are code to keep
                           text = logical(1),                  # these lines are text to keep
                           challenge = NA)                     # this identifies the challenge

  challenge <- NA                   # current challenge
  keep <- FALSE                     # current line contains an answer
  in_code_chunk <- FALSE            # currently in a code chunk

  # detect R code and text chunks
  for(i in 1:nrow(parse_text))
  {
    # watch for code chunks (or when they end)
    if(substr(parse_text$qmd[i], 1, 3) == '```')
    {
      in_code_chunk <- !in_code_chunk
      next
    }

    # watch for sections to keep (or when they end)
    if(substr(parse_text$qmd[i], 1, 14) == "<!-- Challenge")
    {
      keep <- !keep
      challenge <- strsplit(parse_text$qmd[i], ' ', fixed = TRUE)[[1]][3]
      next
    }

    # if we are keeping this line, mark it as either code or text
    if(keep)
    {
      parse_text$code[i] <- in_code_chunk
      parse_text$text[i] <- !in_code_chunk
      parse_text$challenge[i] <- challenge
    }
  }

  # keep only non-blank lines of answers
  parse_text <- subset(parse_text, (parse_text$code | parse_text$text) & parse_text$qmd != '')

  # split out R code and text chunks for each challenge
  retval <- data.frame(challenge = unique(parse_text$challenge), # challenge
                       f_code = character(1),                    # path to code answers
                       f_text = character(1))                    # path to text answers
  for(i in 1:nrow(retval))
  {
    # get challenge i from parse_text
    sub <- subset(parse_text, challenge == retval$challenge[i])

    # write any code to temp file
    if(any(sub$code))
    {
      retval$f_code[i] <- tempfile(fileext = '.R')
      cat(sub$qmd[sub$code], file = retval$f_code[i], sep = '\n')
    }

    # write any text to temp file
    if(any(sub$text))
    {
      retval$f_text[i] <- tempfile(fileext = '.txt')
      cat(sub$qmd[sub$text], file = retval$f_text[i], sep = '\n')
    }
  }

  return(retval)
}
