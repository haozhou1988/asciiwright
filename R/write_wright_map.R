resolve_wright_output_format <- function(file, format = c("auto", "txt", "md")) {
  format <- match.arg(format)

  if (format != "auto") {
    return(format)
  }

  ext <- tolower(tools::file_ext(file))
  if (ext %in% c("md", "markdown", "qmd")) {
    "md"
  } else {
    "txt"
  }
}

#' Write an ASCII Wright map or score table to a text or Markdown file
#'
#' @param x An object created by [wright_map_ascii()] or [score_table_ascii()].
#' @param file Output path.
#' @param format Output format. `"auto"` infers from the file extension.
#' @param append Whether to append to an existing file.
#' @param code_fence When writing Markdown, wrap the map in a fenced code block.
#' @return Invisibly returns the output path.
#' @export
write_wright_map <- function(
    x,
    file,
    format = c("auto", "txt", "md"),
    append = FALSE,
    code_fence = TRUE
) {
  if (!inherits(x, c("ascii_wright_map", "ascii_score_table"))) {
    stop("'x' must be an 'ascii_wright_map' or 'ascii_score_table' object.", call. = FALSE)
  }

  resolved_format <- resolve_wright_output_format(file, format = format)
  lines <- format(x)

  output <- switch(
    resolved_format,
    txt = lines,
    md = {
      if (isTRUE(code_fence)) {
        c("```text", lines, "```")
      } else {
        lines
      }
    }
  )

  con <- file(file, open = if (isTRUE(append)) "a" else "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(output, con = con, sep = "\n", useBytes = TRUE)

  invisible(file)
}
