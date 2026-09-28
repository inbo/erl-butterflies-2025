#' Save a ggplot in multiple file formats
#'
#' Saves a ggplot to one or more file formats using [ggplot2::ggsave()].
#' The output filename is constructed from `path`, `file_name`, and each
#' file extension specified in `devices`.
#'
#' @param plot A ggplot object to save.
#' @param file_name A character string giving the base name of the output
#'   file, without a file extension.
#' @param path A character string giving the directory in which to save
#'   the files.
#' @param devices A character vector of file formats supported by
#'   [ggplot2::ggsave()], such as `"png"` or `"pdf"`.
#' @param ... Additional arguments passed to [ggplot2::ggsave()].
#'
#' @return A character vector of the paths to the saved files, invisibly.
#'
save_figure <- function(plot, file_name, path, devices, ...) {
  sapply(devices, function(dev) {
    ggplot2::ggsave(
      filename = paste0(file.path(path, file_name), ".", dev),
      plot = plot,
      ...
    )
  })
}
