
#' Convert a pdf to png or jpeg
#' @export
smv_pdf_convert = function(in_path,
                           out_paths,
                           format = c("png", "jpeg"),
                           pages = NULL,
                           dpi = 150,
                           ...) {
  suppressWarnings({
    pdftools::pdf_convert(
      pdf = in_path,
      filenames = out_paths,
      format = match.arg(format),
      dpi = dpi,
      ...
    )
  })
}
