#' Turn an R plot into a beautiful pdf made by LaTeX and TikZ,
#' using the tikzDevice package
#' @export
smv_plot_tikz = function(plot = NULL, expression = NULL, file = "Rplots.pdf", ...) {
  expression = substitute(expression)
  if (is.null(plot) && is.null(expression)) {
    stop("Either `plot` or `expression` must be non-NULL")
  }

  # Create a temporary file for the tikz-output
  tmp = tempfile(tmpdir = getwd())
  # Clean up after yourself on early interrupt
  on.exit(suppressWarnings(file.remove(tmp)), add = TRUE)

  # Extract default tex usepackages and add the bm,amsmath and amssymb packages
  opt = options()
  on.exit(options(opt)) #Reset global options on exit
  tikzDevice::setTikzDefaults(overwrite = FALSE)
  tex_packages = options()$tikzLatexPackages
  if (!any(grepl("usepackage\\{bm\\}", tex_packages))) {
    tex_packages = c(tex_packages, "\\usepackage{bm}\n")
  }
  if (!any(grepl("usepackage\\{amsmath\\}", tex_packages))) {
    tex_packages = c(tex_packages, "\\usepackage{amsmath}\n")
  }
  if (!any(grepl("usepackage\\{amssymb\\}", tex_packages))) {
    tex_packages = c(tex_packages, "\\usepackage{amssymb}\n")
  }

  # Open a device for creating a tex-file
  tikzDevice::tikz(tmp, standAlone = TRUE, packages = tex_packages, ...)
  # Call dev.off() on exit in case of interruptions
  current_device = dev.cur()
  on.exit({
    if (current_device %in% dev.list()) dev.off(current_device)
  })

  # Plot something into the tex-file
  if (!is.null(plot)) {
    if (any(class(plot) %in% c("gg", "ggplot", "patchwork"))) {
      print(plot)
    } else {
      for (p in plot) print(p)
    }
  } else {
    eval(expression)
  }

  # Finish the creation of the tex-file
  dev.off()

  # Compile to pdf using lualatex
  smv_run("lualatex", shQuote(tmp))

  # Copy pdf file to final destination
  file.copy(paste0(tmp, ".pdf"), file, overwrite = TRUE)

  # Clean up all temporary files
  tmp_filename = tail(strsplit(tmp, "/")[[1]], 1)
  files_to_clean = grep(tmp_filename, list.files(full.names = TRUE), value = TRUE)
  unlink(files_to_clean)

  # Silently return a TRUE to show that the function completed successfully
  invisible(TRUE)
}
