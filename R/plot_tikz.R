#' Turn an R plot into a beautiful pdf made by LaTeX and TikZ,
#' using the tikzDevice package
#' @export
smv_plot_tikz = function(plot = NULL,
                         expression = NULL,
                         file = "Rplots.pdf",
                         extra_packages = NULL,
                         tex_engine = c("pdflatex", "lualatex"),
                         ...) {
  expression = substitute(expression)
  if (is.null(plot) && is.null(expression)) {
    stop("Either `plot` or `expression` must be non-NULL")
  }

  # Create a temporary file for the tikz-output
  tmp = tempfile(tmpdir = getwd())
  # Clean up after yourself on early interrupt
  on.exit(unlink(tmp))

  # I am nor sure about what is going on here...
  opt = options()
  on.exit(options(opt), add = TRUE) #Reset global options on exit
  tikzDevice::setTikzDefaults(overwrite = FALSE)

  # Extract default tex usepackages and extra packages
  tex_packages = c(options()$tikzLatexPackages, "\\usepackage[utf8]{inputenc}")
  extra_packages = c(extra_packages, "bm", "amsmath", "amssymb")
  extra_packages = paste0("\\usepackage{", extra_packages, "}\n")
  tex_packages = unique(c(tex_packages, extra_packages))

  # Open a device for creating a tex-file
  tikzDevice::tikz(tmp, standAlone = TRUE, packages = tex_packages, ...)
  # Call dev.off() on exit in case of interruptions
  current_device = dev.cur()
  on.exit({
    if (current_device %in% dev.list()) dev.off(current_device)
  }, add = TRUE)

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
  smv_run(tex_engine[1], shQuote(tmp))

  # Copy pdf file to final destination
  file.copy(paste0(tmp, ".pdf"), file, overwrite = TRUE)

  # Clean up all temporary files
  tmp_filename = tail(strsplit(tmp, "/")[[1]], 1)
  files_to_clean = grep(tmp_filename, list.files(full.names = TRUE), value = TRUE)
  unlink(files_to_clean)

  # Silently return a TRUE to show that the function completed successfully
  invisible(TRUE)
}
