".First.lib" <- function(library, section)
{
  dyn.load(paste(library, section, "locfit.obj", sep = "\\"))
  cat("Locfit for Splus 4.0,4.5,2000.\n")
  cat("Aug 1, 2001.\n")
  cat("Copyright Lucent Technologies.\n")
  cat("For documentation, visit\n")
  cat("  http://cm.bell-labs.com/stat/project/locfit/\n")
  invisible(NULL)
}
