".First.lib" <- function(library, section)
{ cat("Locfit for Splus 3.3-3.4.\n")
  cat("Aug 1, 2001.\n")
  cat("Copyright Lucent Technologies.\n")
  dyn.load.shared(paste(library, section, "locfit.so", sep = "/"))
}
