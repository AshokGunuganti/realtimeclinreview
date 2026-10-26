# Export spec inputs from the code (data checks, required source columns) so the
# spec is rebuilt from the implementation:  Rscript specs/export_spec_inputs.R && python3 specs/build_spec.py specs/inputs
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)
write.csv(CHECK_DEFS, "specs/inputs/checks.csv", row.names = FALSE)
req <- do.call(rbind, lapply(names(SOURCE_SPECS), function(k) data.frame(FILE = SOURCE_SPECS[[k]]$file, VAR = SOURCE_SPECS[[k]]$cols)))
write.csv(req, "specs/inputs/required.csv", row.names = FALSE)
cat("Spec inputs written to specs/inputs\n")
