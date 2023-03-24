update_shared <- function() {
  file.copy("_shared/", "pubbias/app/www/", overwrite = TRUE, recursive = TRUE)
  file.copy("_shared/", "phacking/app/www/", overwrite = TRUE, recursive = TRUE)
}
