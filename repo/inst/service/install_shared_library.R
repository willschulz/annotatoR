#!/usr/bin/env Rscript

# Publish annotatoR and its non-base dependency closure to a collaborator-
# readable library. Run after installing the service package in the operator's
# normal R library, which is the authoritative source for this copy.

args <- commandArgs(trailingOnly = TRUE)
shared_lib <- if (length(args) >= 1L) {
  args[[1]]
} else {
  "/srv/projects/tools/annotatoR/r-library/4.5"
}
refresh_dependencies <- length(args) >= 2L &&
  identical(args[[2]], "--refresh-dependencies")

dir.create(shared_lib, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(shared_lib)) {
  stop("Could not create shared R library: ", shared_lib)
}
Sys.chmod(shared_lib, mode = "0755")

installed <- installed.packages()
if (!"annotatoR" %in% rownames(installed)) {
  stop("annotatoR must be installed before publishing the shared library")
}

dependencies <- tools::package_dependencies(
  "annotatoR",
  db = installed,
  recursive = TRUE
)[[1]]
packages <- unique(c("annotatoR", dependencies))
packages <- packages[packages %in% rownames(installed)]

# Base and recommended packages are already available from the system R
# installation and should not be duplicated in the shared library.
priority <- installed[packages, "Priority"]
packages <- packages[is.na(priority) | !nzchar(priority)]

publish_package <- function(package) {
  source <- find.package(package)
  destination <- file.path(shared_lib, package)
  if (package != "annotatoR" && dir.exists(destination) &&
      !refresh_dependencies) {
    message("retained ", package)
    return(invisible(NULL))
  }
  staging <- file.path(
    shared_lib,
    sprintf(".%s-new-%d", package, Sys.getpid())
  )
  backup <- file.path(
    shared_lib,
    sprintf(".%s-old-%d", package, Sys.getpid())
  )

  unlink(staging, recursive = TRUE, force = TRUE)
  unlink(backup, recursive = TRUE, force = TRUE)
  dir.create(staging)

  entries <- list.files(
    source,
    all.files = TRUE,
    no.. = TRUE,
    full.names = TRUE
  )
  copied <- file.copy(
    entries,
    staging,
    recursive = TRUE,
    copy.mode = TRUE,
    copy.date = TRUE
  )
  if (!all(copied)) {
    unlink(staging, recursive = TRUE, force = TRUE)
    stop("Failed to stage shared R package: ", package)
  }

  # Preserve required execute bits and remove ordinary write bits. The deploy
  # wrapper additionally applies a recursive immutable flag on TrueNAS because
  # this NFS export maps all clients to the same owning UID.
  status <- system2("chmod", c("-R", "a+rX,go-w", staging))
  if (!identical(status, 0L)) {
    unlink(staging, recursive = TRUE, force = TRUE)
    stop("Failed to set shared package permissions: ", package)
  }

  if (dir.exists(destination) &&
      !file.rename(destination, backup)) {
    unlink(staging, recursive = TRUE, force = TRUE)
    stop("Failed to stage existing shared package: ", package)
  }

  if (!file.rename(staging, destination)) {
    if (dir.exists(backup)) file.rename(backup, destination)
    unlink(staging, recursive = TRUE, force = TRUE)
    stop("Failed to publish shared R package: ", package)
  }

  unlink(backup, recursive = TRUE, force = TRUE)
  message("published ", package)
}

for (package in packages) publish_package(package)

manifest <- data.frame(
  package = packages,
  version = installed[packages, "Version"],
  published_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  stringsAsFactors = FALSE
)
manifest_path <- file.path(shared_lib, "MANIFEST.tsv")
utils::write.table(
  manifest,
  manifest_path,
  sep = "\t",
  row.names = FALSE,
  quote = FALSE
)
Sys.chmod(manifest_path, mode = "0644")

message(
  "Published ", nrow(manifest), " packages to ", shared_lib
)
