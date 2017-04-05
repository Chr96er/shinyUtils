#'@export
incVersion <-
  function(directory,
           level = c("build", "feature", "stable", "release"),
           resetSubLevel = T,
           build = T,
           version = F) {
    level = level[1]
    if (!version) {
      versionFile <-
        dir(directory)[grep(dir(directory), pattern = "VERSION")]
      versionFileParts <-
        strsplit(gsub(".VERSION", replacement = "", versionFile), "-")[[1]]
      version <- strsplit(versionFileParts[1], "\\.")[[1]]
      releaseVersion <- as.numeric(version[1])
      stableVersion <- as.numeric(version[2])
      featureVersion <- as.numeric(version[3])
      buildVersion <- as.numeric(version[4])
      if (build) {
        build = sample(1:99999, 1)
      }
      version = switch(
        level,
        build   = paste(releaseVersion, stableVersion, featureVersion  , build, sep = "."),
        feature   = paste(releaseVersion, stableVersion, featureVersion + 1, build, sep = "."),
        stable   = paste(releaseVersion, stableVersion + 1, (!resetSubLevel) * featureVersion, build, sep = "."),
        release = paste(releaseVersion + 1, (!resetSubLevel) * stableVersion, (!resetSubLevel) * featureVersion, build, sep = ".")
      )
    }
    file.rename(
      paste0(directory, "/", versionFile),
      paste0(directory, "/", version, "-", versionFileParts[2], ".VERSION")
    )
  }

#'@export
setDeploymentTime <-
  function(directory,
           datetimeString = paste0(strftime(as.POSIXlt(Sys.time()), format = "%Y.%m.%d.%H.%M.%S#"),
                                   gsub("/", ".", Sys.timezone()))) {
    versionFile <-
      dir(directory)[grep(dir(directory), pattern = "VERSION")]
    versionFileParts <-
      strsplit(gsub(".VERSION", replacement = "", versionFile), "-")[[1]]
    file.rename(
      paste0(directory, "/", versionFile),
      paste0(
        directory,
        "/", 
        versionFileParts[1],
        "-",
        datetimeString,
        ".VERSION"
      )
    )
  }