#'@export
incVersion <-
  function(directory,
           level = c("build", "feature", "test", "release"),
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
      if (build) {
        build = sample(1:99999, 1)
      }
      version = switch(
        level,
        build   = paste(version[1], version[2], version[3]    , build, sep = "."),
        minor   = paste(version[1], version[2], as.numeric(version[3]) + 1, build, sep = "."),
        major   = paste(version[1], as.numeric(version[2]) + 1, (!resetSubLevel) * version[3], build, sep = "."),
        release = paste(as.numeric(version[1]) + 1, (!resetSubLevel) * version[2], (!resetSubLevel) * version[3], build, sep = ".")
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