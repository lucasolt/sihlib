#' Get (or create) a Google Drive folder
#'
#' Returns the Drive folder called `nome`, inside `pai` or at the root of
#' My Drive. Creates it when it does not exist.
#'
#' @param nome Folder name.
#' @param pai Parent folder (a `dribble`, id or path), or `NULL` for the root.
#' @return A one-row `dribble` with the folder.
#' @export
garantir_pasta <- function(nome, pai = NULL) {
  precisa_drive()
  if (is.null(pai)) {
    achou <- googledrive::drive_get(nome)
  } else {
    achou <- googledrive::drive_ls(pai, type = "folder")
    achou <- achou[achou$name == nome, ]
  }
  achou <- achou[googledrive::is_folder(achou), ]
  if (nrow(achou) > 0) return(achou[1, ])
  googledrive::drive_mkdir(nome, path = pai)
}

#' Upload a local folder (with subfolders) to Google Drive
#'
#' Creates a folder named like `local` inside `pai` and uploads every file,
#' recursively. Existing files with the same name are overwritten.
#'
#' @param local Path of the local folder.
#' @param pai Parent folder on Drive (a `dribble`, id or path).
#' @return The destination folder (`dribble`), invisibly.
#' @export
subir_pasta <- function(local, pai) {
  precisa_drive()
  destino <- garantir_pasta(basename(local), pai = pai)
  for (f in list.files(local, full.names = TRUE)) {
    if (dir.exists(f)) {
      subir_pasta(f, destino)
    } else {
      googledrive::drive_upload(f, path = destino, overwrite = TRUE)
    }
  }
  invisible(destino)
}
