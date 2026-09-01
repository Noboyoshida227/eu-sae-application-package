# ============================================================
# R/zip_permissions.R
#
# Set Unix executable bits on selected entries inside a ZIP archive.
#
# Why this exists
# ---------------
# A ZIP built on Windows records no Unix permission bits. When such an
# archive is extracted on macOS or Linux, every file arrives without the
# executable flag, so Finder refuses to run the .command launchers and
# double-clicking them does nothing. This rewrites the "external file
# attributes" field of the archive's central directory so the shipped
# launchers extract as executable on Unix, while leaving Windows behaviour
# and every byte of file content untouched.
#
# Only the central directory is modified. Local file headers carry no
# permission information, and no compressed data is rewritten, so entry
# offsets stay valid and the archive remains bit-identical apart from the
# attribute words.
# ============================================================

# Locate the End Of Central Directory record and return its offset (0-based).
.sae_zip_find_eocd <- function(raw_bytes) {
  sig <- as.raw(c(0x50, 0x4b, 0x05, 0x06))
  n <- length(raw_bytes)
  # The EOCD is at most 22 bytes plus a 65535-byte comment.
  start <- max(1L, n - 22L - 65535L)
  for (i in seq(n - 21L, start, by = -1L)) {
    if (identical(raw_bytes[i:(i + 3L)], sig)) return(i - 1L)
  }
  stop("Not a ZIP archive, or the end-of-central-directory record is missing.")
}

.sae_rd_u16 <- function(b, off) {
  as.integer(b[off + 1L]) + as.integer(b[off + 2L]) * 256L
}
.sae_rd_u32 <- function(b, off) {
  as.numeric(b[off + 1L]) + as.numeric(b[off + 2L]) * 256 +
    as.numeric(b[off + 3L]) * 65536 + as.numeric(b[off + 4L]) * 16777216
}
.sae_wr_u32 <- function(value) {
  value <- as.numeric(value)
  as.raw(c(value %% 256,
           (value %/% 256) %% 256,
           (value %/% 65536) %% 256,
           (value %/% 16777216) %% 256))
}

#' Mark entries inside a ZIP archive executable on Unix.
#'
#' @param archive Path to the .zip file, modified in place.
#' @param patterns Character vector of regular expressions matched against
#'   each entry's stored path. Matching entries get mode 0755; every other
#'   entry is left exactly as it was.
#' @param mode File mode as an OCTAL STRING, default "755". R has no octal
#'   literals, so the mode is given as text and converted with strtoi().
#' @return Character vector of the entry names that were changed, invisibly.
sae_set_zip_exec_bits <- function(archive,
                                  patterns = c("\\.command$", "\\.sh$"),
                                  mode = "755") {
  if (!file.exists(archive)) stop("Archive not found: ", archive)
  mode_int <- if (is.character(mode)) strtoi(mode, base = 8L) else as.integer(mode)
  if (is.na(mode_int) || mode_int < 0L || mode_int > 4095L) {
    stop("Invalid file mode: ", mode, " (give an octal string such as \"755\").")
  }
  size <- file.info(archive)$size
  b <- readBin(archive, what = "raw", n = size)
  if (length(b) != size) stop("Short read on archive: ", archive)

  eocd <- .sae_zip_find_eocd(b)
  total <- .sae_rd_u16(b, eocd + 10L)          # entries in central dir
  cd_size <- .sae_rd_u32(b, eocd + 12L)        # size of central dir
  cd_off <- .sae_rd_u32(b, eocd + 16L)         # offset of central dir
  if (.sae_rd_u16(b, eocd + 4L) != 0L || .sae_rd_u16(b, eocd + 6L) != 0L) {
    stop("Multi-disk ZIP archives are not supported.")
  }
  # Zip64 sentinels. This package's archives are far below any Zip64 threshold,
  # so rather than implement the extension, refuse loudly instead of writing
  # into the wrong offset.
  if (total == 0xFFFFL || cd_off >= 0xFFFFFFFF || cd_size >= 0xFFFFFFFF) {
    stop("Zip64 archives are not supported by sae_set_zip_exec_bits().")
  }
  if (cd_off + cd_size > length(b)) stop("Central directory extends past end of file.")

  # 0x100000 is S_IFREG; the mode occupies the high 16 bits of external attrs.
  # The low byte keeps the MS-DOS attribute so Windows tools stay happy.
  unix_attr <- (bitwOr(0x8000L, mode_int) * 65536)

  cd_sig <- as.raw(c(0x50, 0x4b, 0x01, 0x02))
  pos <- cd_off
  changed <- character(0)

  for (i in seq_len(total)) {
    if (!identical(b[(pos + 1L):(pos + 4L)], cd_sig)) {
      stop("Corrupt central directory at entry ", i, " (bad signature).")
    }
    name_len  <- .sae_rd_u16(b, pos + 28L)
    extra_len <- .sae_rd_u16(b, pos + 30L)
    cmt_len   <- .sae_rd_u16(b, pos + 32L)
    name <- rawToChar(b[(pos + 46L + 1L):(pos + 46L + name_len)])
    Encoding(name) <- "UTF-8"

    is_dir <- grepl("/$", name)
    if (!is_dir && any(vapply(patterns, function(p) grepl(p, name), logical(1)))) {
      # external file attributes sit at offset 38 within the header
      b[(pos + 38L + 1L):(pos + 38L + 4L)] <- .sae_wr_u32(unix_attr)
      # creator version high byte 3 == Unix, so extractors read the mode
      b[pos + 6L] <- as.raw(3L)
      changed <- c(changed, name)
    }
    pos <- pos + 46L + name_len + extra_len + cmt_len
  }

  if (pos != cd_off + cd_size) {
    stop("Central directory walk ended at an unexpected offset; archive not modified.")
  }

  if (length(changed)) {
    con <- file(archive, open = "wb")
    writeBin(b, con)
    close(con)
    # Read the archive back and confirm the bits really are set, so a silent
    # mis-parse cannot ship. The caller's own re-extraction check follows.
    check <- readBin(archive, what = "raw", n = file.info(archive)$size)
    if (!identical(check, b)) stop("Archive readback differs from what was written.")
  }
  invisible(changed)
}
