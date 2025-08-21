# library(fs)
#
# cache_dir <- "cache"
# dir_create(cache_dir)  # make sure it's there, even though it's mounted
#
# remote_url <- "https://your-host/meta.duckdb"
# remote_sha_url <- "https://your-host/meta.duckdb.sha256"
#
# local_db <- path(cache_dir, "meta.duckdb")
# local_sha <- path(cache_dir, "meta.duckdb.sha256")
#
# remote_hash <- readLines(remote_sha_url)
# local_hash <- if (file_exists(local_sha)) readLines(local_sha) else ""
#
# if (!identical(remote_hash, local_hash)) {
#   message("🔄 Downloading updated meta.duckdb...")
#   download.file(remote_url, local_db, mode = "wb")
#   writeLines(remote_hash, local_sha)
# } else {
#   message("✅ meta.duckdb is already up-to-date.")
# }
#
# # Connect and proceed
# con <- DBI::dbConnect(duckdb::duckdb(), dbdir = local_db, read_only = TRUE)
