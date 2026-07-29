meridian_db_config <- function() {
  list(
    host=Sys.getenv("DB_HOST"), port=Sys.getenv("DB_PORT", "5432"), dbname=Sys.getenv("DB_NAME"),
    user=Sys.getenv("DB_USER"), password=Sys.getenv("DB_PASSWORD"), sslmode=Sys.getenv("DB_SSLMODE", "require")
  )
}
meridian_db_ready <- function() {
  cfg <- meridian_db_config(); all(nzchar(c(cfg$host,cfg$dbname,cfg$user,cfg$password)))
}
meridian_db_connect <- function() {
  if (!meridian_db_ready()) return(NULL)
  if (!requireNamespace("DBI", quietly=TRUE) || !requireNamespace("RPostgres", quietly=TRUE)) return(NULL)
  cfg <- meridian_db_config()
  DBI::dbConnect(RPostgres::Postgres(), host=cfg$host, port=as.integer(cfg$port), dbname=cfg$dbname, user=cfg$user, password=cfg$password, sslmode=cfg$sslmode)
}
