# authenticate.R

# Sys.setenv("DATABRICKS_HOST" = "your-databricks-host.com")
# Sys.setenv("DATABRICKS_TOKEN" = "your-databricks-token")
con <-
  dbConnect(odbc::databricks(), httpPath = Sys.getenv("HTTP_PATH"))
