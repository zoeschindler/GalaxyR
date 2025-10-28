# install.packages(c("httr", "jsonlite"))
# library(httr)
# library(jsonlite)

#source("C:/Users/Frey/Desktop/2025-10-27_JF_R_Galaxy_functions.R")

# ---- 0. set your credentials ----
# edit your ~/.Renviron or C:/Users/<user>/Documents/.Renviron file and add
# GALAXY_API_KEY="key"
# GALAXY_USERNAME="usr"
# GALAXY_PASSWORD="passwd"

# ---- CONFIGURATION ----
workflow_id <- "ca9b55a020b0161b" #SAT + DetailView "0b835e2f3f14627e"
input_file <- "C:/TLS/docker/input/2022-02-28 breisach mmes.laz"

# # ---- 1. Get or create a history ----
# history_id <- galaxy_initialize()
#
# # ---- 2. Upload dataset into that history ----
# dataset_id <- galaxy_upload(input_file)
#
# # --- 2. Run workflow ---
# invocation_id <- galaxy_start_workflow(dataset_id, workflow_id)
#
# # --- 3. Poll for completion ---
# output_ids <- galaxy_poll_workflow(invocation_id, poll_interval = 30)
#
# # --- 4. Download result dataset ---
# result_file <- galaxy_download_result(output_ids, "C:/TLS/test.laz")

# LAS Catalog example

catalog <- lidR::readTLScatalog(input_file)
lidR::opt_chunk_size(catalog) <- 45
lidR::opt_chunk_buffer(catalog) <- 10
lidR::plot(catalog)
lidR::opt_output_files(catalog) <- paste0("C:/TLS/test_galaxy_lidr/{ID}")
catalog_function <- function(cluster) {

  las <- suppressWarnings(lidR::readLAS(cluster)) # read files
  if (lidR::is.empty(las) ) return(NULL) # stop if empty
  message(str(cluster))
  # get the bbox bbbox
  # the bbox includes the buffer, the bbbox excludes the buffer
  bbox <- cluster@bbox
  bbbox <- cluster@bbbox

  # get the TileID
  tile_id <- as.numeric(basename(cluster@save))

  # write the cluster to a tmp file
  tmpfile <- tempfile(fileext = ".laz")
  lidR::writeLAS(las, tmpfile)

  # initialize a history
  history_id <- galaxy_initialize(paste0("tile_", tile_id))


  # upload the file
  dataset_id <- galaxy_upload(tmpfile, history_id)

  # remove the tmpfile
  file.remove(tmpfile)

  # start workflow
  invocation_id <- galaxy_start_workflow(dataset_id, workflow_id)

  # --- 3. Poll for completion ---
  output_ids <- galaxy_poll_workflow(invocation_id, poll_interval = 30)

  # --- 4. Download result dataset ---
  # new tmp file
  tmpfile <- tempfile(fileext = ".laz")
  result_file <- galaxy_download_result(output_ids, tmpfile)

  # read the result
  las <- lidR::readTLS(tmpfile)
  file.remove(tmpfile)
  if (lidR::is.empty(las) ) return(NULL)

  # Get the heighest point XY location per PredInstance Id of the las file
  # get the row index of the highest Z per PredInstance
  idx <- las@data[, .I[which.max(Z)], by = PredInstance]$V1

  # subset to get X,Y,Z (and PredInstance if you want)
  highest_points <- las@data[idx, .(PredInstance, X, Y, Z)]
  highest_points <- highest_points[highest_points$PredInstance > 0,]

  # Get PredINstance IDs of heighest points in bbox
  highest_points <- highest_points[highest_points$X >= bbox[1,1] & highest_points$X <= bbox[1,2] &
                                       highest_points$Y >= bbox[2,1] & highest_points$Y <= bbox[2,2], ]

  # filter las by InstanceIds with highest points in bbox
  las <- las |> lidR::filter_poi(PredInstance %in% highest_points$PredInstance)
  if(lidR::is.empty(las) ) return(NULL)

  # renumber trees
  las@data$PredInstance <- las@data$PredInstance + (100000 * tile_id)

  # validate las
  las <- las  |>  lidR::las_quantize()  |> lidR::las_update()
  if (lidR::is.empty(las)) return(NULL)
  return(las)
}

ctg <- lidR::catalog_apply(catalog, catalog_function)
# TODO: Solve problem with wrong histories
# TODO: Remove data automatically from Galaxy
# TODO: Check with multiple Tiles in parallel
