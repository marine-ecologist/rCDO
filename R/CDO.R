library(magrittr)


#' Read CDO Input Files
#'
#' Initializes the command structure with the input files.
#' @param ... passes on to other functions
#' @return A list containing the command structure for CDO operations.
#' @examples
#'\dontrun{
#' read_cdo("/path/to/infile1.nc")
#'
#' read_cdo(infile) %>%
#' operator("maskregion", myregion) %>%
#'   write_cdo(outfile)
#'}
read_cdo <- function(...) {
  infiles <- list(...)
  list(command = "cdo", args = list(), infiles = infiles, outfile = NULL)
}

#' Append a CDO Operator
#'
#' Appends a specified CDO operator to the command structure.
#' @param cdo_command The command structure for CDO operations.
#' @param op The CDO operator to be appended.
#' @param params Parameters for the CDO operator (optional).
#' @return The updated command structure with the appended operator.
#' @examples
#' \dontrun{
#' read_cdo("/path/to/infile.nc") %>%
#'   operator("maskregion", "/path/to/region.txt")
#'
#'
#' read_cdo(infile) %>%
#'   operator("maskregion", myregion) %>%
#'   operator("sellonlatbox", lonlatbox) %>%
#'   write_cdo(outfile)
#'}
#'
operator <- function(cdo_command, op, params = NULL) {
  # Ensure params are properly formatted
  if (is.null(params)) {
    params_str <- ""
  } else if (is.character(params) && length(params) == 1) {
    params_str <- params
  } else if (is.character(params) && length(params) > 1) {
    params_str <- paste(params, collapse = ",")
  } else if (is.numeric(params) || is.vector(params)) {
    params_str <- paste(params, collapse = ",")
  } else {
    stop("Unsupported parameter type")
  }

  # Append the operator and params to the command
  if (params_str != "") {
    cdo_command$args <- append(cdo_command$args, paste0("-", op, ",", params_str))
  } else {
    cdo_command$args <- append(cdo_command$args, paste0("-", op))
  }
  return(cdo_command)
}

#' Write CDO Output File
#'
#' Finalizes the command structure with the output file and executes the CDO command.
#'
#' @param cdo_command The command structure for CDO operations.
#' @param outfile The path for the output file (optional). If not specified, a temporary file will be created.
#' @param debug return just the CDO command, don't execute (TRUE/FALSE)
#' @return The path to the output file.
#' @examples
#' \dontrun{
#' read_cdo("/path/to/infile.nc") %>%
#'   operator("maskregion", "/path/to/region.txt") %>%
#'   write_cdo("/path/to/outfile.nc")
#'
#' read_cdo(infile) %>%
#'   operator("maskregion", myregion) %>%
#'   operator("sellonlatbox", lonlatbox) %>%
#'   write_cdo(outfile)
#'}
write_cdo <- function(cdo_command, outfile = NULL, debug = FALSE) {
  # Create a temporary file if outfile is not specified

  if (is.null(outfile)) {
    setoutfile=TRUE
    outfile <- tempfile(fileext = ".nc")
  } else {
    setoutfile=FALSE
  }
  cdo_command$outfile <- outfile

  # Construct the command string
  args <- c(cdo_command$args, cdo_command$infiles, cdo_command$outfile)

  # Execute the command
  result <- tryCatch({
    output <- system2("cdo", args = args, stdout = TRUE, stderr = TRUE)
    list(output = output, status = 0)
  }, error = function(e) {
    list(output = e$message, status = 1)
  })

  if (result$status != 0) {
    stop(paste("Error in executing CDO command:", result$output))
  }

  if (debug==TRUE){
    print(paste(cdo_command$command,
                paste(cdo_command$args, collapse = " "),
                cdo_command$infiles[[1]],
                cdo_command$outfile))
  }

  tryCatch({
    if (setoutfile==TRUE){
      return(cdo_command$outfile)
    }
  })
}




#' simplify mask
#'
#' simplify input sf for creating mask region
#'
#' @param sf_in sf polygon
#' @param crs set CRS of input sf file (see st_crs)
#' @param buffer add buffer around input sf file (see st_buffer)
#' @param tolerance simplify polygon by setting tolerance  (see st_simplify)
#' @return  output file.
#'
simplify_mask <- function(sf_in, crs=4326, buffer=8000, tolerance=15000){

  simple <- sf_in |>
    sf::st_transform(crs) |>
    sf::st_buffer(buffer) |>
    sf::st_as_sfc() |>
    sf::st_simplify(preserveTopology = TRUE, dTolerance = tolerance) |>
    sf::st_as_sf()

  return(simple)

}


#' create mask region
#'
#' takes an input sf polygon and creates a text list suitable for CDO "maskregion"
#'
#' @param input sf polygon
#' @param output output
#' @param return return output? True or False
#' @param crs set CRS of input sf file (see st_crs)
#' @param buffer add buffer around input sf file (see st_buffer)
#' @param tolerance simplify polygon by setting tolerance  (see st_simplify)
#' @param ... passes on to other functions
#' @return The path to the output file.
#'
create_maskregion <- function(input, crs, buffer, tolerance, output=NULL, return=FALSE, ...){


  border_points <- simplify_mask(input, crs, buffer, tolerance) |>
    sf::st_cast("POINT")

  coordinates <- sf::st_coordinates(border_points)
  points_df <- data.frame(lon = coordinates[, "X"], lat = coordinates[, "Y"])

  if (!is.null(output)){
    utils::write.table(points_df, output, row.names = FALSE, col.names = FALSE, sep = " ")
  }

  if (isTRUE(return)){
    return(points_df)
  }
}



#' create lonlat box
#'
#' takes an input sf polygon and creates a bounding box in the format of CDO
#'
#'
#' @param input sf polygon
#' @return Bounding box xmin xmax ymin ymax
#'
create_sellonlatbox <- function(input){

  selbboxbounds <- sf::st_bbox(input)
  selbbox <- c(selbboxbounds[1], selbboxbounds[3], selbboxbounds[2], selbboxbounds[4])
  return(selbbox)

}

