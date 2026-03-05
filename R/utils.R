#' Internal functions
#'
#' WildTrax authentication
#'
#' @description Get Auth0 token and assign information to the hidden environment
#'
#' @keywords internal
#'
#' @import httr2

.wt_auth <- function() {

  # ABMI Auth0 client ID
  cid <- rawToChar(
    as.raw(c(0x45, 0x67, 0x32, 0x4d, 0x50, 0x56, 0x74, 0x71, 0x6b,
             0x66, 0x33, 0x53, 0x75, 0x4b, 0x53, 0x35, 0x75, 0x58, 0x7a, 0x50,
             0x39, 0x37, 0x6e, 0x78, 0x55, 0x31, 0x33, 0x5a, 0x32, 0x4b, 0x31,
             0x69)))

  # Initialize request to Auth0
  req <-  request("https://abmi.auth0.com/")

  if (Sys.getenv("WT_USERNAME") == "" || Sys.getenv("WT_PASSWORD") == "") {
    stop(
      "Environment variables are not set:\n",
      " - WT_USERNAME: ", ifelse(Sys.getenv("WT_USERNAME") == "", "MISSING", "SET"), "\n",
      " - WT_PASSWORD: ", ifelse(Sys.getenv("WT_PASSWORD") == "", "MISSING", "SET"), "\n",
      "Please set these variables using Sys.setenv() or add them to your .Renviron file."
    )
  }

  r <- req |>
    req_url_path("oauth/token") |>
    req_body_form(
      audience = "http://www.wildtrax.ca",
      grant_type = "password",
      client_id = cid,
      username = Sys.getenv("WT_USERNAME"),
      password = Sys.getenv("WT_PASSWORD")
    ) |>
    req_error(is_error = function(resp) FALSE) |>
    req_perform()

  # Check for authentication errors
  if (resp_is_error(r)) {
    stop(sprintf(
      "Authentication failed [%s]\n%s",
      resp_status(r),
      resp_body_json(r)$error_description
    ),
    call. = FALSE)
  }

  # Parse the JSON response
  x <- resp_body_json(r)

  # Calculate token expiry time
  t0 <- Sys.time()
  x$expiry_time <- t0 + x$expires_in

  # Check if the authentication environment exists
  if (!exists("._wt_auth_env_")) {
    stop("Cannot find the correct environment.", call. = FALSE)
  }

  # Send the token information to the ._wt_auth_env_ environment
  list2env(x, envir = ._wt_auth_env_)

  message("Authentication into WildTrax successful.")

  invisible(NULL)

}

#' Internal function to check if Auth0 token has expired
#'
#' @description Check if the Auth0 token has expired
#'
#' @keywords internal
#'

.wt_auth_expired <- function () {

  if (!exists("._wt_auth_env_"))
    stop("Cannot find the correct environment.", call. = TRUE)

  if (is.null(._wt_auth_env_$expiry_time))
    return(TRUE)

  ._wt_auth_env_$expiry_time <= Sys.time()
}

#' Generate user agent
#'
#' @description Generic function to to encapsulate user agents
#'
#' @keywords internal
#'

.gen_ua <- function() {
  user_agent <- getOption("HTTPUserAgent")
  if (is.null(user_agent)) {
    user_agent <- sprintf(
      "R/%s; R (%s)",
      getRversion(),
      paste(getRversion(), R.version$platform, R.version$arch, R.version$os)
    )
  }
  user_agent <- paste0("wildrtrax ", as.character(packageVersion("wildrtrax")), "; ", user_agent)
  return(user_agent)
}

#' Switch locale to another language
#'
#' @description Global function to allow a user to request data in another language. Currently English = en or French = fr.
#'
#' @keywords internal
#'

.language <- function(language = c("en", "fr")) {
  language <- match.arg(language) # Ensure valid language selection
  return(language)
}

#' An internal function to handle generic POST requests to WildTrax
#'
#' @description Generic function to handle certain POST requests
#'
#' @param path The path to the API
#' @param ... Argument to pass along into POST query
#' @param max_time The maximum number of seconds the API request can take. By default 300.
#'
#' @keywords internal
#'
#' @import httr2

.wt_api_pr <- function(path, ..., max_time=300) {

  # Check if authentication has expired:
  if (.wt_auth_expired()) {stop("Please authenticate with wt_auth().", call. = FALSE)}

  ## User agent
  u <- .gen_ua()

  # Convert ... into a list
  query_params <- list(...)

  # Check if query_params is a list; if not, ensure it is treated as a list
  if (length(query_params) == 1 && is.character(query_params[[1]])) {
    # If there's only one element and it's a character, treat it as a named query
    query_params <- as.list(query_params)
  }

  if (path != "/bis/download-report") {
    req <- request("https://www-api.wildtrax.ca") |>
      req_url_path_append(path) |>
      req_body_json(query_params) |>
      req_headers(Authorization = paste("Bearer", ._wt_auth_env_$access_token)) |>
      req_user_agent(u) |>
      req_method("POST") |>
      req_timeout(max_time) |>
      req_perform()

    if (resp_status(req) >= 400) {
      stop(sprintf("API request failed [%s]", resp_status(req)), call. = FALSE)
    }
    return(req)

  } else {

    stop("The API you provided is not yet supported by this function")

  }
}

#' An internal function to handle generic GET requests to WildTrax
#'
#' @description Generic function to handle certain GET requests
#'
#' @param path The path to the API
#' @param ... Argument to pass along into GET query
#' @param max_time The maximum number of seconds the API request can take. By default 300.
#'
#' @keywords internal
#'
#' @import httr2

.wt_api_gr <- function(path, ..., max_time=300) {

  # Check if authentication has expired:
  if (.wt_auth_expired()) {stop("Please authenticate with wt_auth().", call. = FALSE)}

  ## User agent
  u <- .gen_ua()

  # Validate language input
  #accept_language <- .language(language)

  # Convert ... into a list
  query_params <- list(...)

  # Check if query_params is a list; if not, ensure it is treated as a list
  if (length(query_params) == 1 && is.character(query_params[[1]])) {
    # If there's only one element and it's a character, treat it as a named query
    query_params <- as.list(query_params)
  }

  r <- request("https://www-api.wildtrax.ca") |>
    req_url_path_append(path) |>
    req_body_json(query_params) |>
    req_headers(Authorization = paste("Bearer", ._wt_auth_env_$access_token)) |>
    req_user_agent(u) |>
    req_method("GET") |>
    req_timeout(max_time) |>
    req_perform()

  # Handle errors
  if (resp_status(r) >= 400) {
    stop(sprintf(
      "Authentication failed [%s]\n%s",
      resp_status(r),
      message),
      call. = FALSE)
  } else {
    return(r)
  }

}

#' Column assignments
#'
#' @description Assign correct column types for reports
#'
#' @importFrom readr col_character col_double col_logical col_date col_datetime col_integer
#'
#' @keywords internal
#'

.wt_col_types <- list(
  abundance = col_character(),
  age_class = col_character(),
  behaviours = col_character(),
  bounding_box_number = col_double(),
  category = col_character(),
  clip_channel_used = col_character(),
  classifier_confidence = col_double(),
  classifier_version = col_character(),
  coat_attributes = col_character(),
  coat_colours = col_character(),
  confidence = col_double(),
  date_deployed = col_date(),
  date_retrieved = col_date(),
  daylight_hours = col_double(),
  direction_travel = col_character(),
  detection_time = col_double(),
  disabled_for_autotag = col_logical(),
  elevation = col_double(),
  equipment = col_character(),
  equipment_make = col_character(),
  equipment_model = col_character(),
  equipment_serial = col_character(),
  has_collar = col_logical(),
  has_eartag = col_logical(),
  health_diseases = col_character(),
  height = col_double(),
  ihf = col_character(),
  image_comments = col_character(),
  image_date_time = col_datetime(),
  image_exif_sequence = col_character(),
  image_exif_temperature = col_double(),
  image_fire = col_logical(),
  image_fov = col_character(),
  image_id = col_integer(),
  image_in_wildtrax = col_logical(),
  image_is_blurred = col_logical(),
  image_malfunction = col_logical(),
  image_nice = col_logical(),
  image_set_count_motion = col_integer(),
  image_set_count_timelapse = col_integer(),
  image_set_count_total = col_integer(),
  image_set_start_date_time = col_datetime(),
  image_set_status = col_character(),
  image_set_url = col_character(),
  image_snow = col_logical(),
  image_snow_depth_m = col_double(),
  image_trigger_mode = col_character(),
  image_url = col_character(),
  image_water_depth_m = col_double(),
  individual_count = col_character(),
  individual_order = col_integer(),
  is_complete = col_logical(),
  is_enabled_project_species = col_logical(),
  is_species_allowed_in_project = col_logical(),
  latitude = col_double(),
  location = col_character(),
  location_buffer_m = col_double(),
  location_comments = col_character(),
  location_id = col_integer(),
  location_visibility = col_character(),
  longitude = col_double(),
  media_url = col_character(),
  min_tag_freq = col_double(),
  max_tag_freq = col_double(),
  `name/region/country` = col_character(),
  needs_review = col_logical(),
  observer = col_character(),
  observer_id = col_integer(),
  organization = col_character(),
  project = col_character(),
  project_description = col_character(),
  project_id = col_integer(),
  project_results = col_character(),
  project_status = col_character(),
  project_creation_date = col_date(),
  project_due_date = col_date(),
  recording_date_time = col_datetime(),
  recording_id = col_double(),
  recording_length = col_double(),
  rms_peak_dbfs = col_double(),
  source_file_name = col_character(),
  species_class = col_character(),
  species_code = col_character(),
  species_common_name = col_character(),
  species_health = col_character(),
  species_individual_comments = col_character(),
  species_scientific_name = col_character(),
  sunrise_utc = col_datetime(),
  sunset_utc = col_datetime(),
  start_s = col_double(),
  end_s = col_double(),
  tag_comments = col_character(),
  tag_duration = col_double(),
  tag_id = col_integer(),
  tag_is_verified = col_logical(),
  tag_needs_review = col_logical(),
  tag_rating = col_character(),
  tagged_in_wildtrax = col_logical(),
  task_comments = col_character(),
  task_duration = col_double(),
  task_id = col_double(),
  task_method = col_character(),
  task_url = col_character(),
  task_status = col_character(),
  tine_attributes = col_character(),
  version = col_character(),
  vocalization = col_character(),
  width = col_double(),
  x_loc = col_double(),
  y_loc = col_double()
)

#' Internal evaluation function for acoustic classifiers
#'
#' @description Internal function to calculate precision, recall, and F-score for a given score threshold.
#'
#' @param data Output from the `wt_download_report()` function when you request the `main` and `ai` reports
#' @param threshold A single numeric value for score threshold
#' @param human_total The total number of detections in the gold standard, typically from human listening data (e.g., the main report)
#'
#' @keywords internal
#'
#' @import dplyr
#'
#' @return A vector of precision, recall, F-score, and threshold

.wt_calculate_prf <- local({

  message_shown <- FALSE

  function(threshold, data, human_total){
    # Summarize
    data_thresholded <- data |>
      filter(confidence >= threshold) |>
      reframe(
        precision = sum(tp) / (sum(tp) + sum(fp)),
        recall = sum(tp) / human_total
      ) |>
      mutate(
        fscore = 2 * precision * recall / (precision + recall),
        threshold = threshold
      )

    if(anyNA(data_thresholded$precision) && !message_shown){
      message('No classifier detections for some higher selected thresholds; results will contain NAs')
      message_shown <<- TRUE
    }

    return(data_thresholded)
  }
})

#' Internal function to delete media
#'
#' @description Internal function to delete media.
#'
#' @param dir Directory containing files
#'
#' @keywords internal
#'

.delete_wav_files <- function(dir) {
  wav_files <- list.files(path = dir, pattern = "\\.wav$", recursive = TRUE, full.names = TRUE)
  file.remove(wav_files)
}

#' Internal function to get Organizations
#'
#' @description Internal function to get Organizations
#'
#' @keywords internal

.get_org_id <- function(organization) {

  if (is.numeric(organization)) {
    return(organization)
  } else if (is.character(organization)) {
    orgs <- .wt_api_gr(path = "/bis/get-all-readable-organizations")
    og <- httr2::resp_body_json(orgs)

    # Create a lookup table
    og_table <- tibble(
      org_id = purrr::map_dbl(og, ~ ifelse(!is.null(.x$id), .x$id, NA)),
      org_code = purrr::map_chr(og, ~ ifelse(!is.null(.x$name), .x$name, NA))
    )

    # Retrieve and return the numeric org_id
    return(og_table |>
             filter(org_code == organization) |>
             pull(org_id))
  }
  stop("Organization must be either numeric or character")
}

#' Internal function for QPAD offsets
#'
#' QPAD offsets, wrapped by the `wt_qpad_offsets` function.
#'
#' @description Functions to format reports for qpad offset calculation.
#'
#' @param data Dataframe output from the `wt_make_wide` function.
#' @param tz Character; whether or not the data is in local or UTC time ("local", or "utc"). Defaults to "local".
#' @param check_xy Logical; check whether coordinates are within the range that QPAD offsets are valid for.
#'
#' @keywords internal
#'
#' @import dplyr httr2
#' @importFrom terra extract rast vect project
#' @importFrom suntools sunriset
#'

.make_x <- function(data, tz="local", check_xy=TRUE) {

  # if(!requireNamespace("QPAD")) {
  #   stop("The QPAD package is required for this function. Please install it using remotes::install_github('borealbirds/QPAD')")
  # }

  # Download message
  message("Downloading geospatial assets. This may take a moment.")

  # Function to download and read a raster file using httr2
  download_and_read_raster <- function(url, filename) {
    req <- request(url) |>
      req_perform()  # Perform the request

    # Save the response content to a file
    writeBin(req$body, filename)

    return(rast(filename))  # Read the raster file
  }

  # Download and read TIFF files
  .rlcc <- download_and_read_raster("https://raw.githubusercontent.com/ABbiodiversity/wildRtrax-assets/main/lcc.tif", "lcc.tif")
  .rtree <- download_and_read_raster("https://raw.githubusercontent.com/ABbiodiversity/wildRtrax-assets/main/tree.tif", "tree.tif")
  .rd1 <- download_and_read_raster("https://raw.githubusercontent.com/ABbiodiversity/wildRtrax-assets/main/seedgrow.tif", "seedgrow.tif")
  .rtz <- download_and_read_raster("https://raw.githubusercontent.com/ABbiodiversity/wildRtrax-assets/main/utcoffset.tif", "utcoffset.tif")

  crs <- terra::crs(.rtree)

  #get vars
  date <- substr(data$recording_date_time, 1, 10)
  time <- substr(data$recording_date_time, 12, 19)
  lon <- as.numeric(data$longitude)
  lat <- as.numeric(data$latitude)
  dur <- as.numeric(data$task_duration)
  dis <- Inf

  #parse date+time into POSIXlt
  if(tz=="local"){
    dtm <- strptime(paste0(date, " ", time, ":00"),
                    format="%Y-%m-%d %H:%M:%S", tz="America/Edmonton")
  }
  if(tz=="utc"){
    dtm <- strptime(paste0(date, " ", time, ":00"),
                    format="%Y-%m-%d %H:%M:%S", tz="GMT")
  }
  day <- as.integer(dtm$yday)
  hour <- as.numeric(round(dtm$hour + dtm$min/60, 2))

  #checks
  checkfun <- function(x, name="", range=c(-Inf, Inf)) {
    if (any(x[!is.na(x)] < range[1] | x[!is.na(x)] > range[2])) {
      stop(sprintf("Parameter %s is out of range [%.0f, %.0f]", name, range[1], range[2]))
    }
    invisible(NULL)
  }

  #Coordinates
  if (check_xy) {
    checkfun(lon, "lon", c(-164, -52))
    checkfun(lat, "lat", c(39, 69))
  }

  if (any(is.infinite(lon)))
    stop("Parameter lon must be finite")
  if (any(is.infinite(lat)))
    stop("Parameter lat must be finite")

  #handling missing values
  ok_xy <- !is.na(lon) & !is.na(lat)
  #Other fields
  checkfun(day, "day", c(0, 365))
  checkfun(hour, "hour", c(0, 24))
  checkfun(dur, "dur", c(0, Inf))

  #intersect here
  xydf <- data.frame(x=lon, y=lat)
  xydf$x[is.na(xydf$x)] <- mean(xydf$x, na.rm=TRUE)
  xydf$y[is.na(xydf$y)] <- mean(xydf$y, na.rm=TRUE)
  xy <- vect(xydf, geom=c("x", "y"), crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  xy <- project(xy, crs)

  #LCC4 and LCC2
  vlcc <- extract(.rlcc, xy)$lcc
  lcclevs <- c("0"="", "1"="Conif", "2"="Conif", "3"="", "4"="",
               "5"="DecidMixed", "6"="DecidMixed", "7"="", "8"="Open", "9"="",
               "10"="Open", "11"="Open", "12"="Open", "13"="Open", "14"="Wet",
               "15"="Open", "16"="Open", "17"="Open", "18"="", "19"="")
  lcc4 <- factor(lcclevs[vlcc+1], c("DecidMixed", "Conif", "Open", "Wet"))
  lcc2 <- lcc4
  levels(lcc2) <- c("Forest", "Forest", "OpenWet", "OpenWet")

  #TREE
  vtree <- extract(.rtree, xy)$tree
  TREE <- vtree / 100
  TREE[TREE < 0 | TREE > 1] <- 0

  #raster::extract seedgrow value (this is rounded)
  d1 <- extract(.rd1, xy)$seedgrow

  #UTC offset + 7 makes Alberta 0 (MDT offset) for local times
  if(tz=="local"){
    ltz <- extract(.rtz, xy)$utcoffset + 7
  }
  if(tz=="utc"){
    ltz <- 0
  }

  message("Removing geospatial assets from local")

  # Remove once downloaded and read
  file.remove(list.files(pattern = "*.tif$"))

  #sunrise time adjusted by offset
  ok_dt <- !is.na(dtm)
  dtm[is.na(dtm)] <- mean(dtm, na.rm=TRUE)
  if(tz=="local"){
    sr <- sunriset(cbind("X"=xydf$x, "Y"=xydf$y),
                   as.POSIXct(dtm, tz="America/Edmonton"),
                   direction="sunrise", POSIXct.out=FALSE) * 24
  }
  if(tz=="utc"){
    sr <- sunriset(cbind("X"=xydf$x, "Y"=xydf$y),
                   as.POSIXct(dtm, tz="GMT"),
                   direction="sunrise", POSIXct.out=FALSE) * 24
  }
  TSSR <- round(unname((hour - sr - ltz) / 24), 4)

  #days since local spring
  DSLS <- (day - d1) / 365

  #transform the rest
  JDAY <- round(day / 365, 4) # 0-365
  TREE <- round(vtree / 100, 4)
  MAXDIS <- round(dis / 100, 4)
  MAXDUR <- round(dur, 4)

  out <- data.frame(
    TSSR=TSSR,
    JDAY=JDAY,
    DSLS=DSLS,
    LCC2=lcc2,
    LCC4=lcc4,
    TREE=TREE,
    MAXDUR=MAXDUR,
    MAXDIS=MAXDIS)
  out$TSSR[!ok_xy | !ok_dt] <- NA
  out$DSLS[!ok_xy] <- NA
  out$LCC2[!ok_xy] <- NA
  out$LCC4[!ok_xy] <- NA
  out$TREE[!ok_xy] <- NA

  return(out)

}

#' QPAD offsets, wrapped by the `wt_qpad_offsets` function.
#'
#' @description Functions to get the offsets.
#'
#' @param spp species for offset calculation.
#' @param x Dataframe out from the `.make_x` function.
#'
#' @keywords internal

.make_off <- function(spp, x){

  if(!requireNamespace("QPAD", quietly = T)) {
    stop("The QPAD package is required for this function. Please install it using remotes::install_github('borealbirds/QPAD')")
  }

  if (length(spp) > 1L)
    stop("spp argument must be length 1. Use a loop or map for multiple species.")
  spp <- as.character(spp)

  getBAMspecieslist <- get("getBAMspecieslist", envir = asNamespace("QPAD"))
  coefBAMspecies <- get("coefBAMspecies", envir = asNamespace("QPAD"))
  bestmodelBAMspecies <- get("bestmodelBAMspecies",  envir = asNamespace("QPAD"))
  sra_fun <- get("sra_fun",  envir = asNamespace("QPAD"))
  edr_fun <- get("edr_fun",  envir = asNamespace("QPAD"))

  #checks
  if (!(spp %in% getBAMspecieslist()))
    stop(sprintf("Species %s has no QPAD estimate available", spp))

  #constant for NA cases
  cf0 <- exp(unlist(coefBAMspecies(spp, 0, 0)))

  #best model
  mi <- bestmodelBAMspecies(spp, type="BIC")
  cfi <- coefBAMspecies(spp, mi$sra, mi$edr)

  TSSR <- x$TSSR
  DSLS <- x$DSLS
  JDAY <- x$JDAY
  lcc2 <- x$LCC2
  lcc4 <- x$LCC4
  TREE <- x$TREE
  MAXDUR <- x$MAXDUR
  MAXDIS <- x$MAXDIS
  n <- nrow(x)

  #Design matrices for singing rates (`Xp`) and for EDR (`Xq`)
  Xp <- cbind(
    "(Intercept)"=1,
    "TSSR"=TSSR,
    "JDAY"=JDAY,
    "TSSR2"=TSSR^2,
    "JDAY2"=JDAY^2,
    "DSLS"=DSLS,
    "DSLS2"=DSLS^2)

  Xq <- cbind("(Intercept)"=1,
              "TREE"=TREE,
              "LCC2OpenWet"=ifelse(lcc4 %in% c("Open", "Wet"), 1, 0),
              "LCC4Conif"=ifelse(lcc4=="Conif", 1, 0),
              "LCC4Open"=ifelse(lcc4=="Open", 1, 0),
              "LCC4Wet"=ifelse(lcc4=="Wet", 1, 0))

  p <- rep(NA, n)
  A <- q <- p

  #design matrices matching the coefs
  Xp2 <- Xp[,names(cfi$sra),drop=FALSE]
  OKp <- rowSums(is.na(Xp2)) == 0
  Xq2 <- Xq[,names(cfi$edr),drop=FALSE]
  OKq <- rowSums(is.na(Xq2)) == 0

  #calculate p, q, and A based on constant phi and tau for the respective NAs
  p[!OKp] <- sra_fun(MAXDUR[!OKp], cf0[1])
  unlim <- ifelse(MAXDIS[!OKq] == Inf, TRUE, FALSE)
  A[!OKq] <- ifelse(unlim, pi * cf0[2]^2, pi * MAXDIS[!OKq]^2)
  q[!OKq] <- ifelse(unlim, 1, edr_fun(MAXDIS[!OKq], cf0[2]))

  #calculate time/lcc varying phi and tau for non-NA cases
  phi1 <- exp(drop(Xp2[OKp,,drop=FALSE] %*% cfi$sra))
  tau1 <- exp(drop(Xq2[OKq,,drop=FALSE] %*% cfi$edr))
  p[OKp] <- sra_fun(MAXDUR[OKp], phi1)
  unlim <- ifelse(MAXDIS[OKq] == Inf, TRUE, FALSE)
  A[OKq] <- ifelse(unlim, pi * tau1^2, pi * MAXDIS[OKq]^2)
  q[OKq] <- ifelse(unlim, 1, edr_fun(MAXDIS[OKq], tau1))

  #log(0) is not a good thing, apply constant instead
  ii <- which(p == 0)
  p[ii] <- sra_fun(MAXDUR[ii], cf0[1])

  #package output
  data.frame(
    p=p,
    q=q,
    A=A,
    correction=p*A*q,
    offset=log(p) + log(A) + log(q))

}


setClassUnion("df_or_NULL", c("data.frame", "NULL"))
setClassUnion("list_or_NULL", c("list", "NULL"))

# The CAM class
setClass("wt_CAM",
         slots = list(
           main = "df_or_NULL",
           project = "df_or_NULL",
           location = "df_or_NULL",
           image_report = "df_or_NULL",
           image_set_report = "df_or_NULL",
           tag = "df_or_NULL",
           megadetector = "df_or_NULL",
           definitions = "list_or_NULL"
         )
)

setValidity("wt_CAM", function(object) {
  slots_to_check <- c(
    "main", "project", "location",
    "image_report", "image_set_report",
    "tag", "megadetector",
    "definitions"
  )
  # Check that there is at least
  #  one element that is not NULL within the
  #  object. This is essentially 'forced'
  #  when using wt_download_report(), but
  #  adding just in case someone tries
  #  to create the class downstream.
  values <- lapply(slots_to_check, function(s) slot(object, s))

  if (all(vapply(values, is.null, logical(1)))) {
    return("At least one data slot must be non-NULL.")
  }

  return(TRUE)
})

### Class generation and checks ####

# hidden functions for the various reports.
# These functions check to see if the
#  columns are present and that their
#  classes are correct. We are adding
#  these here (instead of via
#  setValidity) because we can
#  have more verbose errors this
#  way.
.validate_col_classes <- function(df, column_class_list) {

  bad_cols <- vapply(names(column_class_list), function(col) {

    expected_class <- column_class_list[[col]]
    actual_class <- class(df[[col]])[1]

    # allow multiple acceptable classes
    if (length(expected_class) > 1) {
      !any(vapply(expected_class, function(cls) inherits(df[[col]], cls), logical(1)))
    } else {
      !inherits(df[[col]], expected_class)
    }

  }, logical(1))

  if (any(bad_cols)) {

    bad_names <- names(column_class_list)[bad_cols]

    msg <- paste0(
      "Columns with incorrect classes:\n",
      paste(
        sprintf(
          "  - %s (expected: %s, actual: %s)",
          bad_names,
          vapply(column_class_list[bad_names], function(x) paste(x, collapse = "/"), character(1)),
          vapply(df[bad_names], function(x) class(x)[1], character(1))
        ),
        collapse = "\n"
      )
    )

    stop(msg, call. = FALSE)
  }

  TRUE
}

.check_missing_cols <- function(df, required_cols, slot_name) {
  missing_cols <- setdiff(
    names(required_cols),
    names(df)
  )

  if (length(missing_cols) > 0) {
    stop(
      paste0("`", slot_name, "` is missing required columns: ",
             paste(missing_cols, collapse = ", ")),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

.verify_main_cam <- function(df){

  # At the moment just requiring
  #  all of these. We may want
  #  to have some be optional I
  #  guess? But at the moment I
  #  am unsure which ones I'd
  #  want to do that with. Also,
  #  specifying the class for
  #  each object so we can
  #  confirm that as well.

  main_cam_cols_and_classes <- list(
    project_id = "integer",
    location = "character",
    location_id = "integer",
    latitude = "numeric",
    longitude = "numeric",
    location_buffer_m = "numeric",
    equipment_serial = "character",
    image_id = "integer",
    image_date_time = "POSIXct",
    image_set_id = "numeric",
    series_no_at_gap = "character",
    image_fov = "character",
    image_snow = "logical",
    image_snow_depth_m = "numeric",
    image_water_depth_m = "numeric",
    species_scientific_name = "character",
    species_common_name = "character",
    individual_count = c("integer", "character"),
    age_class = "character",
    sex_class = "logical",
    behaviours = "character",
    health_diseases = "character",
    coat_colours = "character",
    coat_attributes = "character",
    tine_attributes = "character",
    direction_travel = "character",
    has_collar = "logical",
    has_eartag = "logical",
    ihf = "character",
    observer = "character",
    observer_id = "integer",
    tag_comments = "character",
    tag_needs_review = "logical",
    tag_is_verified = "logical",
    image_in_wildtrax = "logical",
    tag_id = "integer"
  )

  missing_col_check <- .check_missing_cols(
    df = df,
    required_cols = main_cam_cols_and_classes,
    slot_name = "main"
  )

  # Now check the classes of the columns
  class_check <- .validate_col_classes(
    df = df,
    column_class_list = main_cam_cols_and_classes
  )

  # Give warnings if lat / long are outside
  #  of their respective bounds.
  if (any(df$latitude < -90 | df$latitude > 90, na.rm = TRUE)) {
    warning(
      "`latitude` are outside of [-90,90], please update `latitude` coordinates in WildTrax.",
      call. = FALSE
    )
  }
  if (any(df$longitude < -180 | df$longitude > 180, na.rm = TRUE)) {
    warning(
      "`longitude` are outside of [-180,180], please update `longitude` coordinates in WildTrax.",
      call. = FALSE
    )
  }
  if(any(is.na(df$longitude)) || any(is.na(df$latitude))){
    warning(
      "Some `latitude` and/or `longitude` elements are missing. Please add them to WildTrax.",
      call. = FALSE
    )
  }

  if (any(df$image_date_time < as.POSIXct("1995-01-01", tz = "UTC"), na.rm = TRUE)) {
    warning("Some `image_date_time` values are before 1995 (EXIF standard). Please verify.", call. = FALSE)
  }

  if (any(df$image_date_time > Sys.time(), na.rm = TRUE)) {
    warning("Some `image_date_time` values are in the future. Please verify", call. = FALSE)
  }

  # if everything works, then return an
  #  invisible TRUE
  return(invisible(TRUE))
}

setValidity("wt_CAM", function(object) {
  slots_to_check <- c(
    "main", "project", "location",
    "image_report", "image_set_report",
    "tag", "megadetector",
    "definitions"
  )
  # Check that there is at least
  #  one element that is not NULL within the
  #  object. This is essentially 'forced'
  #  when using wt_download_report(), but
  #  adding just in case someone tries
  #  to create the class downstream.
  values <- lapply(slots_to_check, function(s) slot(object, s))

  if (all(vapply(values, is.null, logical(1)))) {
    return("At least one data slot must be non-NULL.")
  }

  return(TRUE)
})


# verify project cam
.verify_project_cam <- function(df) {

  project_cam_cols_and_classes <- list(
    organization = "character",
    project = "character",
    project_id = "integer",
    project_status = "character",
    project_description = "character",
    project_results = "character"
  )

  missing_col_check <- .check_missing_cols(
    df = df,
    required_cols = project_cam_cols_and_classes,
    slot_name = "project"
  )

  .validate_col_classes(
    df = df,
    column_class_list = project_cam_cols_and_classes
  )

  # Return TRUE invisibly
  invisible(TRUE)
}



.verify_location_cam <- function(df) {

  location_cam_cols_and_classes <- list(
    organization = "character",
    location = "character",
    location_id = "integer",
    location_buffer_m = "numeric",
    latitude = "numeric",
    longitude = "numeric",
    location_visibility = "character",
    elevation = "numeric",
    location_comments = "character"
  )

  missing_col_check <- .check_missing_cols(
    df = df,
    required_cols = location_cam_cols_and_classes,
    slot_name = "location"
  )

  .validate_col_classes(
    df = df,
    column_class_list = location_cam_cols_and_classes
  )

  if (any(df$latitude < -90 | df$latitude > 90, na.rm = TRUE)) {
    warning("Some `latitude` values are outside [-90, 90], please update them in WildTrax.", call. = FALSE)
  }

  if (any(df$longitude < -180 | df$longitude > 180, na.rm = TRUE)) {
    warning("Some `longitude` values are outside [-180, 180], please update them in WildTrax.", call. = FALSE)
  }

  # Return TRUE invisibly
  invisible(TRUE)
}

.verify_image_report_cam <- function(df) {

  image_report_cam_cols_and_classes <- list(
    project_id = "integer",
    location = "character",
    location_id = "integer",
    image_id = "integer",
    image_date_time = "POSIXct",
    source_file_name = "character",
    equipment_make = "character",
    equipment_model = "character",
    equipment_serial = "character",
    image_fire = "logical",
    image_nice = "logical",
    image_malfunction = "logical",
    image_set_id = "numeric",
    image_fov = "character",
    image_snow = "logical",
    image_snow_depth_m = "numeric",
    image_water_depth_m = "numeric",
    image_trigger_mode = "character",
    image_is_blurred = "logical",
    media_url = "character",
    image_in_wildtrax = "logical",
    image_comments = "character"
  )

  missing_col_check <- .check_missing_cols(
    df = df,
    required_cols = image_report_cam_cols_and_classes,
    slot_name = "image_report"
  )

  .validate_col_classes(
    df = df,
    column_class_list = image_report_cam_cols_and_classes
  )

  if (any(df$image_date_time < as.POSIXct("1995-01-01", tz = "UTC"), na.rm = TRUE)) {
    warning("Some `image_date_time` values are before 1995 (EXIF standard). Please verify.", call. = FALSE)
  }

  if (any(df$image_date_time > Sys.time(), na.rm = TRUE)) {
    warning("Some `image_date_time` values are in the future. Please verify", call. = FALSE)
  }

  invisible(TRUE)
}

# to do: image set report, tag, megadetector,
#  and definitions.

# constructor for wt_CAM objects
wt_CAM <- function(main, project, location,
                   image_report, image_set_report,
                   tag, megadetector, definitions){

  if(!is.null(main)){
    .validate_main_cam(main)
  }

  if(!is.null(project)){
    .validate_project_cam(project)
  }
  if(!is.null(location)){
    .validate_location_cam(location)
  }

  new_wt_CAM_object <- new(
    "wt_CAM",
    main = main,
    project = project,
    location = location,
    image_report = image_report,
    image_set_report = image_set_report,
    tag = tag,
    megadetector = megadetector,
    definitions = definitions
  )
  validObject(new_wt_CAM_object)

  return(new_wt_CAM_object)
}
