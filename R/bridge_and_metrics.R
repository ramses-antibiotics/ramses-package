

#' Create database bridge tables
#'
#' @description Create or re-create bridge tables to facilitate linking prescribing
#' events with encounters or episodes of care. Bridge tables are used when computing
#' rates of prescribing per admission or per 1,000 bed-days. Examples are available
#' from the \link[Ramses]{Ramses} vignette: \code{browseVignettes("Ramses")}.
#' The resulting tables on the database are named \itemize{
#'   \item \code{bridge_episode_prescription_overlap}
#'   \item \code{bridge_episode_prescription_initiation}
#'   \item \code{bridge_episode_therapy_overlap}
#'   \item \code{bridge_drug_prescriptions_date}
#'   \item \code{bridge_inpatient_episodes_date}.
#' }
#' @param conn a database connection
#' @param overwrite if \code{TRUE}, the function will overwrite any existing
#' bridge table on the database. The default is \code{FALSE}
#' @param silent if \code{TRUE}, the progress bar will be hidden. The default is 
#' \code{FALSE}.
#' @details 
#' 
#' Prescriptions with status \code{"entered-in-error"}, \code{"draft"}, 
#' \code{"cancelled"}, or \code{"unknown"} are not taken into account.
#' \describe{
#'    \item{\code{bridge_tables()}}{Generates all bridge tables.}
#'    \item{\code{bridge_episode_prescription_overlap()}}{Links prescriptions
#'    with inpatient episodes when they were administered. The resulting 
#'    table is the natural join of \code{inpatient_episodes} and 
#'    \code{drug_prescriptions} based on matching patient identifiers 
#'    and a time overlap between prescriptions and inpatient episodes.}
#'    \item{\code{bridge_episode_prescription_initiation()}}{Links prescriptions
#'    with inpatient episodes when they were authored. The resulting table 
#'    differs from {\code{bridge_episode_prescription_overlap}}: it links prescriptions 
#'    to the episode referencing the clinical team who prescribed them, rather
#'    that episodes during which the prescription was administered. The resulting
#'    table is the natural join of \code{inpatient_episodes} and 
#'    \code{drug_prescriptions} based on matching patient identifiers 
#'    and the prescription authoring date being comprised between the episode 
#'    start and end dates.}
#'    \item{\code{bridge_episode_therapy_overlap()}}{Links therapy episodes
#'    with inpatient episodes during which they were administered. The resulting 
#'    table is the natural join of \code{inpatient_episodes} and 
#'    \code{drug_therapy_episodes} based on matching patient identifiers 
#'    and a time overlap between therapy episodes and hospital stays.}
#'    \item{\code{bridge_drug_prescriptions_date()}}{Links prescriptions with
#'    the date dimension: the resulting table has one row per date for each day
#'    during which a prescription was active based on \code{prescription_start}
#'    and \code{prescription_end} fields. Each \code{prescription_id} and \code{date}
#'    combination is accompanied by the \emph{pro rata} day of therapy for that particular
#'    date in the \code{DOT_prescribed} variable.}
#'    \item{\code{bridge_inpatient_episodes_date()}}{Links inpatient episodes with
#'    the date dimension: the resulting table has one row per date for each encounter
#'    episode was taking place. Each \code{encounter_id}, \code{episode_number} and \code{date}
#'    combination is accompanied by the \emph{pro rata} length of stay in the
#'    \code{bed_days} variable.}
#' } 
#' @return \code{TRUE} if tables were successfully created
#' @seealso \code{browseVignettes("Ramses")}
#' @name bridge_tables
#' @export 
bridge_tables <- function(conn,
                          overwrite = FALSE,
                          silent = FALSE) {
  
  if (!DBI::dbExistsTable(conn, "drug_prescriptions") | 
      !DBI::dbExistsTable(conn, "drug_therapy_episodes")) {
    stop("Table `drug_prescriptions` and `therapy_episodes` must exist.\n Please use `load_medications()` first.")
  }
  if (!DBI::dbExistsTable(conn, "inpatient_episodes")) {
    stop("Table `inpatient_episodes` must exist.\nPlease use `load_inpatient_episodes()` first.")
  }
  
  x <- vector()
  if( !silent ) {
    progress_bar <- progress::progress_bar$new(
      format = "  building bridge tables [:bar] :percent",
      total = 5)
    progress_bar$tick(0)
  }
  x[1] <- bridge_episode_prescription_overlap(conn, overwrite)
  if( !silent ) progress_bar$tick()
  x[2] <- bridge_episode_prescription_initiation(conn, overwrite)
  if( !silent ) progress_bar$tick()
  x[3] <- bridge_episode_therapy_overlap(conn, overwrite)
  if( !silent ) progress_bar$tick()
  x[4] <- bridge_drug_prescriptions_date(conn, overwrite)
  if( !silent ) progress_bar$tick()
  x[5] <- bridge_inpatient_episodes_date(conn, overwrite)
  if( !silent ) progress_bar$tick()
  
  return(all(x))
}

#' @export
#' @name bridge_tables
bridge_episode_prescription_overlap <- function(conn, 
                                                overwrite = FALSE) {
  stopifnot(is.logical(overwrite))
  if( !is(conn, "PqConnection") && !is(conn, "duckdb_connection") ) {
    .throw_error_method_not_implemented("bridge_episode_prescription_overlap()",
                                        class(conn))
  }
  
  if (!DBI::dbExistsTable(conn, "inpatient_episodes")) {
    stop("Table `inpatient_episodes` must exist.\nPlease use `load_inpatient_episodes()` first.")
  }
  
  if (!DBI::dbExistsTable(conn, "drug_prescriptions")) {
    stop("Table `drug_prescriptions` must exist.\nPlease use `load_medications()` first.")
  }
  
  tblz_episodes <- tbl(conn, "inpatient_episodes")
  tblz_prescriptions <- tbl(conn, "drug_prescriptions") %>% 
    dplyr::filter(
      .data$prescription_status %in% c("active",
                                       "stopped",
                                       "completed")
    ) %>% 
    dplyr::select(
      "patient_id",
      "prescription_id",
      "prescription_start",
      "prescription_end"
    )

  tblz_bridge_episode_prescriptions_overlap <- tblz_episodes %>% 
    dplyr::inner_join(tblz_prescriptions, 
                      by = "patient_id") %>% 
    dplyr::filter(
      dplyr::between(.data$prescription_start, .data$episode_start, .data$episode_end) |
        dplyr::between(.data$prescription_end, .data$episode_start, .data$episode_end) |
        dplyr::between(.data$episode_start, .data$prescription_start, .data$prescription_end)
    )
  
  tblz_bridge_episode_prescriptions_overlap <- dplyr::mutate(
    tblz_bridge_episode_prescriptions_overlap,
    t_start = dplyr::sql("GREATEST(prescription_start::TIMESTAMP, episode_start::TIMESTAMP)"), 
    t_end = dplyr::sql("LEAST(prescription_end::TIMESTAMP, episode_end::TIMESTAMP)"))
  
  tblz_bridge_episode_prescriptions_overlap <- dplyr::select(
    tblz_bridge_episode_prescriptions_overlap,
    "patient_id",
    "encounter_id",
    "episode_number",
    "prescription_id",
    "t_start",
    "t_end"
  )
  
  if (overwrite) {
    .remove_db_tables(conn, "bridge_episode_prescription_overlap")
  }
  
  dplyr::compute(tblz_bridge_episode_prescriptions_overlap, 
                 name = "bridge_episode_prescription_overlap", 
                 temporary = FALSE)
  
  return(TRUE)
}

#' @export
#' @name bridge_tables
bridge_episode_prescription_initiation <- function(conn, 
                                                   overwrite = FALSE) {
  stopifnot(is.logical(overwrite))
  if( !is(conn, "PqConnection") && !is(conn, "duckdb_connection") ) {
    .throw_error_method_not_implemented("bridge_episode_prescription_overlap()",
                                        class(conn))
  }
  
  if (!DBI::dbExistsTable(conn, "inpatient_episodes")) {
    stop("Table `inpatient_episodes` must exist.\nPlease use `load_inpatient_episodes()` first.")
  }
  
  if (!DBI::dbExistsTable(conn, "drug_prescriptions")) {
    stop("Table `drug_prescriptions` must exist.\nPlease use `load_medications()` first.")
  }
  
  tblz_episodes <- tbl(conn, "inpatient_episodes")
  tblz_prescriptions <- tbl(conn, "drug_prescriptions") %>% 
    dplyr::filter(
      .data$prescription_status %in% c("active",
                                       "stopped",
                                       "completed")
    ) %>% 
    dplyr::select(
      "patient_id",
      "prescription_id",
      "prescription_start",
      "prescription_end",
      "authoring_date"
    )
  
  tblz_bridge_episode_prescription_initiation <- tblz_episodes %>% 
    dplyr::inner_join(tblz_prescriptions, by = "patient_id") %>% 
    dplyr::filter(
      dplyr::between(.data$authoring_date, .data$episode_start, .data$episode_end) 
    ) %>% 
    dplyr::select(
      "patient_id",
      "encounter_id",
      "episode_number",
      "prescription_id"
    )
  
  if (overwrite) {
    .remove_db_tables(conn, "bridge_episode_prescription_initiation")
  }
  
  silence <- dplyr::compute(tblz_bridge_episode_prescription_initiation, 
                            name = "bridge_episode_prescription_initiation", 
                            temporary = FALSE)
  
  return(TRUE)
}

#' @name bridge_tables
#' @export
bridge_episode_therapy_overlap <- function(conn, 
                                             overwrite = FALSE) {
  stopifnot(is.logical(overwrite))
  if( !is(conn, "PqConnection") && !is(conn, "duckdb_connection") ) {
    .throw_error_method_not_implemented("bridge_episode_therapy_overlap()",
                                        class(conn))
  }
  
  if (!DBI::dbExistsTable(conn, "inpatient_episodes")) {
    stop("Table `inpatient_episodes` must exist.\nPlease use `load_inpatient_episodes()` first.")
  }
  
  if (!DBI::dbExistsTable(conn, "drug_therapy_episodes")) {
    stop("Table `drug_therapy_episodes` must exist.\nPlease use `load_medications()` first.")
  }
  
  tblz_encounters <- tbl(conn, "inpatient_episodes") %>% 
    dplyr::select("patient_id", 
                  "encounter_id", 
                  "episode_number",
                  "episode_start", 
                  "episode_end")
  tblz_therapies <- tbl(conn, "drug_therapy_episodes")
  
  tblz_bridge_episode_therapy_overlap <- tblz_encounters %>% 
    dplyr::inner_join(tblz_therapies, by = "patient_id") %>% 
    dplyr::filter(
      dplyr::between(.data$therapy_start, .data$episode_start, .data$episode_end) |
        dplyr::between(.data$therapy_end, .data$episode_start, .data$episode_end) |
        dplyr::between(.data$episode_start, .data$therapy_start, .data$therapy_end)
    ) %>% 
    dplyr::mutate(
      t_start = dplyr::sql("GREATEST(therapy_start::TIMESTAMP, episode_start::TIMESTAMP)"),
      t_end = dplyr::sql("LEAST(therapy_end::TIMESTAMP, episode_end::TIMESTAMP)")
    ) %>% 
    dplyr::select(
      "patient_id",
      "encounter_id",
      "episode_number",
      "therapy_id",
      "t_start",
      "t_end"
    )
  
  if (overwrite) {
    .remove_db_tables(conn, "bridge_episode_therapy_overlap")
  }
  
  silence <- dplyr::compute(tblz_bridge_episode_therapy_overlap, 
                            name = "bridge_episode_therapy_overlap", 
                            temporary = FALSE)
  return(TRUE)
}


#' @name bridge_tables
#' @export
bridge_inpatient_episodes_date <- function(conn, overwrite = FALSE) {
  stopifnot(is.logical(overwrite))
  if( !is(conn, "PqConnection") && !is(conn, "duckdb_connection") ) {
    .throw_error_method_not_implemented("bridge_episode_prescription_overlap()",
                                        class(conn))
  }
  
  if (!DBI::dbExistsTable(conn, "inpatient_episodes")) {
    stop("Table `inpatient_episodes` must exist.\nPlease use `load_inpatient_episodes()` first.")
  }
  
  if (!DBI::dbExistsTable(conn, "drug_therapy_episodes")) {
    stop("Table `drug_therapy_episodes` must exist.\nPlease use `load_medications()` first.")
  }
  
  if (overwrite) {
    .remove_db_tables(conn, "bridge_inpatient_episodes_date")
  }
  
  ip_dates <- tbl(conn, "inpatient_episodes") %>%
    .sql_generate_date_series("episode_start", "episode_end") %>%
    dplyr::mutate(bed_days = .data$date_weight) %>%
    dplyr::select("patient_id", "encounter_id", "episode_number", "date", "bed_days") %>% 
    dplyr::compute(
      name = "bridge_inpatient_episodes_date",
      temporary = FALSE
    )
  
  return(TRUE)
}

#' @name bridge_tables
#' @export
bridge_drug_prescriptions_date <- function(conn, overwrite = FALSE) {
  stopifnot(is.logical(overwrite))
  if( !is(conn, "PqConnection") && !is(conn, "duckdb_connection") ) {
    .throw_error_method_not_implemented("bridge_episode_prescription_overlap()",
                                        class(conn))
  }
  
  if (!DBI::dbExistsTable(conn, "drug_prescriptions")) {
    stop("Table `drug_prescriptions` must exist.\nPlease use `load_medications()` first.")
  }
  
  if (!DBI::dbExistsTable(conn, "bridge_episode_prescription_overlap")) {
    stop("Table `bridge_episode_prescription_overlap` must exist.")
  }
  
  if (overwrite) {
    .remove_db_tables(conn, "bridge_drug_prescriptions_date")
  }
  
  DDD_enabled <- "DDD" %in% colnames(dplyr::tbl(conn, "drug_prescriptions"))
  
  rx_all <- dplyr::tbl(conn, "drug_prescriptions") %>% 
    dplyr::filter(
      .data$prescription_status %in% c("active",
                                       "stopped",
                                       "completed")
    ) %>%
    .sql_generate_date_series("prescription_start", "prescription_end") %>% 
    dplyr::mutate(DOT_prescribed_all = .data$date_weight)
  
  if (DDD_enabled) {
    rx_all <- rx_all %>%
      dplyr::mutate(
        DDD_prescribed_all = .data$DDD * .data$DOT_prescribed_all
      ) %>% 
      dplyr::select("patient_id", "prescription_id", "date", "DOT_prescribed_all", "DDD_prescribed_all") %>% 
      dplyr::compute()
  } else {
    rx_all <- rx_all %>% 
      dplyr::select("patient_id", "prescription_id", "date", "DOT_prescribed") %>% 
      dplyr::compute()
  }
  
  rx_ip_only <- dplyr::tbl(conn, "bridge_episode_prescription_overlap") %>% 
    .sql_generate_date_series("t_start", "t_end") %>% 
    dplyr::mutate(DOT_prescribed_IP_only = .data$date_weight)
  
  if (DDD_enabled) {
    rx_DDDs <- dplyr::tbl(conn, "drug_prescriptions") %>% 
      dplyr::select("prescription_id", "DDD")
    
    rx_ip_only <- rx_ip_only %>% 
      dplyr::left_join(rx_DDDs, by = "prescription_id") %>% 
      dplyr::mutate(DDD_prescribed_IP_only = .data$DOT_prescribed_IP_only * .data$DDD) %>% 
      dplyr::select(
        "patient_id",
        "prescription_id",
        "date", 
        "DOT_prescribed_IP_only",
        "DDD_prescribed_IP_only"
      ) %>%
      dplyr::compute()
    
  } else {
    rx_ip_only <- rx_ip_only %>% 
      dplyr::select(
        "patient_id", 
        "prescription_id", 
        "date",
        "DOT_prescribed_IP_only"
      ) %>%
      dplyr::compute()
  }
  
  rx_final <- dplyr::full_join(
    rx_all,
    rx_ip_only,
    by = c("patient_id", 
           "prescription_id", 
           "date")
  ) %>% 
    dplyr::compute(
      name = "bridge_drug_prescriptions_date",
      temporary = FALSE
    )
  
  return(TRUE)
}


#' Add demographics to a remote table with a "patient_id" key
#'
#' @param x a remote `tbl` object
#'
#' @return a remote `tbl` object enriched with any variables available from the
#' `patients` table out of: `date_of_birth`, `sex`, `ethnic_category_UK`, if it 
#' exists. Otherwise, return `x` untransformed.
#' @noRd
.tbl_add_demographics <- function(x) {
  # Verify if sex, dob and ethnicity are available. If they are, add them to the table
  
  if (!("patient_id" %in% colnames(x))) {
    stop("`x` does not contain a `patient_id` field.")
  }
  
  if (DBI::dbExistsTable(x$src$con, "patients")) {
    x <- dplyr::compute(x)
    pt_demog <- dplyr::tbl(x$src$con, "patients") 
    
    demog_selection <- character(0)
    if ("date_of_birth" %in% colnames(pt_demog)) demog_selection <- c(demog_selection, "date_of_birth")
    if ("sex" %in% colnames(pt_demog)) demog_selection <- c(demog_selection, "sex")
    if ("ethnic_category_UK" %in% colnames(pt_demog)) demog_selection <- c(demog_selection, "ethnic_category_UK")
    
    pt_demog <- dplyr::select(pt_demog, "patient_id", dplyr::all_of(demog_selection))
    
    x <- dplyr::left_join(
      x,
      pt_demog,
      by = "patient_id"
    )
  }
  
  x
}


#' Build a reporting hypercube for inpatient activity
#'
#' @description Create or overwrite table \code{`hypercube_inpatient`} which reports 
#' on prescribing during inpatient encounters.
#' 
#' @param conn a database connection
#'
#' @details This function may be used after loading records into the Ramses database
#' to facilitate reporting on inpatient activity.
#' 
#' DIMENSIONs
#' The following dimensions are built, if available from the Ramses database
#' 
#' 
#' @return a object of \code{`tbl_sql`} connected to the \code{reporting_inpatient_length_of_stay} table
#' @export
create_reporting_inpatient <- function(conn) {

  if (!DBI::dbExistsTable(conn, "inpatient_episodes")) {
    stop("Table `inpatient_episodes` must exist.\nPlease use `load_inpatient_episodes()` first.")
  }
  
  if (
    !DBI::dbExistsTable(conn, "bridge_episode_prescription_overlap") |
    !DBI::dbExistsTable(conn, "bridge_drug_prescriptions_date") |
    !DBI::dbExistsTable(conn, "bridge_inpatient_episodes_date")
  ) {
    stop("Bridge tables must exist. Please refer to ?bridge_tables for help")
  }

  .remove_db_tables(conn, "metrics_inpatient")
  
  cube_dim_ip <- c(
    "patient_id", 
    "encounter_id",
    "episode_number",
    "main_specialty_code", 
    "admission_method"
  )
  
  ip_expanded <- dplyr::tbl(conn, "inpatient_episodes") %>% 
    dplyr::select(dplyr::all_of(cube_dim_ip)) %>% 
    dplyr::left_join(
      dplyr::tbl(conn, "bridge_inpatient_episodes_date"),
      by = c("patient_id", "encounter_id", "episode_number")
    ) %>% 
    dplyr::mutate(
      admission_method_name = dplyr::case_when(
        admission_method == "1" ~ "Elective",
        admission_method == "2" ~ "Emergency",
        admission_method == "3" ~ "Transfer/Other"
      )
    ) %>% 
    .tbl_add_demographics()
  
  age_enabled <- "date_of_birth" %in% colnames(ip_expanded)

  if (age_enabled) {
    ip_expanded <- ip_expanded %>% 
      dplyr::mutate(
        age = dplyr::sql("date_part('year', age(date, date_of_birth))")
      ) %>%
      dplyr::select(-"date_of_birth")
  }
  
  ip_expanded <- ip_expanded %>% 
    dplyr::compute(
      name = "metrics_inpatient",
      temporary = FALSE
    )
  
  ip_expanded
}


#' Create prescribing reporting table
#'
#' @param conn a database connection
#' @details 
#' #' METRICS
#' 
#' \itemize{
#'     \item{DOT_prescribed: }
#'     \item{DOT_administered: } IF AVAILABLE
#'     \item{DDD_prescribed: } IF AVAILABLE
#'     \item{DDD_administered: } IF AVAILABLE
#' }
#' @return a object of \code{`tbl_sql`} connected to the \code{metrics_prescribing} table
#' @export
create_reporting_med_prescribing <- function(conn) {
  
  if (!DBI::dbExistsTable(conn, "drug_prescriptions")) {
    stop("Table `drug_prescriptions` must exist.\n Please use `load_medications()` first.")
  }
  
  if (
    !DBI::dbExistsTable(conn, "bridge_drug_prescriptions_date")
  ) {
    stop("Bridge tables must exist. Please refer to ?bridge_tables for help")
  }
  
  .remove_db_tables(conn, "metrics_prescribing")
  
  DDD_enabled <- "DDD" %in% colnames(dplyr::tbl(conn, "drug_prescriptions"))

  dim_cube_rx <- c(
    "patient_id",
    "prescription_id",
    "antiinfective_type",
    "prescription_context",
    "drug_code",
    "drug_group",
    "drug_name",
    "drug_display_name",
    "ATC_code",
    "ATC_route"
  )
  
  rx_data <- dplyr::tbl(conn, "drug_prescriptions") %>%
    dplyr::select(
      dplyr::all_of(dim_cube_rx)
    ) %>%
    dplyr::mutate(
      parenteral = dplyr::if_else(.data$ATC_route == "P", 1, 0)
    ) %>%
    dplyr::inner_join(
      dplyr::tbl(conn, "bridge_drug_prescriptions_date"),
      by = c("patient_id", "prescription_id")
    ) %>%
    .tbl_add_demographics()
  
  if ( "date_of_birth" %in% colnames(rx_data) ) {
    rx_data <- rx_data %>%
      dplyr::mutate(
        age = dplyr::sql("date_part('year', age(date, date_of_birth))")
      ) %>%
      dplyr::select(-"date_of_birth")
  }
  
  rx_data <- rx_data %>% 
    dplyr::compute(
      name = "metrics_prescribing",
      temporary = FALSE
    )
  
  rx_data
}
