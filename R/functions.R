
clean_diet_file <- function(filepath){
  computrition_export_raw <- suppressMessages(readxl::read_excel(filepath)) %>%
      janitor::clean_names() %>% 
      select(where(function(x) any(!is.na(x))))
  computrition_export <- computrition_export_raw
  
  new_names <- computrition_export[stringr::str_detect(computrition_export$name, "Menu Item"),] %>%
    slice(1) %>%
    as.character() %>%
    stringr::str_replace_all("\n|\r", "") %>% as.vector()
  
  new_names[is.na(new_names)] <- c("meal", "mrn")
  
  names(computrition_export) <- new_names
  
  remove_items <- c("Name", "MRN", "Room", "Menu Item Name", "Acuity", "Current Menu", "Current Diet Order", "TVR Order", "PLEASE RUSH ORDER", "Parent/Guest Tray To Follow")
  # SuppressWarnings for mrn conversion because it will always warn and the current behavior
  # of forcing non-ints to NAs is intentional.
  # the case_when is similar
  
  computrition_export_clean <- computrition_export %>% 
    janitor::clean_names() %>%
    dplyr::mutate(
      mrn = suppressWarnings(as.integer(mrn)),
      meal_date = suppressWarnings(
        dplyr::case_when(
          stringr::str_detect(menu_item_name, "Date: ") ~ lubridate::mdy(menu_item_name), 
          TRUE ~ lubridate::NA_Date_
        )
      )
    ) %>%
    tidyr::fill(c("meal", "mrn", "meal_date"), .direction = "down") %>%
    dplyr::filter(!stringr::str_detect(tolower(menu_item_name), "(daily|[:digit:]{1} [value|average|total])|(condiments, salt|pepper)|(\\*{8})") &
             !stringr::str_detect(menu_item_name, "Date:  ") & !is.na(meal) & 
             !menu_item_name %in% remove_items
    )
  if (! "portion_consumed" %in% colnames(computrition_export_clean)){
    computrition_export_clean$portion_consumed <- NA
  }
  computrition_export_clean <- computrition_export_clean %>%
    dplyr::mutate(
      serving_amt =  gsub("(.*?) (.*)", "\\1", portion_size),
      raw_food_serving_unit =  gsub("(.*?) (.*)", "\\2", portion_size),
      dplyr::across(c(serving_amt), 
             function(x) as.numeric(readr::parse_number(x)),
             .names = "{col}_numeric"),
      serving_amt_numeric = case_when(
        stringr::str_detect(serving_amt, "1/2") ~ 0.5,
        stringr::str_detect(serving_amt, "1/4") ~ 0.25,
        TRUE ~ serving_amt_numeric
      ),
      raw_food_id = sub("^\\^", "", menu_item_name),
      amt_eaten = factor(NA_integer_, levels = c("Missing", 0.0, 0.25, 0.33, 0.5, 0.66, 0.75, 1))
    ) %>%
    dplyr::mutate(raw_food_id = factor(raw_food_id)) %>% #, levels = sort(unique(unit_table$raw_food_id)))) %>%
    dplyr::select(mrn, meal_date, meal, raw_food_id, serving_amt_numeric, raw_food_serving_unit,  portion_consumed,
                  amt_eaten) %>% 
    dplyr::rename(
      computrition_portion_consumed = portion_consumed,
      serving_size=serving_amt_numeric) %>%
    dplyr::mutate(id = paste(mrn, meal_date, meal, raw_food_id, sep = "_"))

  
  return(computrition_export_clean)
}


#' get_mrn_record_id
#'. This function is used to check whether a given MRN is in REDcap.  It would be nice to do this
#' all in one go, but we can't both filter by a non-record-id field (in this case, the MRN)
#' and also return fields from instruments that lack the field we are filtering by.
#' In this case, the MRN is in the baseline pt info instrument, but the diet data is in the computrition instrument.  
#' So, we have to first use this function to fetch the right record ID, then query with that record id
#' to get the actual meal data
#' @param mrn 
#'
#' @return redcapr response object with fields in data for eb_mrn and record_id
#' @export
#'
#' @examples
#' \dontrun{
#' get_mrn_record_id(12345)
#'}
get_mrn_record_id <- function(mrn){
  REDCapR::redcap_read_oneshot(
    col_types = readr::cols(eb_mrn = readr::col_integer()),
    fields = c("record_id", "eb_mrn"),
    filter_logic = paste0("[eb_mrn]=", mrn),
    redcap_uri = Sys.getenv("DIETDATA_REDCAP_URI"),
    token = Sys.getenv("DIETDATA_REDCAP_TOKEN"),
    config_options = redcap_config_options
  )
}

pull_diet_redcap <- function(mrn_vec = NULL) {

  pts_diet_data <- lapply(mrn_vec, FUN = function(mrn){
    this_record_id <- get_mrn_record_id(mrn)

    if (this_record_id$success) {
       REDCapR::redcap_read_oneshot(
        col_types = readr::cols(eb_mrn = readr::col_integer(), raw_food_serving_unit = readr::col_character()),
        records = this_record_id$data$record_id,
        # forms = c("computrition_data"),
        fields = c("record_id", "meal_date", "meal", "eb_mrn", "raw_food_id", "raw_food_serving_unit", "serving_size", "amt_eaten", "upload_date", "uploader"),
        # filter_logic = paste0("[eb_mrn]=", mrn_vec),
        # filter_logic = paste0("[record_id]=", ds_different_cert_file1$record_id),
        redcap_uri = Sys.getenv("DIETDATA_REDCAP_URI"),
        token = Sys.getenv("DIETDATA_REDCAP_TOKEN"),
        config_options = redcap_config_options
      )$data
    } else{
      showNotification(paste("MRN",  mrn, "not added part of this study; please enter this patient's baseline info prior to uploading diet data"))
      data.frame()
    }
    
    
  })
  if (any(sapply(pts_diet_data, FUN = function(x) nrow(x) > 0 ))){
    return(pts_diet_data %>% dplyr::bind_rows())
  } else {
    return(data.frame())
  }
  
}


pull_redcap_pts <- function() {
  REDCapR::redcap_read_oneshot(
    col_types = readr::cols(eb_mrn = readr::col_integer()),
    # records = mrn_vec,
    events = c("baseline_arm_1"),
    fields = c("record_id", "eb_mrn"),
    # filter_logic = paste0("[eb_mrn]=", mrn),
    # filter_logic = filter_statement,
    redcap_uri = Sys.getenv("DIETDATA_REDCAP_URI"),
    token = Sys.getenv("DIETDATA_REDCAP_TOKEN"),
    config_options = redcap_config_options
  )$data
}






clean_diet_redcap <- function(redcap_pull) {
  if(nrow(redcap_pull) > 0) {
    redcap_pull <- redcap_pull%>%
      tidyr::fill(eb_mrn) %>%
      dplyr::filter(redcap_repeat_instrument == "computrition_data") %>%
      dplyr::mutate(id = paste(eb_mrn, meal_date, meal, raw_food_id, sep = "_"))
  } else {
    # There in no blank date type that would allow creation of an empty dataframe with date type for meal_date
    redcap_pull <- data.frame(record_id = numeric(), redcap_event_name = character(), redcap_repeat_instrument = character(), 
                              redcap_repeat_instance = numeric(), eb_mrn = integer(), meal_date = character(), meal = character(),
                              raw_food_id = character(), raw_food_serving_unit = character(), serving_size = integer(), 
                              amt_eaten = numeric(), id = character()
    )
    
  }
  
  redcap_pull
}

push_to_redcap <- function(clean_diet_table) {

  # need the following fields
  tbl_names <-  c("record_id", "redcap_event_name", "redcap_repeat_instrument", "redcap_repeat_instance", "eb_mrn",
  "meal_date", "meal", "raw_food_id", "raw_food_serving_unit", "serving_size", "amt_eaten", "upload_date", "uploader")
  
  # final check against redcap to eliminate duplicates
  # redcap_raw <- pull_diet_redcap(12345678) # if no MRNs in redcap
  redcap_raw <- pull_diet_redcap(unique(clean_diet_table$mrn))
  
  redcap_pull <- clean_diet_redcap(redcap_raw)
  
  redcap_pts <- pull_redcap_pts() %>%
    dplyr::mutate(eb_mrn = as.integer(eb_mrn))
  
  formatted_tbl <- clean_diet_table %>%
    dplyr::mutate(
      mrn = as.integer(mrn),
      id = paste(mrn, meal_date, meal, raw_food_id, sep = "_")) %>%
    dplyr::filter(!id %in% redcap_pull$id) %>%
    dplyr::mutate(redcap_repeat_instrument = "computrition_data", redcap_event_name = "computrition_data_arm_1", eb_mrn = mrn)
  
  # get latest instance number for existing patients
  redcap_dtls <- redcap_pull %>%
    dplyr::arrange(desc(redcap_repeat_instance)) %>%
    dplyr::distinct(record_id, eb_mrn, .keep_all = T) %>%
    dplyr::select(record_id, eb_mrn, redcap_repeat_instance) %>%
    dplyr::mutate(redcap_repeat_instance = redcap_repeat_instance + 1, in_redcap = TRUE)
  
  
  if(exists("next_id")) remove(next_id)
  next_id <- REDCapR::redcap_next_free_record_name(redcap_uri = Sys.getenv("DIETDATA_REDCAP_URI"), token = Sys.getenv("DIETDATA_REDCAP_TOKEN"))
  
  # generate appropriate record_id and repeat_instrument for each unique patient (based one whether they already are in redcap)
  for(mrn in unique(dplyr::filter(formatted_tbl, !eb_mrn %in% redcap_dtls$eb_mrn)$eb_mrn)) {
    
    # if patient is not in redcap baseline (so no record ID has been assigned to their MRN) give them the next available record ID
    if(!mrn %in% redcap_pts$eb_mrn) {
      assigned_id <- next_id 
      next_id <- as.character(as.integer(next_id) + 1)
    } else {
      # if they are in redcap (but have not had any diet data entered) link them to their pre-existing record ID
      assigned_id <- redcap_pts$record_id[redcap_pts$eb_mrn == mrn]
    }
    
    tbl <- data.frame(record_id = assigned_id, eb_mrn = mrn, redcap_repeat_instance = 1, in_redcap = FALSE)
    redcap_dtls <- rbind(redcap_dtls, tbl)
  }
  
  # print("redcap details:")
  # print(redcap_dtls)

  # need to link new patients by MRN to their record ID (so have to add to "baseline" instrument)
  new_pts_to_add <- redcap_dtls %>%
    filter(!in_redcap) %>% 
    mutate(redcap_event_name = "baseline_arm_1") %>%
    select(-c(in_redcap, redcap_repeat_instance))
  
  user_str="unknown"
  # check for Rsconnect username
  if(session$user != ""){
    user_str = session$user
  } else if (Sys.info()['user'] != ""){
    # fall back to local username
    user_str = Sys.info()['user']
  }
  
  # increment the repeat instance for those patients
  formatted_tbl_final <- formatted_tbl %>%
    dplyr::left_join(redcap_dtls, by = "eb_mrn") %>%
    dplyr::mutate(upload_date = as.character(Sys.Date()),
                  uploader =user_str) %>%
    dplyr::select(dplyr::all_of(tbl_names)) %>%
    dplyr::group_by(eb_mrn) %>%
    dplyr::mutate(redcap_repeat_instance = seq(from = max(redcap_repeat_instance, na.rm = T), to = max(redcap_repeat_instance, na.rm = T) + dplyr::n()-1)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(eb_mrn = NA_integer_, 
           amt_eaten = as.character(amt_eaten),
           amt_eaten = dplyr::case_when(amt_eaten == "1" ~ "1.0", amt_eaten == "Missing" ~ "-1.0", TRUE ~ amt_eaten)) %>%
    dplyr::bind_rows(new_pts_to_add)
  

  
  
  REDCapR::redcap_write(
    ds_to_write = formatted_tbl_final,
    redcap_uri = Sys.getenv("DIETDATA_REDCAP_URI"),
    token = Sys.getenv("DIETDATA_REDCAP_TOKEN"),
    config_options = redcap_config_options,
    overwrite_with_blanks = F, # for now we can keep what is already in there
    verbose = T # don't print any details (to minimize printing of PHI)
  )
  
  cat(paste("the following were written to redcap:", formatted_tbl_final, "", sep="\n"))
}








redcap_delete_event <- function (redcap_uri, token, records_to_delete, arm_of_records_to_delete = NULL, 
                           verbose = TRUE, config_options = NULL, event_name = NULL, instrument_name = NULL, 
                           repeat_instance_id = NULL) 
{
  checkmate::assert_character(redcap_uri, any.missing = FALSE, 
                              len = 1, pattern = "^.{1,}$")
  checkmate::assert_character(token, any.missing = FALSE, len = 1, 
                              pattern = "^.{1,}$")
  checkmate::assert_vector(records_to_delete, any.missing = FALSE, 
                           min.len = 1)
  checkmate::assert_integer(arm_of_records_to_delete, any.missing = FALSE, 
                            null.ok = TRUE, len = 1, lower = 1)
  token <- sanitize_token(token)
  verbose <- REDCapR:::verbose_prepare(verbose)
  records_to_delete <- as.character(records_to_delete)
  checkmate::assert_character(records_to_delete, any.missing = FALSE, 
                              min.len = 1)
  records_to_delete <- stats::setNames(records_to_delete, sprintf("records[%i]", 
                                                                  seq_along(records_to_delete) - 1))
  arms_call <- redcap_arm_export(redcap_uri, token, verbose = FALSE, 
                                 config_options)
  if (arms_call$has_arms & is.null(arm_of_records_to_delete)) {
    stop("This REDCap project has arms.  Please specify which arm contains the records to be deleted.")
  }
  else if (!arms_call$has_arms & !is.null(arm_of_records_to_delete)) {
    stop("This REDCap project does not have arms, but `arm_of_records_to_delete` is not NULL.")
  }
  arm_list <- if (is.null(arm_of_records_to_delete)) {
    NULL
  }
  else {
    list(arm = arm_of_records_to_delete)
  }
  
  # rv <- lapply(events, identity)
  # names(rv) <- sprintf("%s[%s]", 
  #                      "event", 
  #                      seq_along(rv))
  # 
  # rv2 <- lapply(repeat_instance_id, identity)
  # names(rv2) <- sprintf("%s[%s]", 
  #                      "repeat_instance", 
  #                      seq_along(rv2))
  
  
  
  post_body <- c(list(token = token, content = "record", action = "delete"), 
                 arm_list, records_to_delete, event = event_name, instrument = instrument_name
                 )
  
  if(!is.null(repeat_instance_id)) {
    post_body <- c(post_body, repeat_instance = repeat_instance_id)
  }
  
  print(post_body)
  # return()
  
  try({
    kernel <- REDCapR:::kernel_api(redcap_uri, post_body, config_options)
    # return(kernel$result)

      }, silent = FALSE)
  
  
  if (exists("kernel")) {
    if (kernel$success) {
      records_affected_count <- as.integer(kernel$raw_text)
      outcome_message <- sprintf(paste("The %s records were deleted from REDCap in %0.1f seconds.", 
                                       "The http status code was %i."), format(records_affected_count, 
                                                                               big.mark = ",", scientific = FALSE, trim = TRUE), 
                                 kernel$elapsed_seconds, kernel$status_code)
      kernel$raw_text <- ""
    }
    else {
      records_affected_count <- 0
      error_message <- sprintf(paste("The REDCapR record deletion failed.", 
                                     "The http status code was %i.", "The error message was: '%s'."), 
                               kernel$status_code, kernel$raw_text)
      stop(error_message)
    }
  }
  else {
    error_message <- sprintf(paste("The REDCapR record deletion was not successful.", 
                                   "The error message was:\n%s"), kernel$raw_text)
    stop(error_message)
  }
  if (verbose) 
    message(outcome_message)
  list(success = kernel$success, status_code = kernel$status_code, 
       outcome_message = outcome_message, records_affected_count = records_affected_count, 
       elapsed_seconds = kernel$elapsed_seconds, raw_text = kernel$raw_text)
}

# example usage

# remove computrition data for given record IDs
# kernel_return <- redcap_delete_event(redcap_uri = Sys.getenv("DIETDATA_REDCAP_URI"), token = Sys.getenv("DIETDATA_REDCAP_TOKEN"), records_to_delete = c(1, 42, 44), arm_of_records_to_delete = 1L, repeat_instance_id = 1, instrument_name = "computrition_data", event_name = "computrition_data_arm_1", verbose = T)

# to remove multiple rows of computrition data per patient, loop over given number of rows
# lapply(c(1:10), function(x) redcap_delete_event(redcap_uri = Sys.getenv("DIETDATA_REDCAP_URI"), token = Sys.getenv("DIETDATA_REDCAP_TOKEN"), records_to_delete = c(44), arm_of_records_to_delete = 1L, repeat_instance_id = x, instrument_name = "computrition_data", event_name = "computrition_data_arm_1", verbose = T))

# if you need to remove a patient entirely (unassign their record ID), need to remove them from the baseline instrument
# (note that this will still leave a blank unassigned record that needs to be manually deleted from redcap)
# kernel_return <- redcap_delete_event(redcap_uri = Sys.getenv("DIETDATA_REDCAP_URI"), token = Sys.getenv("DIETDATA_REDCAP_TOKEN"), records_to_delete = c(44), arm_of_records_to_delete = 1L, instrument_name = "patient_information", event_name = "baseline_arm_1", verbose = T)


get_meal_entries <- function(){
  col_types = readr::cols(
    .default = readr::col_character(),
    record_id = readr::col_double(),
  )
  REDCapR::redcap_read(records = NULL,
                       col_types=col_types,
                       verbose = TRUE,batch_size = 1000,
                       redcap_uri = Sys.getenv("DIETDATA_REDCAP_URI"),
                       token = Sys.getenv("DIETDATA_REDCAP_TOKEN"),
  )$data  %>%
    tidyr::fill(record_id, eb_mrn) %>% 
    dplyr::filter(!is.na(raw_food_id)) %>% 
    dplyr::select(record_id, eb_mrn, meal_date, meal, raw_food_id, serving_size, raw_food_serving_unit, amt_eaten) %>%
    dplyr::distinct()
  
}
