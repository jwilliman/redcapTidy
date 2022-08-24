#' Imported REDCap csv files and combine into single list.
#'
#' Clean raw .csv data exported from a REDCap database, and export a list of data.frames
#'
#' @param folder The folder containing the files downloaded from the REDCap
#'   database. The four files required include the Data Dictionary, Events
#'   (under the 'Define my events' tab), Instrument Mappings (under the
#'   'Designate Instruments for My Events' tab), and the raw Record data (under
#'   the 'My Reports & Exports' tab). All files should be saved as .csv.
#' @param ids Names of identifiers, for inclusion on all output datasets.
#'
#' @return A named list containing four data frames: dd = metadata, evnt = Events, inst = Instrument mappings, rcrd = Records.
#' @export
#'
#' @examples
rc_read_csv <- function(folder) {


  ## Collect names of input data files ----------------------------------------
  Inputs <- list(
    # 1 Import Data dictionary
    dd = sort(list.files(folder, "Dictionary"), decreasing = TRUE)[1],

    # 2 Import events
    evnt = sort(list.files(folder, "Events"), decreasing = TRUE)[1],

    # 3 Import instrument mappings
    inst = sort(list.files(folder, "Instrument"), decreasing = TRUE)[1],

    # 4 Import REDCap record data
    rcrd = sort(list.files(folder, "DATA"), decreasing = TRUE)[1]
  )

  Inputs <- Inputs[!sapply(Inputs, is.na)]

  ## Import data
  object <- lapply(file.path(folder, Inputs), read.csv, stringsAsFactors = FALSE)
  names(object) <- names(Inputs)

  ## Tidy data dictionary names
  names(object$dd) <-  c(
    "field_name", "form_name", "section_header", "field_type", "field_label",
    "select_choices_or_calculations", "field_note",
    "text_validation_type_or_show_slider_number", "text_validation_min",
    "text_validation_max", "identifier", "branching_logic",
    "required_field", "custom_alignment", "question_number",
    "matrix_group_name", "matrix_ranking", "field_annotation")


  # Format and clean variables ---------------------------------------------------------
  ## Character fields (replace blanks with NA)
  cols_chr <- names(object$rcrd)[sapply(object$rcrd, is.character)]
  for (x in cols_chr)
    object$rcrd[, x][object$rcrd[, x] == ""] <- NA


  ## Dates and date-time variables
  cols_dt <- object$dd$field_name[
    grepl("date_", object$dd$text_validation_type_or_show_slider_number)]

  object$rcrd[, cols_dt] <- lapply(object$rcrd[, cols_dt], as.character) # Correct empty date fields
  object$rcrd[, cols_dt] <- lapply(object$rcrd[, cols_dt], as.Date, format = "%Y-%m-%d")

  cols_dttm <- c(
    object$dd$field_name[grepl("datetime_", object$dd$text_validation_type_or_show_slider_number)],
    grep("timestamp", names(object$rcrd), value = TRUE))
  object$rcrd[, cols_dttm] <- lapply(object$rcrd[, cols_dttm], as.character)
  object$rcrd[, cols_dttm] <- lapply(object$rcrd[, cols_dttm], as.POSIXct, format = "%Y-%m-%d %H:%M")


  ## Logical (checkboxs)
  cols_lg <- c(
    object$dd$field_name[object$dd$field_type == "yesno" |
                           object$dd$select_choices_or_calculations %in% c(
                             "0, Incorrect | 1, Correct", "0, No | 1, Yes", "1, True | 0, False")],
    unlist(sapply(object$dd$field_name[object$dd$field_type == "checkbox"], function(x)
      grep(paste0(x, "___"), names(object$rcrd), value = TRUE))))
  object$rcrd[, cols_lg] <- lapply(object$rcrd[, cols_lg], as.logical)


  ## Factors (radio)
  cols_fct <- object$dd$field_name[object$dd$field_type %in% c("radio", "dropdown") & !(object$dd$field_name %in% cols_lg)]

  fct_label <- function(x) {
    ## Replace first ',' with '|' before repeating split.
    lbls <- sapply(strsplit(sub(",", "|", strsplit(
      object$dd$select_choices_or_calculations[object$dd$field_name == x],
      split = "\\|")[[1]]), "\\|"), trimws)
    factor(object$rcrd[, x], levels = lbls[1, ], labels = lbls[2, ])
  }

  object$rcrd[, cols_fct] <- lapply(cols_fct, fct_label)


  ## Return object
  return(object)

}



#' Read in record and metadata from REDCap using redcapAPI
#'
#' A wrapper for importing data Records, data dictionary, events, and instrument
#' designation mappings from REDCap. Uses the redcapAPI package.
#'
#' @param url URL for a REDCap database API. Check your institution's REDCap
#'   documentation for this address.
#' @param token REDCap API token
#' @param yesno Determine how to return REDCap 'Yes - No' fields; options include 'factor' (default), 'numeric', or 'logical'.
#'
#' @return A named list containing four dataframes: dd = metadata, evnt =
#'   Events, inst = Instrument mappings, rcrd = Records.
#' @export
#'
#' @examples
rc_read_api <- function(url, token, yesno = "factor", label = FALSE) {

  rcon <- redcapAPI::redcapConnection(url=url, token=token)

  #redcapAPI::exportBundle(rcon)

  ## Read in data dictionary, event and instrument tables, and raw data from REDCap
  object <- list(

    dd   = redcapAPI::exportMetaData(rcon),
    evnt = redcapAPI::exportEvents(rcon),
    inst = redcapAPI::exportMappings(rcon),
    rcrd = redcapAPI::exportRecords(rcon, label = label)

  )

  ## If not a longitudinal project

  ### Create single event
  object$event <- data.frame(
    event_name = "All instruments",
    arm_num    = 1,
    day_offset = 0, offset_min = 0, offset_max = 0,
    unique_event_name = "all_instruments_arm_1",
    custom_event_label = "all_instruments"
  )

  ### Allocate all forms to single event
  object$inst = data.frame(
    arm_num = 1,
    unique_event_name = "all_instruments_arm_1",
    form = unique(object$dd$form_name)
  )

  ### Add event name as second column in record data.frame
  object$rcrd <- cbind(
    object$rcrd[,1],
    redcap_event_name = "all_instruments_arm_1",
    object$rcrd[,-1]
  )

  ## Convert YesNo fields
  if(yesno != "factor") {

    cols_yn <- unique(c(
      object$dd$field_name[object$dd$field_type == "yesno" |
                             object$dd$select_choices_or_calculations %in% c(
                               "0, Incorrect | 1, Correct", "0, No | 1, Yes", "1, True | 0, False")],
      unlist(sapply(object$dd$field_name[object$dd$field_type == "checkbox"], function(x)
        grep(paste0(x, "___"), names(object$rcrd), value = TRUE)))))

    ## To numeric (0 or 1)
    if(yesno == "numeric")
      object$rcrd[, cols_yn] <- lapply(object$rcrd[, cols_yn], redcapAPI::redcapFactorFlip)

    ## To logical
    else if(yesno == "logical")
      object$rcrd[, cols_yn] <- lapply(
        object$rcrd[, cols_yn], function(x) as.logical(redcapAPI::redcapFactorFlip(x)))
  }

  return(object)

}



#' Tidy list of datasets from REDCap.
#'
#'
#' @param object An named list containing the following data.frames; metadata (dd),
#' events (evnt), instruments (inst) and records (rcrd).
#' @param ids Names of identifiers, for inclusion on all output datasets.
#' @param label Add labels to variables. Supply name of labelling package,
#'   \code{Hmisc} or \code{sjlabelled}.
#' @param repeated How shall repeated forms be treated in datasets assembled by
#'   event. Options are: exclude, include, or nest. Nest uses `tidyr` package to
#'   collapse by row id.
#'
#' @return A list of dataframes, with variables grouped by event or by data collection form.
#' @export
#'
#' @examples
rc_tidy <- function(object, ids = NULL, label = FALSE, repeated = "exclude") {

  ## If ID columns not specified take first column and any beginning with 'redcap'.
  if(is.null(ids))
    ids <- names(object$rcrd)[[1]]
  ids_rc <- c(ids, grep("^redcap", names(object$rcrd), value = TRUE, ignore.case = TRUE))


  # Create list of datasets ---------------------------------------------------
  dat_ed <- vector("list", 2)
  names(dat_ed) <- c("form", "event")

  ## By form (data collection instrument)
  ### All forms
  forms <- unique(object$dd$form_name)
  ### Repeating forms
  forms_rpt <- unique(object$rcrd$redcap_repeat_instrument[!is.na(
    object$rcrd$redcap_repeat_instrument
  )])

  if(is.null(forms_rpt))
    repeated = "exclude"

  ## Form completion
  cols_cmp <- intersect(
    ## All possible form names
    paste0(forms, "_complete"),
    ## Forms actually used
    grep("_complete$", names(object$rcrd), value = TRUE))


  if(all(sapply(object$rcrd[, cols_cmp], is.numeric))) {

    object$rcrd[, cols_cmp] <- lapply(object$rcrd[, cols_cmp], function(x)
      factor(x, levels = c("Incomplete", "Unverified", "Complete")))

  } else {

    object$rcrd[, cols_cmp] <- lapply(
      object$rcrd[, cols_cmp], factor, levels = c("Incomplete", "Unverified", "Complete"))

  }

  ## Form timestamps
  cols_tmstmp <- intersect(paste0(forms, "_timestamp"), names(object$rcrd))
  ### Drop timestamp values recorded as 'not completed'.
  object$rcrd[, cols_tmstmp][object$rcrd[, cols_tmstmp] == "[not completed]"] <- NA
  object$rcrd[, cols_tmstmp] <- lapply(object$rcrd[, cols_tmstmp], as.POSIXct)

  ## Create list with name, columns, events, and repeating status by form
  form_data <- sapply(forms, function(form) {

    vars  <- object$dd$field_name[object$dd$form_name %in% form]
    cols  <- unlist(sapply(vars, function(x)
      grep(
        paste(paste0("^", x, c("$", "___")), collapse = "|")
        , names(object$rcrd), value = TRUE
      )))
    ## Add form timestamp and complete columns. Ensure names are in dataset
    cols <- c(cols, paste(form, c("timestamp", "complete"), sep = "_"))
    cols <- intersect(cols, names(object$rcrd))

    if("evnt" %in% names(object))
      events <- object$inst$unique_event_name[object$inst$form == form]
    else
      events <- NA
    repeating <- form %in% forms_rpt
    return(list(name = form, vars = vars, cols = cols, events = events, repeating = repeating))

  }, simplify = FALSE)

  ## Create list of data by form
  dat_ed$form <-  lapply(form_data, function(form){

    if(form$repeating) {
      data  <- object$rcrd[
        object$rcrd$redcap_repeat_instrument %in% form$name
        , unique(c(ids_rc, form$cols))]
    } else if (!is.null(forms_rpt)) {
      data  <- object$rcrd[
        is.na(object$rcrd$redcap_repeat_instrument)
        , unique(c(ids_rc, form$cols))]
    } else {
      data  <- object$rcrd[, unique(c(ids_rc, form$cols))]
    }

    ## Only run for longitudinal project
    if("redcap_event_name" %in% names(object$rcrd)) {
      data <- data[data$redcap_event_name %in% form$events,]

      ## Add in days offset column for each event (if exists)
      if(!is.null(object$evnt$day_offset)) {
        col_rc_evnt <- grep("redcap_event_name", names(data))

        redcap_event_day_offset <- as.numeric(as.character(
          factor(
            data$redcap_event_name, levels = object$evnt$unique_event_name,
            labels = object$evnt$day_offset)
        ))

        data <- cbind(
          data[, 1:col_rc_evnt],
          redcap_event_day_offset,
          data[, (col_rc_evnt + 1):ncol(data)])
      }
    }

    ## Add labels if requested
    if(label != FALSE) {
      labs <- sapply(names(data), function(x) {
        object$dd$field_label[object$dd$field_name %in% gsub("___[0-9]+$","",x)]
      })
      labs[lengths(labs) == 0] <- NA_character_

      if(label == "Hmisc") {
        Hmisc::label(data[!is.na(unlist(labs))], self = FALSE) <- na.omit(unlist(labs))
      } else if(grepl("^sj", label)) {
        data <- sjlabelled::set_label(data, label = unlist(labs))
      } else if(label == "labelled") {
        labelled::var_label(data) <- labs[!is.na(labs)]
      }
    }

    return(data)

  })



  ## Create list of datasets by event name --------------------------------------
  if("evnt" %in% names(object)) {

    # events <- object$evnt$unique_event_name Not all defined events have instuments attached to them.
    events <- unique(object$inst$unique_event_name)

    dat_ed$event <- sapply(events, function(event) {

      ## Approach 2: Identify required forms and columns
      forms  <- form_data[sapply(form_data, function(form) event %in% form$events)]
      cols   <- sapply(forms, "[[", "cols")

      ### Separate by whether repeating forms are included and how they should be treated
      if(is.null(forms_rpt) | repeated == "include") {

        ## Keep all columns
        data <- object$rcrd[
          object$rcrd$redcap_event_name == event
          , unique(c(ids_rc, unlist(cols)))]

      } else {

        cols_rpt   <- sapply(forms[ sapply(forms, "[[", "repeating")], "[[", "cols")
        cols_norpt <- sapply(forms[!sapply(forms, "[[", "repeating")], "[[", "cols")

        ## Keep only non-repeated columns. (Repeated == "exclude". Default.)
        data <- object$rcrd[
          object$rcrd$redcap_event_name == event &
            is.na(object$rcrd$redcap_repeat_instrument)
          , unique(c(ids, unlist(cols_norpt)))]

        if(repeated == "nest") {

          ## Keep and nest repeated columns
          dats_rpt <- sapply(names(cols_rpt), function(x) {

            dat_t1 <- object$rcrd[
              object$rcrd$redcap_event_name == event &
                object$rcrd$redcap_repeat_instrument %in% x
              , unique(c(ids, "redcap_repeat_instance", unlist(cols_rpt[x])))]

            tidyr::nest(dat_t1, !! x := -ids)

          }, simplify = FALSE)


          data <- Reduce(function(...) merge(..., all = TRUE, by = ids, sort = FALSE)
                         , c(norpt = list(data), dats_rpt))

        }

      }

      if(label != FALSE) {
        labs <- sapply(names(data), function(x) {
          object$dd$field_label[object$dd$field_name %in% gsub("___[0-9]+$","",x)]
        })
        labs[lengths(labs) == 0] <- NA_character_

        if(label == "Hmisc") {
          Hmisc::label(data[!is.na(unlist(labs))], self = FALSE) <- na.omit(unlist(labs))
        } else if(grepl("^sj", label)) {
          data <- sjlabelled::set_label(data, label = unlist(labs))
        } else if(label == "labelled") {
          labelled::var_label(data) <- labs[!is.na(labs)]
        }
      }

      return(data)

    }, simplify = FALSE)

  }

  return(dat_ed)

}
