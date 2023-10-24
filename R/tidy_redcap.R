
#' Clean variable types of raw data export
#'
#' @param data Data exported from REDCap via csv or API call.
#' @param dictionary Data dictionary exported from REDCap via csv or API call.
#' @param yesno Determine how to return REDCap 'Yes - No' fields; options include 'factor' (default), 'numeric', or 'logical'.
#'
#' @return The REDCap dataset with variable types cleaned.
#' @importFrom redcapAPI redcapFactorFlip
#' @export
#'

rc_format_variables <- function(data, dictionary, yesno = "logical") {

  ## Tidy data dictionary names
  dictionary_tidy_names <-  c(
    "field_name", "form_name", "section_header", "field_type", "field_label",
    "select_choices_or_calculations", "field_note",
    "text_validation_type_or_show_slider_number", "text_validation_min",
    "text_validation_max", "identifier", "branching_logic",
    "required_field", "custom_alignment", "question_number",
    "matrix_group_name", "matrix_ranking", "field_annotation")

  if(!identical(names(dictionary), dictionary_tidy_names))
    names(dictionary) <- dictionary_tidy_names

  ## Fields actually present in downloaded dataset
  cols_data <- names(data)


## Character fields (replace blanks with NA)
  cols_chr <- names(data)[sapply(data, is.character)]
  for (x in cols_chr)
    data[, x][data[, x] == ""] <- NA


## Dates and date-time variables to Dates or POSIXct variables, correcting empty date fields
  cols_dt <- intersect(
    cols_data,
    dictionary$field_name[
    grepl("date_", dictionary$text_validation_type_or_show_slider_number)]
  )

  for(col in cols_dt) {
    data[, col] <- as.character(data[, col]) |> as.Date(format = "%Y-%m-%d")
  }

  cols_dttm <- intersect(
    cols_data, c(
      dictionary$field_name[grepl("datetime_", dictionary$text_validation_type_or_show_slider_number)],
      grep("timestamp", names(data), value = TRUE))
  )

  for(col in cols_dttm) {
    data[, col] <- as.character(data[, col]) |> as.POSIXct(format = "%Y-%m-%d %H:%M")
  }

  ## Logical variables (yesno radios and checkboxs)
  if(yesno != "factor") {

    cols_yn <- intersect(
      ## Ensure returned names are in the extracted dataset
      cols_data,

      unique(c(
        ## Retrieve names of all binary fields coded as 0, 1
        dictionary$field_name[
          dictionary$field_type == "yesno" |
            dictionary$select_choices_or_calculations %in% c(
              "0, Incorrect | 1, Correct", "1, Correct | 0, Incorrect",
              "0, No | 1, Yes", "1, Yes | 0, No",
              "1, True | 0, False", "0, False | 1, True")],

        ## Retrieve/create names of all checkbox fields
        unlist(sapply(dictionary$field_name[dictionary$field_type == "checkbox"], function(x)
          grep(paste0(x, "___"), names(data), value = TRUE)))))

    )


    ## To numeric (0 or 1)
    if(yesno == "numeric")
      data[, cols_yn] <- lapply(data[, cols_yn], as.logical(x) |> as.integer())

    ## To logical
    else if(yesno == "logical")
      data[, cols_yn] <- lapply(
        data[, cols_yn], as.logical)

  }

  ## Factors (radio)
  cols_fct <- intersect(
    dictionary$field_name[
      dictionary$field_type %in% c("radio", "dropdown") & !(dictionary$field_name %in% cols_yn)],
    # Check variable hasn't already been converted
    names(data)[sapply(data, is.numeric)]
  )

  fct_label <- function(x) {
    ## Replace first ',' with '|' before repeating split.
    lbls <- sapply(strsplit(sub(",", "|", strsplit(
      dictionary$select_choices_or_calculations[dictionary$field_name == x],
      split = "\\|")[[1]]), "\\|"), trimws)
    factor(data[, x], levels = lbls[1, ], labels = lbls[2, ])
  }

  data[, cols_fct] <- lapply(cols_fct, fct_label)

  return(data)

}




#' Imported REDCap csv files and combine into single list.
#'
#' Clean raw .csv data exported from a REDCap database, and export a list of data.frames
#'
#' @param folder The folder containing the files downloaded from the REDCap
#'   database. The four files required include the Data Dictionary, Events
#'   (under the 'Define my events' tab), Instrument Mappings (under the
#'   'Designate Instruments for My Events' tab), and the raw Record data (under
#'   the 'My Reports & Exports' tab). All files should be saved as .csv.
#' @param yesno Determine how to return REDCap 'Yes - No' fields; options include 'factor' (default), 'numeric', or 'logical'.
#' @param longitudinal Is the study longitudinal or not. If longitudinal, then event and instrument mapping files should be provided as well.
#' @param tidy_format Tidy variable formats or not?
#'
#' @return A named list containing four data frames: dd = metadata, evnt = Events, inst = Instrument mappings, rcrd = Records.
#' @importFrom utils read.csv
#' @importFrom checkmate assert_true test_true
#' @export
#'

rc_read_csv <- function(folder, yesno = "logical", longitudinal = NULL, tidy_formats = TRUE) {

  files_csv <- list.files(folder, ".csv")

  ## Check that the required files are present
  checkmate::assert_true(any(grepl("_DATA_", files_csv)), .var.name = "REDCap Data file")
  checkmate::assert_true(any(grepl("_DataDictionary_", files_csv)), .var.name = "REDCap Data dictionary file")


  ## Check if study is longitudinal and if required files exist
  if(is.null(longitudinal)) {

    files_long <- checkmate::test_true(
      any(grepl("Events", files_csv)) &
        any(grepl("Instrument", files_csv)))


    if(files_long)
      longitudinal = TRUE
    else
      longitudinal = FALSE

  } else {

    if(longitudinal) {

      checkmate::assert_true(any(grepl("_Events_", files_csv)), .var.name = "REDCap Events file")
      checkmate::assert_true(any(grepl("_Instruments_", files_csv)), .var.name = "REDCap Instrument file")

    }
  }


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
  object <- lapply(file.path(folder, Inputs), utils::read.csv, stringsAsFactors = FALSE)
  names(object) <- names(Inputs)

  ## Tidy data dictionary names
  names(object$dd) <-  c(
    "field_name", "form_name", "section_header", "field_type", "field_label",
    "select_choices_or_calculations", "field_note",
    "text_validation_type_or_show_slider_number", "text_validation_min",
    "text_validation_max", "identifier", "branching_logic",
    "required_field", "custom_alignment", "question_number",
    "matrix_group_name", "matrix_ranking", "field_annotation")

  ## Tidy formatting of variables in dataset
  if(tidy_formats)
  {
    object$rcrd <- rc_format_variables(data = object$rcrd, dictionary = object$dd, yesno = yesno)
  }

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
#' @param labels Passed to `redcapAPI::exportRecords`, determines if the variable labels are applied to the data frame. `FALSE` by default.
#'
#' @return A named list containing four dataframes: dd = metadata, evnt =
#'   Events, inst = Instrument mappings, rcrd = Records.
#' @importFrom redcapAPI redcapConnection exportMetaData exportEvents exportMappings exportRecords
#' @export
#'

rc_read_api <- function(url, token, yesno = "logical", labels = FALSE) {

  rcon <- redcapAPI::redcapConnection(url=url, token=token)
  #redcapAPI::exportBundle(rcon)

  ## Read in data dictionary, event and instrument tables, and raw data from REDCap
  object <- list(

    dd   = redcapAPI::exportMetaData(rcon),
    evnt = redcapAPI::exportEvents(rcon),
    inst = redcapAPI::exportMappings(rcon),
    rcrd = redcapAPI::exportRecords(
      rcon, factors = TRUE, labels = labels, dates = FALSE, dag = TRUE, checkboxLabels = TRUE)

  )

  ## Tidy formatting of variables in dataset
  object$rcrd <- rc_format_variables(data = object$rcrd, dictionary = object$dd)

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
#' @param label_checkbox If labels are applied, how should checkbox items be labelled?
#' If set to \code{TRUE} then choice labels are returned, if \code{FALSE} then the field label
#' is returned for all items. If a character value is provided, the both the field label and choick label
#' are return separated by the character value.
#' @param repeated How shall repeated forms be treated in datasets assembled by
#'   event. Options are: exclude, include, or nest. Nest uses `tidyr` package to
#'   collapse by row id.
#'
#' @return A list of dataframes, with variables grouped by event or by data collection form.
#' @importFrom stats setNames na.omit
#' @importFrom Hmisc label
#' @importFrom sjlabelled set_label
#' @importFrom labelled var_label
#' @importFrom tidyr nest
#' @export
#'

rc_tidy <- function(object, ids = NULL, label = FALSE, label_checkbox = TRUE, repeated = "exclude") {

  ## If not a longitudinal project, create single event containing all forms
  if(
    (is.null(object$evnt) | !"evnt" %in% names(object)) &
    (is.null(object$inst) | !"inst" %in% names(object)) &
    !"redcap_event_name" %in% names(object$rcrd)
  ) {

    ### Create single event
    object$evnt <- data.frame(
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
      object$rcrd[, 1, drop = FALSE],
      redcap_event_name = "all_instruments_arm_1",
      object$rcrd[,-1, drop = FALSE]
    )

  }


  ## If ID columns not specified take first column and any beginning with 'redcap'.
  if(is.null(ids))
    ids <- names(object$rcrd)[[1]]
  ids_rc <- c(ids, grep("^redcap", names(object$rcrd), value = TRUE, ignore.case = TRUE))

  # Create list of datasets ----------------------------------------------------
  dat_ed <- vector("list", 2)
  names(dat_ed) <- c("form", "event")

  ## By form (data collection instrument) --------------------------------------
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
      factor(x, levels = 1:3, labels = c("Incomplete", "Unverified", "Complete")))

  } else {

    object$rcrd[, cols_cmp] <- lapply(
      object$rcrd[, cols_cmp], factor, levels = c("Incomplete", "Unverified", "Complete"))

  }

  ## Form timestamps
  cols_tmstmp <- intersect(paste0(forms, "_timestamp"), names(object$rcrd))
  ### Drop timestamp values recorded as 'not completed'. (Done already during previous step)
  # object$rcrd[, cols_tmstmp][object$rcrd[, cols_tmstmp] == "[not completed]"] <- NA
  # object$rcrd[, cols_tmstmp] <- lapply(object$rcrd[, cols_tmstmp], as.POSIXct)

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

        ## Find original name of checkboxes
        x_ = gsub("___[0-9]+$", "", x)

        if(x_ %in% object$dd$field_name) {

          x_dd <- object$dd[object$dd$field_name %in% x_,]

          ## Labelling options for checkboxes.
          if(!is.null(x_dd) & x_dd$field_type %in% "checkbox") {

            cb_opts <- trimws(strsplit(x_dd$select_choices_or_calculations, "|", fixed = TRUE)[[1]])
            cb_codes <- sapply(
              strsplit(cb_opts, ", ", fixed = TRUE), function(x)
                setNames(x[[2]], x[[1]]))
            cb_label <- cb_codes[sub(".*___([0-9]+)$", "\\1", x)]

            ## Combine checkbox field label and choice value
            if(is.character(label_checkbox))
              return(paste(x_dd$field_label, sep = label_checkbox, cb_label))
            ## Just return choice value
            else if(is.logical(label_checkbox) & label_checkbox)
              return(unname(cb_label))
            ## Just return field labbel
            else
              return(x_dd$field_label)
          } else {
            return(object$dd$field_label[object$dd$field_name %in% x])
          }} else {

            return(NA_character_)
          }})

      if(label == "Hmisc") {
        Hmisc::label(data[!is.na(unlist(labs))], self = FALSE) <- stats::na.omit(unlist(labs))
      } else if(grepl("^sj", label)) {
        data <- sjlabelled::set_label(data, label = unlist(labs))
      } else if(label == "labelled") {
        labelled::var_label(data) <- labs[!is.na(labs)]
      }
    }

    return(data)

  })



  ## Create list of datasets by event name -------------------------------------
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

          ## Find original name of checkboxes
          x_ = gsub("___[0-9]+$", "", x)

          if(x_ %in% object$dd$field_name) {

            x_dd <- object$dd[object$dd$field_name %in% x_,]

            ## Labelling options for checkboxes.
            if(!is.null(x_dd) & x_dd$field_type %in% "checkbox") {

              cb_opts <- trimws(strsplit(x_dd$select_choices_or_calculations, "|", fixed = TRUE)[[1]])
              cb_codes <- sapply(
                strsplit(cb_opts, ", ", fixed = TRUE), function(x)
                  setNames(x[[2]], x[[1]]))
              cb_label <- cb_codes[sub(".*___([0-9]+)$", "\\1", x)]

              ## Combine checkbox field label and choice value
              if(is.character(label_checkbox))
                return(paste(x_dd$field_label, sep = label_checkbox, cb_label))
              ## Just return choice value
              else if(is.logical(label_checkbox) & label_checkbox)
                return(unname(cb_label))
              ## Just return field labbel
              else
                return(x_dd$field_label)
            } else {
              return(object$dd$field_label[object$dd$field_name %in% x])
            }} else {

              return(NA_character_)
            }})

        if(label == "Hmisc") {
          Hmisc::label(data[!is.na(unlist(labs))], self = FALSE) <- stats::na.omit(unlist(labs))
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
