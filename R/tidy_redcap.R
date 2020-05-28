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
#' @return
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

  return(object)

}

#' Tidy list of datasets from REDCap.
#'
#' Clean raw .csv data exported from a REDCap database, and export a list of
#' data.frames
#'
#' @param folder The folder containing the files downloaded from the REDCap
#'   database. The four files required include the Data Dictionary, Events
#'   (under the 'Define my events' tab), Instrument Mappings (under the
#'   'Designate Instruments for My Events' tab), and the raw Record data (under
#'   the 'My Reports & Exports' tab). All files should be saved as .csv.
#' @param ids Names of identifiers, for inclusion on all output datasets.
#' @param label Add labels to variables. Supply name of labelling package,
#'   \code{Hmisc} or \code{sjlabelled}.
#' @param repeated How shall repeated forms be treated in datasets assembled by
#'   event. Options are: exclude, include, or nest. Nest uses `tidyr` package to
#'   collapse by row id.
#'
#' @return
#' @export
#'
#' @examples
rc_tidy <- function(object, ids = NULL, label = FALSE, repeated = "exclude") {

  if(is.null(ids))
    ids <- names(object$rcrd)[[1]]
  ids_rc <- c(ids, grep("^redcap", names(object$rcrd), value = TRUE, ignore.case = TRUE))

  # 5 Format and clean variables ---------------------------------------------------------
  ## Character fields (replace blanks with NA)
  cols_chr <- names(object$rcrd)[sapply(object$rcrd, is.character)]
  for (x in cols_chr)
    object$rcrd[, x][object$rcrd[, x] == ""] <- NA


  ## Dates and date-time variables
  cols_dt <- object$dd[[1]][grepl("date_", object$dd$Text.Validation.Type.OR.Show.Slider.Number)]
  object$rcrd[, cols_dt] <- lapply(object$rcrd[, cols_dt], as.Date, format = "%Y-%m-%d")

  cols_dttm <- c(
    object$dd[[1]][grepl("datetime_", object$dd$Text.Validation.Type.OR.Show.Slider.Number)],
    grep("timestamp", names(object$rcrd), value = TRUE))
  object$rcrd[, cols_dttm] <- lapply(object$rcrd[, cols_dttm], as.POSIXct, format = "%Y-%m-%d %H:%M")


  ## Logical (checkboxs)
  cols_lg <- c(
    object$dd[[1]][object$dd$Field.Type == "yesno" |
                     object$dd$Choices..Calculations..OR.Slider.Labels %in% c(
                       "0, Incorrect | 1, Correct", "0, No | 1, Yes", "1, True | 0, False")],
    unlist(sapply(object$dd[[1]][object$dd$Field.Type == "checkbox"], function(x)
      grep(paste0(x, "___"), names(object$rcrd), value = TRUE))))
  object$rcrd[, cols_lg] <- lapply(object$rcrd[, cols_lg], as.logical)


  ## Factors (radio)
  cols_fct <- object$dd[[1]][object$dd$Field.Type %in% c("radio", "dropdown") & !(object$dd[[1]] %in% cols_lg)]

  fct_label <- function(x) {
    ## Replace first ',' with '|' before repeating split.
    lbls <- sapply(strsplit(sub(",", "|", strsplit(
      object$dd$Choices..Calculations..OR.Slider.Labels[object$dd[[1]] == x],
      split = "\\|")[[1]]), "\\|"), trimws)
    factor(object$rcrd[, x], levels = lbls[1, ], labels = lbls[2, ])
  }

  object$rcrd[, cols_fct] <- lapply(cols_fct, fct_label)


  # 6 Create list of datasets ---------------------------------------------------
  dat_ed <- vector("list", 2)
  names(dat_ed) <- c("form", "event")

  ## By form (data collection instrument)
  ### All forms
  forms <- unique(object$dd$Form.Name)
  ### Repeating forms
  forms_rpt <- unique(object$rcrd$redcap_repeat_instrument[!is.na(
    object$rcrd$redcap_repeat_instrument
  )])

  ## Form completion
  cols_cmp <- paste0(forms, "_complete")
  object$rcrd[, cols_cmp] <- lapply(object$rcrd[, cols_cmp], function(x)
    factor(x, levels = c(0,1,2), labels = c("Incomplete", "Unverified", "Complete")))

    ## Create list with name, columns, events, and repeating status by form
  form_data <- sapply(forms, function(form) {

    vars  <- object$dd[[1]][object$dd$Form.Name %in% form]
    cols  <- unlist(sapply(vars, function(x)
      grep(
        paste(paste0("^", x, c("$", "___")), collapse = "|")
        , names(object$rcrd), value = TRUE
      )))
    cols  <- c(cols, paste0(form, "_complete"))

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
    } else {
      data  <- object$rcrd[
        is.na(object$rcrd$redcap_repeat_instrument)
        , unique(c(ids_rc, form$cols))]
    }

    if("redcap_event_name" %in% names(object$rcrd))
      data <- data[data$redcap_event_name %in% form$events,]

    if(label != FALSE) {
      labs <- sapply(names(data), function(x) {
        dats_raw$dd$Field.Label[dats_raw$dd[[1]] %in% gsub("___[0-9]+$","",x)]
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

    events <- object$evnt$unique_event_name

    dat_ed$event <- sapply(events, function(event) {

      ## Approach 2: Identify required forms
      forms  <- form_data[sapply(form_data, function(form) event %in% form$events)]

      ### Separate by whether they are repeating or not.
      cols       <- sapply(forms, "[[", "cols")
      cols_rpt   <- sapply(forms[ sapply(forms, "[[", "repeating")], "[[", "cols")
      cols_norpt <- sapply(forms[!sapply(forms, "[[", "repeating")], "[[", "cols")


      if(repeated == "include") {

        data <- object$rcrd[
          object$rcrd$redcap_event_name == event
          , unique(c(ids_rc, unlist(cols)))]

      } else {

        data <- object$rcrd[
          object$rcrd$redcap_event_name == event &
            is.na(object$rcrd$redcap_repeat_instrument)
          , unique(c(ids, unlist(cols_norpt)))]

        if(repeated == "nest") {

          dats_rpt <- sapply(names(cols_rpt), function(x) {

            dat_t1 <- object$rcrd[
              object$rcrd$redcap_event_name == event &
                object$rcrd$redcap_repeat_instrument %in% x
              , unique(c(ids, "redcap_repeat_instance", unlist(cols_rpt[x])))]

            tidyr::nest(dat_t1, !! x := -ids)

          }, simplify = FALSE)


          data <- Reduce(function(...) merge(..., all = TRUE, by = ids)
                         , c(norpt = list(data), dats_rpt))

        }

      }

      if(label != FALSE) {
        labs <- sapply(names(data), function(x) {
          dats_raw$dd$Field.Label[dats_raw$dd[[1]] %in% gsub("___[0-9]+$","",x)]
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
