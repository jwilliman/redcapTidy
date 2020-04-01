make_labels <- function(vars, dd) {
  do.call(
    rbind,
    lapply(vars, function(x) {
      checkbox <- dd$Field.Type[dd[[1]] == x] == "checkbox"
      if(checkbox) {
        mat <- sapply(strsplit(sub(",", "|", strsplit(
          dd$Choices..Calculations..OR.Slider.Labels[dd[[1]] == x],
          split = "\\|")[[1]]), "\\|"), trimws)
        lab <- data.frame(
          col = paste(x, mat[1,], sep = "___"),
          label = paste(
            dd$Field.Label[dd[[1]] == x],
            mat[2,], sep = ":")
          , stringsAsFactors = FALSE)
      } else {
        lab <- data.frame(
          col = x, label = dd$Field.Label[dd[[1]] == x]
          , stringsAsFactors = FALSE)
      }
      return(lab)
    }))
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
#' @param ids Names of identifiers, for inclusion on all output datasets.
#'
#' @return
#' @export
#'
#' @examples
import_rc_csv <- function(folder) {


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

  ## Import data
  object <- lapply(file.path(folder, Inputs), read.csv, stringsAsFactors = FALSE)
  names(object) <- names(Inputs)

  return(object)

}

#' Tidy list of datasets from REDCap.
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
tidy_redcap <- function(object, ids = NULL) {

  if(is.null(ids))
    ids <- c(names(object$rcrd)[[1]], grep("redcap",names(object$rcrd), value = TRUE, ignore.case = TRUE))

  # 5 Format and clean variables ---------------------------------------------------------
  ## Character fields (replace blanks with NA)
  v.chr <- names(object$rcrd)[sapply(object$rcrd, is.character)]
  for (x in v.chr)
    object$rcrd[, x][object$rcrd[, x] == ""] <- NA


  ## Dates and date-time variables
  v.dt <- object$dd[[1]][grepl("date_", object$dd$Text.Validation.Type.OR.Show.Slider.Number)]
  object$rcrd[, v.dt] <- lapply(object$rcrd[, v.dt], as.Date, format = "%Y-%m-%d")

  v.dttm <- c(
    object$dd[[1]][grepl("datetime_", object$dd$Text.Validation.Type.OR.Show.Slider.Number)],
    grep("timestamp", names(object$rcrd), value = TRUE))
  object$rcrd[, v.dttm] <- lapply(object$rcrd[, v.dttm], as.POSIXct, format = "%Y-%m-%d %H:%M")


  ## Logical (checkboxs)
  v.lg <- c(
    object$dd[[1]][object$dd$Field.Type == "yesno" |
                 object$dd$Choices..Calculations..OR.Slider.Labels %in% c(
                   "0, Incorrect | 1, Correct", "0, No | 1, Yes", "1, True | 0, False")],
    unlist(sapply(object$dd[[1]][object$dd$Field.Type == "checkbox"], function(x)
      grep(paste0(x, "___"), names(object$rcrd), value = TRUE))),
    grep("complete$", names(object$rcrd), value = TRUE))
  object$rcrd[, v.lg] <- lapply(object$rcrd[, v.lg], as.logical)


  ## Factors (radio)
  v.fct <- object$dd[[1]][object$dd$Field.Type %in% c("radio", "dropdown") & !(object$dd[[1]] %in% v.lg)]

  fct_label <- function(x) {
    ## Replace first ',' with '|' before repeating split.
    lbls <- sapply(strsplit(sub(",", "|", strsplit(
      object$dd$Choices..Calculations..OR.Slider.Labels[object$dd[[1]] == x],
      split = "\\|")[[1]]), "\\|"), trimws)
    factor(object$rcrd[, x], levels = as.integer(lbls[1, ]), labels = lbls[2, ])
  }

  object$rcrd[, v.fct] <- lapply(v.fct, fct_label)


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

  dat_ed$form <- sapply(forms, function(form) {

    vars  <- object$dd[[1]][object$dd$Form.Name %in% form]
    cols  <- unlist(sapply(vars, function(x)
      grep(
        paste(paste0("^", x, c("$", "___")), collapse = "|")
        , names(object$rcrd), value = TRUE
      )))
    events <- object$inst$unique_event_name[object$inst$form == form]

    if(form %in% forms_rpt)
    data  <- object$rcrd[
      object$rcrd$redcap_event_name %in% events &
        object$rcrd$redcap_repeat_instrument %in% form
      , unique(c(ids, cols))]
    else
      data  <- object$rcrd[
        object$rcrd$redcap_event_name %in% events &
          is.na(object$rcrd$redcap_repeat_instrument)
        , unique(c(ids, cols))]

    labs <- make_labels(vars, object$dd)

    Hmisc::label(data, self = FALSE) <- sapply(names(data), function(col)
      ifelse(col %in% labs$col, labs$label[labs$col == col], NA))

    return(data)

  }, simplify = FALSE)



  ## Create list of datasets by event name --------------------------------------
  events <- object$evnt$unique_event_name

  dat_ed$event <- sapply(events, function(event) {

    forms <- object$inst$form[object$inst$unique_event_name == event]
    vars  <- object$dd[[1]][object$dd$Form.Name %in% forms]
    cols  <- unlist(sapply(vars, function(x)
      grep(
        paste(paste0("^", x, c("$", "___")), collapse = "|")
        , names(object$rcrd), value = TRUE
      )))

    data <- object$rcrd[object$rcrd$redcap_event_name == event, unique(c(ids, cols))]

    labs <- make_labels(vars, object$dd)

    Hmisc::label(data, self = FALSE) <- sapply(names(data), function(col)
      ifelse(col %in% labs$col, labs$label[labs$col == col], NA))

    return(data)

  }, simplify = FALSE)


  return(dat_ed)

}
