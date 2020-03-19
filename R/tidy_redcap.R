#' Tidy datasets imported from REDCap.
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
tidy_redcap <- function(folder, ids = c("id", "redcap_event_name")) {


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
  RC <- lapply(file.path(folder, Inputs), read.csv, stringsAsFactors = FALSE)
  names(RC) <- names(Inputs)


  # 5 Format and clean variables ---------------------------------------------------------
  ## Character fields (replace blanks with NA)
  v.chr <- names(RC$rcrd)[sapply(RC$rcrd, is.character)]
  for (x in v.chr)
    RC$rcrd[, x][RC$rcrd[, x] == ""] <- NA


  ## Dates and date-time variables
  v.dt <- RC$dd[[1]][grepl("date_", RC$dd$Text.Validation.Type.OR.Show.Slider.Number)]
  RC$rcrd[, v.dt] <- lapply(RC$rcrd[, v.dt], as.Date, format = "%Y-%m-%d")

  v.dttm <- c(
    RC$dd[[1]][grepl("datetime_", RC$dd$Text.Validation.Type.OR.Show.Slider.Number)],
    grep("timestamp", names(RC$rcrd), value = TRUE))
  RC$rcrd[, v.dttm] <- lapply(RC$rcrd[, v.dttm], as.POSIXct, format = "%Y-%m-%d %H:%M")


  ## Logical (checkboxs)
  v.lg <- c(
    RC$dd[[1]][RC$dd$Field.Type == "yesno" |
                 RC$dd$Choices..Calculations..OR.Slider.Labels %in% c(
                   "0, Incorrect | 1, Correct", "0, No | 1, Yes", "1, True | 0, False")],
    unlist(sapply(RC$dd[[1]][RC$dd$Field.Type == "checkbox"], function(x)
      grep(paste0(x, "___"), names(RC$rcrd), value = TRUE))),
    grep("complete$", names(RC$rcrd), value = TRUE))
  RC$rcrd[, v.lg] <- lapply(RC$rcrd[, v.lg], as.logical)


  ## Factors (radio)
  v.fct <- RC$dd[[1]][RC$dd$Field.Type %in% c("radio", "dropdown") & !(RC$dd[[1]] %in% v.lg)]

  fct_label <- function(x) {
    ## Replace first ',' with '|' before repeating split.
    lbls <- sapply(strsplit(sub(",", "|", strsplit(
      RC$dd$Choices..Calculations..OR.Slider.Labels[RC$dd[[1]] == x],
      split = "\\|")[[1]]), "\\|"), trimws)
    factor(RC$rcrd[, x], levels = as.integer(lbls[1, ]), labels = lbls[2, ])
  }

  RC$rcrd[, v.fct] <- lapply(v.fct, fct_label)


  # 6 Create list of datasets ---------------------------------------------------
  dat_ed <- vector("list", 2)
  names(dat_ed) <- c("form", "event")

  ## By form (data collection instrument)
  forms <- unique(RC$dd$Form.Name)

  dat_ed$form <- sapply(forms, function(form) {

    vars  <- RC$dd[[1]][RC$dd$Form.Name %in% form]
    cols  <- unlist(sapply(vars, function(x)
      grep(
        paste(paste0("^", x, c("$", "___")), collapse = "|")
        , names(RC$rcrd), value = TRUE
      )))
    events <- RC$inst$unique_event_name[RC$inst$form == form]

    data  <- RC$rcrd[RC$rcrd$redcap_event_name %in% events, unique(c(ids, cols))]

    return(data)

  }, simplify = FALSE)


  ## Create list of datasets by event name --------------------------------------
  events <- RC$evnt$unique_event_name

  dat_ed$event <- sapply(events, function(event) {

    forms <- RC$inst$form[RC$inst$unique_event_name == event]
    vars  <- RC$dd[[1]][RC$dd$Form.Name %in% forms]
    cols  <- unlist(sapply(vars, function(x)
      grep(
        paste(paste0("^", x, c("$", "___")), collapse = "|")
        , names(RC$rcrd), value = TRUE
      )))

    data <- RC$rcrd[RC$rcrd$redcap_event_name == event, unique(c(ids, cols))]

    return(data)

  }, simplify = FALSE)


  return(dat_ed)

}
