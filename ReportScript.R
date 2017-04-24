## ----LoadingPackages---------------------------------------
pkgs <- c("data.table", "jsonlite", "readr", "pander", "tables", "dplyr")
for (pkg in pkgs) require(pkg, character.only = TRUE, quietly = T, warn.conflicts = F)

##----VariousFunctions------------------------------
getWorkData <- function(json.file){
  work_list <- jsonlite::fromJSON(json.file)
  work_hr <- rbindlist(lapply(lapply(work_list, function(x) x[[3]]), as.data.table),
                       idcol = T, fill = TRUE)
  work_from_to <- rbindlist(lapply(lapply(work_list, function(x) x[1:2]), as.data.table),
                            idcol = T, fill = TRUE)
  setnames(work_hr, ".id", "WorkPlace")
  setnames(work_from_to, ".id", "WorkPlace")
  work_hr[is.na(work_hr)] <- 0
  return(list(work_hr = work_hr, work_from_to = work_from_to))
}

generate_wtbl <- function(start_date, end_date, day_time_list){
  names(day_time_list) <- tolower(names(day_time_list))
  date_seq <- seq(as.Date(start_date, "%Y-%m-%d"), 
                  as.Date(end_date, "%Y-%m-%d"), 
                  by = "days")
  wdays_idx <- which(tolower(weekdays(date_seq)) %in% names(day_time_list))
  wdates <- date_seq[wdays_idx]
  wtimes <- sapply(tolower(weekdays(wdates)), function(x) day_time_list[[x]])
  return(data.frame(Date = wdates, Time = wtimes))
}

addDates <- function(work_table, date.vec, time) {
  setDT(tibble::tibble(
    WorkPlace = "Company Book",
    Date = as.Date(date.vec),
    Time = time))
}

getWorkTable <- function(json.file, month = NULL, extra_dates = NULL, time = NULL) {
  work_data <- getWorkData(json.file)
  work_hr <- work_data$work_hr
  work_from_to <- work_data$work_from_to
  workplace <- `names<-`(work_hr$WorkPlace, work_hr$WorkPlace)
  work_table <- purrr::map_df(workplace, 
                              ~generate_wtbl(work_from_to[WorkPlace == .x, start_date],
                                             work_from_to[WorkPlace == .x, end_date], 
                                             work_hr[WorkPlace == .x, -"WorkPlace"]), 
                              .id = "WorkPlace")
  work_table <- setDT(work_table)
  work_table <- work_table[Time != 0]
  if (!is.null(extra_dates) & !is.null(time)) 
    work_table <- rbind(work_table, addDates(work_table, extra_dates, time))
  work_table[, c("Day", "Month") := .(weekdays(Date), month.name[as.numeric(month(Date))])]
  if (!is.null(month)) work_table <- work_table[Month %in% month]
  work_table[, c("WorkPlace", "Day") := lapply(.SD, as.factor), .SDcols = c("WorkPlace", "Day")]
  work_table[, Month := factor(Month, levels = month.name)]
  return(work_table)
}


makeTable <- function(work_table) {
  tab <- tabular(
    (Month) * (WorkPlace + (All = 1)) * DropEmpty("-") + Heading("Grand Total") * 1 ~  
      Heading() * (Day + Heading("All") * 1) * Heading() * Format(digits = 3) * Time * Heading() * sum, 
    data = work_table)
  
  allRow <- which(attr(tab, "rowLabels")[, "WorkPlace"] %in% c("All"))
  grandTotalRow <- which(attr(tab, "rowLabels")[, "WorkPlace"] %in% c("Grand Total"))
  allCol <- which(attr(tab, "colLabels") %in% c("All")) + ncol(attr(tab, "rowLabels"))
  return(list(tab = tab, allRow = allRow, 
              grandTotalRow = grandTotalRow, 
              allCol = allCol))
}

get_table_tax <- function(amount, year) {
  api_url <- paste0('https://api-tabellkort.app.skatteetaten.no/?valgtTabell=7100&valgtInntektType=Lonn&valgtPeriode=PERIODE_1_MAANED&valgtLonn=',round(amount),'&visHeleTabellen=true&valgtAar=',year)
  return(fromJSON(api_url))
}

printTable <- function(tab_obj) {
  tab <- tab_obj$tab
  allRow <- tab_obj$allRow
  allCol <- tab_obj$allCol
  grandTotalRow <- tab_obj$grandTotalRow
  alignment <- paste("ll", paste(rep("r", ncol(tab)), collapse = ""), collapse = "", sep = "")
  pander(tab, justify = alignment, style = "rmarkdown", split.tables = Inf,
         emphasize.verbatim.cols = 3:(ncol(tab) + 2), emphasize.rownames = FALSE,
         emphasize.italics.rows = allRow, emphasize.italics.cols = allCol,
         emphasize.strong.rows = grandTotalRow, emphasize.strong.cols = 1)
}
