# =====================================================================
# SerialNumber - Export tab: dynamic filter -> SQL WHERE -> grid + Excel
#   Helpers below are prefixed SerialNumberExport_ so that they do not
#   clash with the original SerialNumber.R files.
# =====================================================================

"%||%" <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

safe_date <- function(x) {
  d <- suppressWarnings(as.Date(x))
  if (is.na(d)) Sys.Date() else d
}

#' Resolve an ERP token.
#'   Order: dbConfig(app_id, run_env) -> erp_token visible in the module env.
#' @export
SerialNumberExport_token <- function(app_id, run_env, caller_env = NULL) {
  app <- tryCatch(app_id, error = function(e) NULL)
  if (is.null(app) || is.logical(app)) {
    app <- tryCatch(get0("app_id", envir = caller_env, inherits = TRUE), error = function(e) NULL)
  }
  env_name <- SerialNumberExport_run_env(run_env)
  tok <- NULL
  if (!is.null(app)) {
    tok <- tryCatch(rdbepkg::dbConfig(FAppId = app, FType = "ERP", FRunEnv = env_name),
                    error = function(e) NULL)
  }
  if (!is.null(tok) && !is.logical(tok)) return(tok)

  tok2 <- tryCatch(get0("erp_token", envir = caller_env, inherits = TRUE), error = function(e) NULL)
  if (!is.null(tok2) && !is.logical(tok2)) return(tok2)

  stop(paste0("Cannot obtain an ERP token: dbConfig(app_id=", app, ", FRunEnv=", env_name,
              ") did not return a usable token and no erp_token was found."), call. = FALSE)
}

#' Field list (label / expr / where_expr / type)
#' @export
#' Resolve the run environment safely.
#'   If run_env was not passed (or is not visible at the call site) we fall
#'   back to PRD and log a message instead of failing the whole tab.
#' @export
SerialNumberExport_run_env <- function(x) {
  v <- tryCatch(x, error = function(e) NULL)
  ok <- !is.null(v) && length(v) && !is.na(v) && nzchar(as.character(v))
  if (!ok) {
    message("[SerialNumberExport] run_env is not available at the call site, fallback to PRD")
    return("PRD")
  }
  as.character(v)
}

SerialNumberExport_field_meta <- function() {
  mdlVmSerialNumberPkg::SerialNumberExport_fields()
}

#' Operators by field type
#' @export
SerialNumberExport_ops <- function(type) {
  if (identical(type, "number")) {
    c("Equals" = "=", "Not equal" = "<>", "Greater than" = ">", "Greater or equal" = ">=",
      "Less than" = "<", "Less or equal" = "<=", "Between" = "BETWEEN",
      "In list" = "IN", "Is empty" = "IS NULL", "Is not empty" = "IS NOT NULL")
  } else if (identical(type, "date")) {
    c("Equals" = "=", "Not equal" = "<>", "Before" = "<", "After" = ">",
      "Between" = "BETWEEN", "Today" = "TODAY", "This month" = "THIS_MONTH",
      "This year" = "THIS_YEAR", "Last 7 days" = "LAST7", "Last 30 days" = "LAST30",
      "Is empty" = "IS NULL", "Is not empty" = "IS NOT NULL")
  } else {
    c("Equals" = "=", "Not equal" = "<>", "Contains" = "CONTAINS",
      "Not contains" = "NOT_CONTAINS", "Starts with" = "STARTS", "Ends with" = "ENDS",
      "In list" = "IN", "Is empty" = "IS NULL", "Is not empty" = "IS NOT NULL")
  }
}

SerialNumberExport_meta <- function(label) {
  m <- SerialNumberExport_field_meta()
  i <- match(label, m$label)
  if (is.na(i)) NULL else m[i, , drop = FALSE]
}

SerialNumberExport_no_value <- function() {
  c("IS NULL", "IS NOT NULL", "TODAY", "THIS_MONTH", "THIS_YEAR", "LAST7", "LAST30")
}

SerialNumberExport_blank <- function(x) {
  is.null(x) || length(x) == 0 || is.na(x) || !nzchar(trimws(as.character(x)))
}

SerialNumberExport_split <- function(x) {
  p <- trimws(unlist(strsplit(as.character(x), "[,，;；]")))
  p[nzchar(p)]
}

SerialNumberExport_str <- function(x) {
  mdlVmSerialNumberPkg::SerialNumberExport_sql_string(x)
}

#' One condition -> T-SQL predicate
#' @export
SerialNumberExport_cond <- function(meta, op, v1, v2 = "") {
  expr <- meta$where_expr
  if (is.null(expr) || length(expr) == 0 || is.na(expr)) expr <- meta$expr
  type <- meta$type

  coerce <- function(v) {
    if (identical(type, "number")) {
      x <- suppressWarnings(as.numeric(gsub(",", "", v, fixed = TRUE)))
      if (is.na(x)) stop(sprintf("'%s' is not a valid number", v), call. = FALSE)
      x
    } else if (identical(type, "date")) {
      x <- suppressWarnings(as.Date(v))
      if (is.na(x)) stop(sprintf("'%s' is not a valid date", v), call. = FALSE)
      format(x, "%Y-%m-%d")
    } else {
      as.character(v)
    }
  }

  lit <- function(v) {
    if (identical(type, "number")) format(v, scientific = FALSE, trim = TRUE)
    else SerialNumberExport_str(v)
  }

  if (identical(op, "IS NULL")) return(paste(expr, "is null"))
  if (identical(op, "IS NOT NULL")) return(paste(expr, "is not null"))
  if (identical(op, "TODAY")) return(paste0(expr, " = cast(getdate() as date)"))
  if (identical(op, "THIS_MONTH")) {
    return(paste0("(year(", expr, ") = year(getdate()) and month(", expr,
                  ") = month(getdate()))"))
  }
  if (identical(op, "THIS_YEAR")) return(paste0("year(", expr, ") = year(getdate())"))
  if (identical(op, "LAST7") || identical(op, "LAST30")) {
    n <- if (identical(op, "LAST7")) 6L else 29L
    return(paste0(expr, " between dateadd(day, -", n,
                  ", cast(getdate() as date)) and cast(getdate() as date)"))
  }

  if (identical(op, "IN")) {
    vals <- SerialNumberExport_split(v1)
    if (!length(vals)) stop("the list is empty", call. = FALSE)
    vals <- lapply(vals, coerce)
    return(paste0(expr, " in (", paste(vapply(vals, lit, ""), collapse = ", "), ")"))
  }

  if (identical(op, "BETWEEN")) {
    return(paste0(expr, " between ", lit(coerce(v1)), " and ", lit(coerce(v2))))
  }
  if (identical(op, "CONTAINS")) {
    return(paste0("charindex(", SerialNumberExport_str(v1), ", ", expr, ") > 0"))
  }
  if (identical(op, "NOT_CONTAINS")) {
    return(paste0("charindex(", SerialNumberExport_str(v1), ", ", expr, ") = 0"))
  }
  if (identical(op, "STARTS")) {
    v <- SerialNumberExport_str(v1)
    return(paste0("left(", expr, ", len(", v, ")) = ", v))
  }
  if (identical(op, "ENDS")) {
    v <- SerialNumberExport_str(v1)
    return(paste0("right(", expr, ", len(", v, ")) = ", v))
  }
  if (!op %in% c("=", "<>", "<", ">", "<=", ">=")) {
    stop(sprintf("unknown operator: %s", op), call. = FALSE)
  }
  paste0(expr, " ", op, " ", lit(coerce(v1)))
}

#' Rows -> WHERE clause
#' @export
SerialNumberExport_build <- function(rows) {
  if (length(rows) == 0) {
    return(list(where = "", used = integer(0), skipped = integer(0), error = NULL))
  }
  used <- integer(0)
  skipped <- integer(0)
  expr <- NULL
  prev_logic <- "AND"
  balance <- 0L

  for (i in seq_along(rows)) {
    r <- rows[[i]]
    meta <- SerialNumberExport_meta(r$field %||% "")
    if (is.null(meta)) {
      skipped <- c(skipped, i)
      next
    }
    ops <- SerialNumberExport_ops(meta$type)
    op <- r$op %||% ""
    if (!op %in% unname(ops)) {
      return(list(where = "", used = used, skipped = skipped,
                  error = sprintf("Row %d: operator does not match field '%s'", i, meta$label)))
    }
    v1 <- if (is.null(r$value)) "" else trimws(as.character(r$value))
    v2 <- if (is.null(r$value2)) "" else trimws(as.character(r$value2))
    no_value <- op %in% SerialNumberExport_no_value()

    if (identical(op, "BETWEEN") && (SerialNumberExport_blank(v1) || SerialNumberExport_blank(v2))) {
      skipped <- c(skipped, i)
      next
    }
    if (!no_value && !identical(op, "BETWEEN") && SerialNumberExport_blank(v1)) {
      skipped <- c(skipped, i)
      next
    }

    built <- tryCatch(SerialNumberExport_cond(meta, op, v1, v2), error = function(e) e)
    if (inherits(built, "error")) {
      return(list(where = "", used = used, skipped = skipped,
                  error = sprintf("Row %d (%s): %s", i, meta$label, conditionMessage(built))))
    }
    lp <- r$lparen %||% ""
    rp <- r$rparen %||% ""
    piece <- paste0(lp, built, rp)
    expr <- if (is.null(expr)) piece else paste(expr, prev_logic, piece)
    prev_logic <- if (identical(r$logic, "OR")) "OR" else "AND"
    balance <- balance + nchar(lp) - nchar(rp)
    used <- c(used, i)
  }

  if (!is.null(expr) && balance != 0L) {
    return(list(where = "", used = used, skipped = skipped,
                error = "Unmatched parentheses: check the bracket columns"))
  }
  list(where = expr %||% "", used = used, skipped = skipped, error = NULL)
}
#' Export tab server
#'
#' @param input input
#' @param output output
#' @param session session
#' @param app_id app id
#' @param run_env runtime env
#'
#' @return none
#' @export
#'
#' @examples
#' SerialNumberExportServer()
SerialNumberExportServer <- function(input, output, session, app_id, run_env = "PRD") {

  # caller environment, used to look for erp_token when dbConfig cannot build one
  token_env <- parent.frame()

  all_columns <- SerialNumberExport_field_meta()$label
  default_columns <- c("Serial Number", "PN", "Product Name", "Sales OrderID", "Delivery Location", "Production Date")
  reset_columns <- c("Serial Number", "PN", "Product Name")

  # ---------------- column setting ----------------
  observeEvent(input$btn_SerialNumberExport_select_all, {
    updatePickerInput(session = session,
                      inputId = "SerialNumberExport_column_selector",
                      selected = all_columns)
  })
  observeEvent(input$btn_SerialNumberExport_deselect_all, {
    updatePickerInput(session = session,
                      inputId = "SerialNumberExport_column_selector",
                      selected = reset_columns)
  })
  observeEvent(input$btn_SerialNumberExport_defaultValue, {
    updatePickerInput(session = session,
                      inputId = "SerialNumberExport_column_selector",
                      selected = default_columns)
  })

  output$SerialNumberExport_selection_info <- renderPrint({
    selected <- input$SerialNumberExport_column_selector
    cat("Column Count: ", length(selected), "\n")
    cat("Column List: ", paste(selected, collapse = ", "), "\n")
  })

  # ---------------- filter rows ----------------
  filter_fields <- SerialNumberExport_field_meta()
  row_ids <- shiny::reactiveVal(character(0))
  row_seq <- shiny::reactiveVal(0L)
  active_row <- shiny::reactiveVal(NULL)

  new_row_id <- function() {
    i <- row_seq() + 1L
    row_seq(i)
    sprintf("sne_%03d", i)
  }

  row_ui <- function(id, initial = list()) {
    f0 <- initial$field %||% filter_fields$label[1]
    if (!f0 %in% filter_fields$label) f0 <- filter_fields$label[1]
    lp0 <- initial$lparen %||% ""
    if (!lp0 %in% c("", "(", "((")) lp0 <- ""
    rp0 <- initial$rparen %||% ""
    if (!rp0 %in% c("", ")", "))")) rp0 <- ""
    lg0 <- initial$logic %||% "AND"
    if (!lg0 %in% c("AND", "OR")) lg0 <- "AND"
    onclick <- sprintf("Shiny.setInputValue('SerialNumberExport_filter_active','%s',{priority:'event'});document.querySelectorAll('.dlf-row').forEach(function(e){e.classList.remove('is-active');});this.classList.add('is-active');", id)
    shiny::div(
      id = id, class = "dlf-row", onclick = onclick,
      shiny::selectInput(paste0(id, "__lp"), NULL,
                         choices = c("-" = "", "(" = "(", "((" = "(("),
                         selected = lp0, selectize = FALSE, width = "70px"),
      shiny::selectInput(paste0(id, "__field"), NULL,
                         choices = filter_fields$label,
                         selected = f0, selectize = FALSE, width = "162px"),
      shiny::uiOutput(paste0(id, "__op_ui")),
      shiny::uiOutput(paste0(id, "__val_ui")),
      shiny::selectInput(paste0(id, "__rp"), NULL,
                         choices = c("-" = "", ")" = ")", "))" = "))"),
                         selected = rp0, selectize = FALSE, width = "70px"),
      shiny::selectInput(paste0(id, "__logic"), NULL,
                         choices = c("AND" = "AND", "OR" = "OR"),
                         selected = lg0, selectize = FALSE, width = "100px"),
      shiny::actionButton(paste0(id, "__del"), "", icon = shiny::icon("minus"))
    )
  }

  register_row <- function(id, initial = list()) {
    f0 <- initial$field %||% filter_fields$label[1]
    if (!f0 %in% filter_fields$label) f0 <- filter_fields$label[1]

    output[[paste0(id, "__op_ui")]] <- shiny::renderUI({
      f <- input[[paste0(id, "__field")]]
      if (is.null(f)) f <- f0
      m <- SerialNumberExport_meta(f)
      ops <- SerialNumberExport_ops(m$type)
      cur <- input[[paste0(id, "__op")]]
      if (is.null(cur) || !cur %in% unname(ops)) cur <- unname(ops)[1]
      shiny::selectInput(paste0(id, "__op"), NULL, choices = ops,
                         selected = cur, selectize = FALSE, width = "130px")
    })

    output[[paste0(id, "__val_ui")]] <- shiny::renderUI({
      f <- input[[paste0(id, "__field")]]
      if (is.null(f)) f <- f0
      m <- SerialNumberExport_meta(f)
      op <- input[[paste0(id, "__op")]]
      if (is.null(op)) op <- unname(SerialNumberExport_ops(m$type))[1]

      if (op %in% SerialNumberExport_no_value()) {
        return(shiny::div(style = "height:30px;line-height:30px;color:#9aaab5;font-size:12px;",
                          "No value needed"))
      }

      v1 <- shiny::isolate(input[[paste0(id, "__val")]])
      if (is.null(v1)) v1 <- initial$value %||% ""
      v2 <- shiny::isolate(input[[paste0(id, "__val2")]])
      if (is.null(v2)) v2 <- initial$value2 %||% ""

      if (identical(op, "BETWEEN")) {
        if (identical(m$type, "date")) {
          return(shiny::div(style = "display:flex;gap:4px;align-items:center;",
                            shiny::dateInput(paste0(id, "__val"), NULL, value = safe_date(v1), width = "130px"),
                            shiny::span("to"),
                            shiny::dateInput(paste0(id, "__val2"), NULL, value = safe_date(v2), width = "130px")))
        }
        return(shiny::div(style = "display:flex;gap:4px;align-items:center;",
                          shiny::textInput(paste0(id, "__val"), NULL, value = v1, placeholder = "From", width = "110px"),
                          shiny::span("to"),
                          shiny::textInput(paste0(id, "__val2"), NULL, value = v2, placeholder = "To", width = "110px")))
      }

      if (identical(m$type, "date")) {
        return(shiny::dateInput(paste0(id, "__val"), NULL, value = safe_date(v1), width = "150px"))
      }
      if (identical(m$type, "number")) {
        return(shiny::textInput(paste0(id, "__val"), NULL, value = v1,
                                placeholder = "Enter a number", width = "140px"))
      }
      shiny::textInput(paste0(id, "__val"), NULL, value = v1,
                       placeholder = "Enter a value", width = "150px")
    })
  }

  add_row <- function(initial = NULL, after = NULL) {
    initial <- initial %||% list()
    id <- new_row_id()
    ids <- row_ids()
    if (is.null(after) || !after %in% ids) {
      shiny::insertUI("#SerialNumberExport_filter_rows", where = "beforeEnd", ui = row_ui(id, initial))
      row_ids(c(ids, id))
    } else {
      shiny::insertUI(paste0("#", after), where = "afterEnd", ui = row_ui(id, initial))
      row_ids(append(ids, id, after = match(after, ids)))
    }
    register_row(id, initial)
    shiny::observeEvent(input[[paste0(id, "__del")]], {
      remove_row(id)
    }, ignoreInit = TRUE, once = TRUE)
    active_row(id)
    invisible(id)
  }

  remove_row <- function(id) {
    if (!id %in% row_ids()) return(invisible(NULL))
    shiny::removeUI(paste0("#", id), immediate = TRUE)
    row_ids(setdiff(row_ids(), id))
    if (identical(active_row(), id)) {
      ids <- row_ids()
      active_row(if (length(ids)) utils::tail(ids, 1) else NULL)
    }
    invisible(NULL)
  }

  clear_rows <- function() {
    for (id in row_ids()) shiny::removeUI(paste0("#", id), immediate = TRUE)
    row_ids(character(0))
    active_row(NULL)
    invisible(NULL)
  }

  current_rows <- function() {
    lapply(row_ids(), function(id) {
      list(
        field = input[[paste0(id, "__field")]],
        op = input[[paste0(id, "__op")]],
        value = input[[paste0(id, "__val")]],
        value2 = input[[paste0(id, "__val2")]],
        lparen = input[[paste0(id, "__lp")]],
        rparen = input[[paste0(id, "__rp")]],
        logic = input[[paste0(id, "__logic")]]
      )
    })
  }

  selected_columns <- function() {
    cols <- input$SerialNumberExport_column_selector
    if (is.null(cols) || !length(cols)) cols <- default_columns
    intersect(cols, all_columns)
  }

  shiny::observeEvent(input$SerialNumberExport_filter_active, {
    if (input$SerialNumberExport_filter_active %in% row_ids()) {
      active_row(input$SerialNumberExport_filter_active)
    }
  }, ignoreNULL = TRUE)

  shiny::observeEvent(input$btn_SerialNumberExport_filter_add, {
    add_row()
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$btn_SerialNumberExport_filter_del, {
    ids <- row_ids()
    id <- active_row()
    if (is.null(id) || !id %in% ids) id <- if (length(ids)) utils::tail(ids, 1) else NULL
    if (!is.null(id)) remove_row(id)
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$btn_SerialNumberExport_filter_clear, {
    clear_rows()
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$btn_SerialNumberExport_filter_insert, {
    ids <- row_ids()
    after <- active_row()
    if (is.null(after) || !after %in% ids) after <- if (length(ids)) utils::tail(ids, 1) else NULL
    add_row(after = after)
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$btn_SerialNumberExport_filter_copy, {
    ids <- row_ids()
    id <- active_row()
    if (is.null(id) || !id %in% ids) id <- if (length(ids)) utils::tail(ids, 1) else NULL
    if (is.null(id)) {
      add_row()
    } else {
      add_row(initial = current_rows()[[match(id, ids)]], after = id)
    }
  }, ignoreInit = TRUE)

  session$onFlushed(function() {
    shiny::isolate(add_row())
  }, once = TRUE)

  # ---------------- Search: build the WHERE and run it in the database ----------------
  shiny::observeEvent(input$btn_SerialNumberExport_view, {
    b <- SerialNumberExport_build(current_rows())
    if (!is.null(b$error)) {
      tsui::pop_notice(b$error)
      return(invisible(NULL))
    }
    if (length(b$skipped)) {
      tsui::pop_notice(sprintf("%d incomplete row(s) ignored", length(b$skipped)))
    }

    erp_token <- SerialNumberExport_token(app_id = app_id, run_env = run_env, caller_env = token_env)
    data <- tryCatch(
      mdlVmSerialNumberPkg::SerialNumberExport_select(
        erp_token = erp_token,
        FWhere = b$where,
        cols = selected_columns()
      ),
      error = function(e) e
    )
    if (inherits(data, "error")) {
      tsui::pop_notice(paste0("Query failed: ", conditionMessage(data)))
      return(invisible(NULL))
    }
    if (is.null(data) || !nrow(data)) {
      tsui::pop_notice("No data matches the current filter")
    }
    tsui::run_dataTable2(id = 'SerialNumberExport_resultView', data = data, lang = 'en')
    tsui::run_download_xlsx(id = 'dl_SerialNumberExport', data = data, filename = 'SerialNumberExport.xlsx')
  }, ignoreInit = TRUE)

}
