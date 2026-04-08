# ==============================================================================
# 1. PACKAGES
# ==============================================================================

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(xml2)
  library(dplyr)
  library(purrr)
  library(stringr)
  library(tidyr)
  library(openxlsx)
  library(glue)
  library(tibble)
  library(shiny)
  library(shinythemes)
  library(shinyjs)
})

# ==============================================================================
# 2. HELPERS
# ==============================================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

sanitize_colname <- function(x) {
  x %>%
    tolower() %>%
    str_replace_all("[^a-z0-9]", "_")
}

# ==============================================================================
# 3. CONFIG
# ==============================================================================

PORTKEY_URL <- "https://api.portkey.ai/v1/chat/completions"
DEFAULT_MODEL <- "claude-sonnet-4-5-20250929"

# ==============================================================================
# 4. AI REQUEST
# ==============================================================================

openai_request <- function(messages, model = DEFAULT_MODEL) {
  api_key <- Sys.getenv("PORTKEY_API_KEY")
  if (identical(api_key, "")) stop("PORTKEY_API_KEY not set")

  body <- list(
    model = model,
    messages = messages,
    temperature = 0
  )

  res <- POST(
    url = PORTKEY_URL,
    add_headers(
      "x-portkey-api-key" = api_key,
      "x-portkey-provider" = "anthropic",
      "Content-Type" = "application/json"
    ),
    body = toJSON(body, auto_unbox = TRUE, null = "null"),
    encode = "raw",
    timeout(120)
  )

  stop_for_status(res)

  raw_txt <- content(res, "text", encoding = "UTF-8")
  out <- fromJSON(raw_txt, simplifyVector = FALSE)

  if (!is.null(out$error)) {
    stop(out$error$message %||% "API returned an unknown error")
  }

  if (is.null(out$choices) || length(out$choices) == 0) {
    stop("No response choices returned from API")
  }

  out$choices[[1]]$message$content
}

ask_json <- function(prompt, model = DEFAULT_MODEL) {

  txt <- openai_request(
    list(list(role = "user", content = paste(prompt, "\nReturn ONLY valid JSON. No explanation."))),
    model = model
  )

  # 🔥 JSON extract (çok kritik)
  json_txt <- stringr::str_extract(txt, "\\{.*\\}")

  if (is.na(json_txt)) {
    return(list(level = "RED", reason = "no_json_returned", n = 0))
  }

  tryCatch(
    fromJSON(json_txt, simplifyVector = FALSE),
    error = function(e) {
      list(level = "RED", reason = "parse_error", n = 0)
    }
  )
}

# ==============================================================================
# 5. PUBMED
# ==============================================================================

search_pubmed <- function(query, retmax = 50, mindate = NULL, maxdate = NULL) {
  url <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi"

  q <- list(
    db = "pubmed",
    term = query,
    retmode = "json",
    retmax = retmax
  )

  if (!is.null(mindate) && !is.na(mindate)) q$mindate <- as.character(mindate)
  if (!is.null(maxdate) && !is.na(maxdate)) q$maxdate <- as.character(maxdate)
  if (!is.null(mindate) || !is.null(maxdate)) q$datetype <- "pdat"

  res <- GET(url, query = q, timeout(60))
  stop_for_status(res)

  out <- fromJSON(content(res, "text", encoding = "UTF-8"), simplifyVector = FALSE)
  out$esearchresult$idlist %||% character(0)
}

fetch_pubmed <- function(ids) {
  if (length(ids) == 0) return(tibble())

  url <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi"

  res <- GET(
    url,
    query = list(
      db = "pubmed",
      id = paste(ids, collapse = ","),
      retmode = "xml"
    ),
    timeout(120)
  )
  stop_for_status(res)

  doc <- read_xml(content(res, "text", encoding = "UTF-8"))
  arts <- xml_find_all(doc, ".//PubmedArticle")

  map_dfr(arts, function(a) {
    tibble(
      pmid = xml_text(xml_find_first(a, ".//PMID")),
      title = xml_text(xml_find_first(a, ".//ArticleTitle")),
      abstract = paste(xml_text(xml_find_all(a, ".//AbstractText")), collapse = " "),
      journal = xml_text(xml_find_first(a, ".//Journal/Title")),
      year = str_extract(
        xml_text(xml_find_first(a, ".//PubDate")),
        "\\d{4}"
      )
    )
  }) %>%
    mutate(
      title = ifelse(is.na(title), "", title),
      abstract = ifelse(is.na(abstract), "", abstract)
    ) %>%
    filter(abstract != "")
}

# ==============================================================================
# 6. TRIAGE
# ==============================================================================

triage_one <- function(title, abstract, question, model = DEFAULT_MODEL) {
  prompt <- paste0(
    "You are a cardiology literature reviewer.\n\n",
    "Research Question: ", question, "\n\n",
    "Title: ", title, "\n\n",
    "Abstract: ", abstract, "\n\n",
    "Tasks:\n",
    "1. Classify relevance:\n",
    "   - GREEN = directly relevant clinical evidence\n",
    "   - YELLOW = partially relevant or indirectly informative\n",
    "   - RED = irrelevant\n",
    "2. Extract study sample size if available.\n",
    "3. Give a short reason.\n\n",
    "Return JSON only in this exact structure:\n",
    "{\"level\":\"GREEN or YELLOW or RED\",\"reason\":\"short text\",\"n\":0}"
  )

  res <- ask_json(prompt, model = model)

  list(
    relevance = res$level %||% "RED",
    reason = res$reason %||% "",
    n = suppressWarnings(as.numeric(res$n %||% 0))
  )
}

# ==============================================================================
# 7. EXCEL FORMAT
# ==============================================================================

apply_row_colors <- function(wb, sheet, df) {
  if (!"relevance" %in% names(df) || nrow(df) == 0) return(invisible(NULL))

  green_style  <- createStyle(fgFill = "#C6EFCE")
  yellow_style <- createStyle(fgFill = "#FFEB9C")
  red_style    <- createStyle(fgFill = "#FFC7CE")
  header_style <- createStyle(textDecoration = "bold", fgFill = "#D9EAF7", border = "Bottom")

  addStyle(wb, sheet, header_style, rows = 1, cols = 1:ncol(df), gridExpand = TRUE)

  for (i in seq_len(nrow(df))) {
    rel <- toupper(df$relevance[i] %||% "")
    style <- switch(
      rel,
      "GREEN" = green_style,
      "YELLOW" = yellow_style,
      "RED" = red_style,
      NULL
    )
    if (!is.null(style)) {
      addStyle(wb, sheet, style, rows = i + 1, cols = 1:ncol(df), gridExpand = TRUE)
    }
  }
}

# ==============================================================================
# 8. UI
# ==============================================================================

ui <- fluidPage(
  useShinyjs(),
  theme = shinythemes::shinytheme("cosmo"),

  titlePanel("CardioLit AI"),

  sidebarLayout(
    sidebarPanel(
      textAreaInput("q", "Question", "TTVR outcomes for severe TR", rows = 3),
      textAreaInput("o", "Outcomes", "Mortality, TR grade", rows = 2),
      numericInput("retmax", "Abstracts", value = 20, min = 1, max = 200),
      numericInput("min_n", "Minimum sample size", value = 30, min = 0, max = 100000),
      actionButton("run", "Run", class = "btn-primary"),
      br(), br(),
      downloadButton("download", "Download Excel")
    ),

    mainPanel(
      h4("System Log"),
      verbatimTextOutput("log")
    )
  )
)

# ==============================================================================
# 9. SERVER
# ==============================================================================

server <- function(input, output, session) {
  result_file <- reactiveVal(NULL)
  log_text <- reactiveVal("Ready")

  output$log <- renderText({
    log_text()
  })

  observeEvent(input$run, {
    disable("run")
    log_text("Running...")

    tryCatch({
      ids <- search_pubmed(input$q, input$retmax)
      df <- fetch_pubmed(ids)

      if (nrow(df) == 0) stop("No abstracts found")

      log_text(paste("Fetched", nrow(df), "abstracts. Running AI triage..."))

      results <- vector("list", nrow(df))

      for (i in seq_len(nrow(df))) {
        log_text(paste("Processing", i, "/", nrow(df)))

        if (is.na(df$abstract[i]) || trimws(df$abstract[i]) == "") {
          results[[i]] <- df[i, ] %>%
            mutate(
              relevance = "RED",
              sample_size = 0,
              triage_reason = "no_abstract"
            )
          next
        }

        t <- tryCatch(
          triage_one(df$title[i], df$abstract[i], input$q, model = DEFAULT_MODEL),
          error = function(e) {
            list(relevance = "RED", reason = "api_error", n = 0)
          }
        )

        results[[i]] <- df[i, ] %>%
          mutate(
            relevance = t$relevance %||% "RED",
            sample_size = as.numeric(t$n %||% 0),
            triage_reason = t$reason %||% ""
          )

        Sys.sleep(0.2)
      }

      df <- bind_rows(results) %>%
        mutate(
          sample_size = ifelse(is.na(sample_size), 0, sample_size),
          relevance = case_when(
            sample_size < input$min_n ~ "RED",
            TRUE ~ relevance
          )
        )

      wb <- createWorkbook()

      addWorksheet(wb, "Results")
      writeData(wb, "Results", df)
      apply_row_colors(wb, "Results", df)

      addWorksheet(wb, "Parameters")
      writeData(
        wb, "Parameters",
        tibble(
          Field = c("Question", "Outcomes", "Abstracts Requested", "Minimum Sample Size", "Model"),
          Value = c(input$q, input$o, input$retmax, input$min_n, DEFAULT_MODEL)
        )
      )

      fname <- file.path(
        tempdir(),
        paste0("result_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
      )

      saveWorkbook(wb, fname, overwrite = TRUE)
      result_file(fname)

      log_text("Completed with AI triage ✔")

    }, error = function(e) {
      log_text(paste("Error:", e$message))
    })

    enable("run")
  })

  output$download <- downloadHandler(
    filename = function() {
      paste0("CardioLit_Review_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      req(result_file())
      file.copy(result_file(), file, overwrite = TRUE)
    }
  )
}

# ==============================================================================
# 10. RUN APP
# ==============================================================================

shinyApp(ui, server)
