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

# ==============================================================================
# 4. AI REQUEST (HTTR VERSION)
# ==============================================================================

openai_request <- function(messages, model) {

  api_key <- Sys.getenv("PORTKEY_API_KEY")
  if (api_key == "") stop("PORTKEY_API_KEY not set")

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
    body = toJSON(body, auto_unbox = TRUE),
    encode = "raw"
  )

  stop_for_status(res)

  out <- fromJSON(content(res, "text", encoding = "UTF-8"))

  if (is.null(out$choices)) stop("No response from API")

  out$choices[[1]]$message$content
}

ask_json <- function(prompt, model) {

  txt <- openai_request(
    list(list(role = "user", content = paste(prompt, "\nReturn ONLY JSON."))),
    model
  )

  txt <- gsub("^```json\\s*|\\s*```$", "", txt)

  fromJSON(txt, simplifyVector = FALSE)
}

# ==============================================================================
# 5. PUBMED
# ==============================================================================

search_pubmed <- function(query, retmax = 50, mindate = NULL, maxdate = NULL) {

  url <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi"

  res <- GET(url, query = list(
    db = "pubmed",
    term = query,
    retmode = "json",
    retmax = retmax
  ))

  out <- fromJSON(content(res, "text"))

  out$esearchresult$idlist %||% character(0)
}

fetch_pubmed <- function(ids) {

  if (length(ids) == 0) return(tibble())

  url <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi"

  res <- GET(url, query = list(
    db = "pubmed",
    id = paste(ids, collapse = ","),
    retmode = "xml"
  ))

  doc <- read_xml(content(res, "text"))

  arts <- xml_find_all(doc, ".//PubmedArticle")

  map_dfr(arts, function(a) {

    tibble(
      pmid = xml_text(xml_find_first(a, ".//PMID")),
      title = xml_text(xml_find_first(a, ".//ArticleTitle")),
      abstract = paste(xml_text(xml_find_all(a, ".//AbstractText")), collapse = " ")
    )

  }) %>% filter(abstract != "")
}

# ==============================================================================
# 6. UI
# ==============================================================================

ui <- fluidPage(
  useShinyjs(),
  theme = shinythemes::shinytheme("cosmo"),

  titlePanel("CardioLit AI"),

  sidebarLayout(
    sidebarPanel(
      textAreaInput("q", "Question", "TTVR outcomes for severe TR"),
      textAreaInput("o", "Outcomes", "Mortality, TR grade"),

      numericInput("retmax", "Abstracts", 20),

      actionButton("run", "Run"),
      downloadButton("download", "Download")
    ),

    mainPanel(
      verbatimTextOutput("log")
    )
  )
)

# ==============================================================================
# 7. SERVER
# ==============================================================================

server <- function(input, output, session) {

  result_file <- reactiveVal(NULL)

  observeEvent(input$run, {

    output$log <- renderText("Running...")

    tryCatch({

      ids <- search_pubmed(input$q, input$retmax)
      df <- fetch_pubmed(ids)

      if (nrow(df) == 0) stop("No abstracts")

      df$relevance <- "GREEN"

      wb <- createWorkbook()
      addWorksheet(wb, "Results")
      writeData(wb, "Results", df)

      fname <- paste0("result_", Sys.time(), ".xlsx")
      saveWorkbook(wb, fname, overwrite = TRUE)

      result_file(fname)

      output$log <- renderText("Done.")

    }, error = function(e) {
      output$log <- renderText(paste("Error:", e$message))
    })
  })

  output$download <- downloadHandler(
    filename = function() result_file(),
    content = function(file) file.copy(result_file(), file)
  )
}

shinyApp(ui, server)
