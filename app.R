# ==============================================================================
# 1. PACKAGES
# ==============================================================================

suppressPackageStartupMessages({
  library(httr2); library(jsonlite); library(xml2); library(dplyr)
  library(purrr); library(stringr); library(tidyr); library(openxlsx)
  library(glue); library(tibble); library(shiny); library(shinythemes); library(shinyjs)
})
MODEL_TRIAGE  <- "@OpenAI/gpt-5.4"
MODEL_EXTRACT <- "@OpenAI/gpt-5.4"
OPENAI_BASE   <- "https://api.portkey.ai/v1/chat/completions"

MODEL_NAME  <- "gpt-5.4"
OPENAI_BASE <- "https://api.portkey.ai/v1/chat/completions"

openai_request <- function(messages, response_format = NULL) {
  message("PORTKEY_API_KEY empty? ", Sys.getenv("PORTKEY_API_KEY") == "")
  message("PORTKEY_API_KEY empty? ", Sys.getenv("PORTKEY_API_KEY") == "")
  message("PORTKEY_API_KEY nchar: ", nchar(Sys.getenv("PORTKEY_API_KEY")))
  req_body <- list(
    model = MODEL_NAME,
    messages = messages,
    temperature = 0
  )

  if (!is.null(response_format)) {
    req_body$response_format <- response_format
  }

  res <- request(OPENAI_BASE) |>
    req_headers(
      "Content-Type" = "application/json",
      "x-portkey-api-key" = Sys.getenv("PORTKEY_API_KEY"),
      "x-portkey-provider" = "@OpenAI/gpt-5.4"
    ) |>
    req_body_json(req_body, auto_unbox = TRUE) |>
    req_timeout(90) |>
    req_retry(max_tries = 3) |>
    req_perform() |>
    resp_body_json()

  if (is.null(res$choices) || length(res$choices) == 0 ||
      is.null(res$choices[[1]]$message$content)) {
    stop("No valid choices returned from the model.")
  }

  res$choices[[1]]$message$content
}

ask_json <- function(prompt, schema) {
  txt <- openai_request(
    list(list(role = "user", content = prompt)),
    response_format = list(
      type = "json_schema",
      json_schema = list(name = "schema", schema = schema)
    )
  )
  txt <- gsub("^```json\\s*|\\s*```$", "", txt)
  fromJSON(txt, simplifyVector = FALSE)
}

sanitize_colname <- function(x) {
  x |>
    toupper() |>
    str_replace_all("[^A-Z0-9]+", "_") |>
    str_replace_all("^_|_$", "")
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# ==============================================================================
# 3. PUBMED FUNCTIONS
# ==============================================================================
search_pubmed <- function(query, retmax = 50, mindate = NULL, maxdate = NULL) {
  url   <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi"
  fmt_d <- function(d) if (!is.null(d)) format(as.Date(d), "%Y/%m/%d") else NULL
  
  res <- request(url) |>
    req_url_query(
      db       = "pubmed",
      term     = query,
      retmode  = "json",
      retmax   = retmax,
      datetype = "pdat",
      mindate  = fmt_d(mindate),
      maxdate  = fmt_d(maxdate)
    ) |>
    req_perform() |>
    resp_body_json()
  
  res$esearchresult$idlist %||% character(0)
}

fetch_pubmed <- function(ids) {
  if (length(ids) == 0) return(tibble())
  
  url     <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi"
  xml_str <- request(url) |>
    req_url_query(db = "pubmed", id = paste(ids, collapse = ","), retmode = "xml") |>
    req_perform() |>
    resp_body_string()
  
  doc  <- read_xml(xml_str)
  arts <- xml_find_all(doc, ".//PubmedArticle")
  
  map_dfr(arts, function(a) {
    abs_nodes <- xml_find_all(a, ".//AbstractText")
    tibble(
      pmid     = xml_text(xml_find_first(a, ".//PMID")),
      title    = xml_text(xml_find_first(a, ".//ArticleTitle")),
      abstract = paste(xml_text(abs_nodes), collapse = " "),
      journal  = xml_text(xml_find_first(a, ".//Journal/Title")),
      year     = str_extract(xml_text(xml_find_first(a, ".//PubDate")), "\\d{4}")
    )
  }) |>
    filter(!is.na(abstract), abstract != "")
}

# ==============================================================================
# 4. EXCEL COLORING
# ==============================================================================
apply_row_colors <- function(wb, sheet, df) {
  green_style  <- createStyle(fgFill = "#C6EFCE")
  yellow_style <- createStyle(fgFill = "#FFEB9C")
  red_style    <- createStyle(fgFill = "#FFC7CE")
  header_style <- createStyle(textDecoration = "bold", fgFill = "#D9EAF7", border = "Bottom")
  
  addStyle(wb, sheet, header_style, rows = 1, cols = 1:ncol(df), gridExpand = TRUE)
  for (i in seq_len(nrow(df))) {
    rel   <- toupper(df$relevance[i] %||% "")
    style <- switch(rel, GREEN = green_style, YELLOW = yellow_style, RED = red_style, NULL)
    if (!is.null(style)) addStyle(wb, sheet, style, rows = i + 1, cols = 1:ncol(df), gridExpand = TRUE)
  }
}

# ==============================================================================
# 5. QUERY MODE PROMPT BUILDER
# ==============================================================================
build_query_prompt <- function(question, mode) {
  mode_rules <- switch(
    mode,
    "Focused" = "
- Use MAX 2 synonyms per concept.
- Prefer highly specific clinical terminology.
- Strongly prioritize precision over recall.
- Avoid MeSH explosion unless absolutely necessary.
",
    "Balanced" = "
- Use 3-4 synonyms per concept.
- Combine MeSH and Title/Abstract terms.
- Maintain balance between recall and precision.
",
    "Broad" = "
- Use multiple synonyms and include MeSH terms.
- Allow broader semantic expansion.
- Still enforce logical AND structure to avoid noise.
",
    "Default" = "
- Maintain clear concept separation and Boolean structure.
"
  )
  
  glue("
You are a senior medical information specialist with expertise in PubMed query construction.

Your task is to transform a clinical research question into a precise PubMed search query.

---------------------
STEP 1: CONCEPT IDENTIFICATION
---------------------
- Identify TWO core concepts:
  1. Intervention / Exposure (e.g., TTVR, MitraClip, TAVR)
  2. Population / Disease (e.g., tricuspid regurgitation, aortic stenosis)

---------------------
STEP 2: SYNONYM GENERATION
---------------------
- Generate controlled synonyms for each concept
- Include:
  - Full medical terms (no abbreviations like TR, MR)
  - Device names + generic class if applicable
    Example:
      \"Evoque\" -> also include \"transcatheter tricuspid valve replacement\"

---------------------
STEP 3: FIELD TAGGING
---------------------
- Use:
  [Title/Abstract] for clinical terms
  [MeSH Terms] only if highly relevant

---------------------
STEP 4: BOOLEAN STRUCTURE (MANDATORY)
---------------------
Final structure MUST be:

(
  Concept1_term1[Title/Abstract] OR Concept1_term2[Title/Abstract] ...
)
AND
(
  Concept2_term1[Title/Abstract] OR Concept2_term2[Title/Abstract] ...
)

---------------------
STEP 5: NOISE CONTROL
---------------------
- DO NOT include:
  outcomes, mortality, results, management
- DO NOT include:
  \"human\", \"English\", publication type filters
- DO NOT include:
  abbreviations like TR, MR, AS unless expanded

---------------------
STEP 6: MODE
---------------------
Current mode: {mode}
{mode_rules}

---------------------
FINAL OUTPUT
---------------------
- Return ONLY the final PubMed query string
- No explanation
- No formatting text
- No line breaks

---------------------
INPUT QUESTION
---------------------
{question}
")
}

# ==============================================================================
# 6. SHINY UI
# ==============================================================================
ui <- fluidPage(
  useShinyjs(),
  theme = shinythemes::shinytheme("cosmo"),
  titlePanel(tags$div(tags$i(class = "fa fa-heartbeat", style = "color:red"), " CardioLit AI Pro")),
  
  sidebarLayout(
    sidebarPanel(
      textAreaInput("q", "Research Question", value = "TTVR outcomes for severe TR", rows = 3),
      textAreaInput("o", "Outcomes (comma separated)", value = "Mortality, TR grade, NYHA class", rows = 2),
      
      selectInput(
        "query_mode",
        "Query Mode",
        choices  = c("Focused", "Balanced", "Broad"),
        selected = "Focused"
      ),
      
      splitLayout(
        numericInput("retmax", "Max Abstracts", 20, 5, 200),
        numericInput("min_n",  "Min Sample (N)", 30, 0, 5000)
      ),
      
      dateRangeInput("dates", "Date Range", start = Sys.Date() - 1825, end = Sys.Date()),
      
      hr(),
      actionButton("run",      "Start Review",   class = "btn-primary", style = "width:100%"),
      br(), br(),
      downloadButton("download", "Download Excel", class = "btn-success", style = "width:100%")
    ),
    
    mainPanel(
      h4("System Log"),
      wellPanel(
        style = "background: #f8f9fa; border: 1px solid #dee2e6;",
        verbatimTextOutput("log")
      )
    )
  )
)

# ==============================================================================
# 7. SHINY SERVER
# ==============================================================================
server <- function(input, output, session) {
  res_val <- reactiveVal(NULL)
  
  observeEvent(input$run, {
    disable("run")
    output$log <- renderText("Pipeline initiated...")
    
    tryCatch({
      withProgress(message = "Analyzing Cardiology Literature...", value = 0, {
        
        # ------------------------------------------------------------------
        # 1. Query Generation
        # ------------------------------------------------------------------
        incProgress(0.10, detail = "Formulating PubMed Query...")
        q_prompt <- build_query_prompt(input$q, input$query_mode)
        q_raw <- openai_request(list(list(role = "user", content = q_prompt)))
        q_final  <- paste0("(", trimws(q_raw), ") AND hasabstract[text] NOT \"case reports\"[PT]")
        
        # ------------------------------------------------------------------
        # 2. PubMed Search
        # ------------------------------------------------------------------
        incProgress(0.10, detail = "Searching PubMed Database...")
        ids <- search_pubmed(q_final, input$retmax, input$dates[1], input$dates[2])
        if (length(ids) == 0) stop("No records found for these criteria.")
        df <- fetch_pubmed(ids)
        if (nrow(df) == 0) stop("PubMed records retrieved but no abstracts could be parsed.")
        
        # ------------------------------------------------------------------
        # 3. Triage
        # ------------------------------------------------------------------
        triage_schema <- list(
          type       = "object",
          properties = list(
            level  = list(type = "string", enum = list("GREEN", "YELLOW", "RED")),
            reason = list(type = "string"),
            n      = list(type = "integer")
          ),
          required   = list("level", "reason", "n")
        )
        
        triage_list <- list()
        for (i in seq_len(nrow(df))) {
          incProgress(0.40 / nrow(df), detail = paste("Triage:", i, "/", nrow(df)))
          
          # FIX 1: define glue variables before the template
          title    <- df$title[i]
          abstract <- df$abstract[i]
          
          prompt <- glue("
You are a senior cardiology research reviewer performing abstract triage.

Research Question:
{input$q}

Title:
{title}

Abstract:
{abstract}

---------------------
TRIAGE RULES (STRICT)
---------------------

1. GREEN (High relevance)
- Directly answers the research question
- Contains clinical outcome data (e.g., mortality, rehospitalization, procedural success)
- Human study with analyzable data
- Includes quantitative results or clear clinical findings

2. YELLOW (Partial relevance)
- Related population or intervention but does NOT directly answer the question
- Background, subgroup, mechanistic, or indirect evidence
- Limited or unclear outcome reporting

3. RED (Exclude)
- Case reports or case series with N < 10
- Editorials, reviews without original data
- Animal or preclinical studies
- Clearly unrelated topic
- Imaging-only or technical feasibility without outcomes

---------------------
SAMPLE SIZE (CRITICAL)
---------------------
- Extract total study sample size (N)
- If multiple groups exist -> return TOTAL N
- If unclear -> return 0

---------------------
REASON
---------------------
- Provide ONE short reason (max 12 words)
- Be specific (e.g., \"Direct TTVR outcome study\", \"Review article\", \"Small case series\")

---------------------
ANTI-HALLUCINATION
---------------------
- DO NOT guess sample size
- DO NOT infer relevance beyond text
- If uncertain -> downgrade (prefer YELLOW over GREEN)

---------------------
OUTPUT (JSON ONLY)
---------------------
{{
  \"level\": \"GREEN | YELLOW | RED\",
  \"reason\": \"short reason\",
  \"n\": 123
}}
")
          
          t_res <- ask_json(prompt, triage_schema)
          
          rel  <- t_res$level
          reas <- t_res$reason
          if (t_res$n < input$min_n) {
            rel  <- "RED"
            reas <- paste0("Small sample (N=", t_res$n, ")")
          }
          
          triage_list[[i]] <- df[i, ] %>%
            mutate(
              relevance     = rel,
              triage_reason = reas,
              sample_size   = t_res$n
            )
        }
        triage_df <- bind_rows(triage_list)
        
        # ------------------------------------------------------------------
        # 4. Extraction
        # ------------------------------------------------------------------
        eligible_df   <- triage_df %>% filter(relevance %in% c("GREEN", "YELLOW"))
        outcomes_list <- trimws(strsplit(input$o, ",")[[1]])
        
        extract_schema <- list(
          type       = "object",
          properties = list(
            outcomes = list(
              type  = "array",
              items = list(
                type       = "object",
                properties = list(
                  name      = list(type = "string"),
                  rate      = list(type = "string"),
                  timepoint = list(type = "string"),
                  text      = list(type = "string")
                ),
                required = list("name", "rate", "timepoint", "text")
              )
            )
          ),
          required = list("outcomes")
        )
        
        final_rows <- list()
        if (nrow(eligible_df) > 0) {
          for (j in seq_len(nrow(eligible_df))) {
            incProgress(0.30 / nrow(eligible_df), detail = paste("Extracting:", j, "/", nrow(eligible_df)))
            
            # FIX 2: define abstract before glue template; remove duplicate injection
            abstract <- eligible_df$abstract[j]
            
            e_prompt <- glue("
You are an expert clinical data extractor in cardiology.

Your task is to extract structured outcome data from a PubMed abstract.

REQUESTED OUTCOMES:
{input$o}

ABSTRACT:
{abstract}

---------------------
EXTRACTION RULES
---------------------

1. OUTCOME MATCHING
- Treat the requested outcomes strictly as user-defined labels.
- Match outcomes semantically (not just exact wording).
  Example:
  \"all-cause mortality\" ~ \"mortality\"
  \"TR reduction\" ~ \"tricuspid regurgitation grade\"

2. MULTIPLE RESULTS
- If multiple values exist for the same outcome (e.g., 30-day and 1-year mortality):
  -> return the MOST clinically relevant OR longest follow-up.
- Prefer: 1-year > 6 months > 30 days > in-hospital

3. NUMERIC EXTRACTION PRIORITY
Extract the MOST important metric available:
- Percent (%)
- Hazard ratio (HR)
- Odds ratio (OR)
- Mean/median values
- Event rates
If multiple metrics exist -> prefer % or HR

4. TIMEPOINT EXTRACTION
Extract exact timepoint (e.g., in-hospital, 30 days, 6 months, 1 year).
If not stated -> \"NA\"

5. MISSING DATA HANDLING
- Outcome mentioned but no number -> \"reported_no_numeric_value\"
- Outcome absent -> \"NA\"

6. STRICT ANTI-HALLUCINATION
- DO NOT infer or guess numbers
- DO NOT fabricate outcomes
- ONLY extract what is explicitly present

7. TEXT SNIPPET
- Provide a SHORT exact snippet from the abstract supporting the value
- Max 20 words

8. OUTPUT CONSISTENCY
- Always return ALL requested outcomes
- Preserve requested naming as much as possible

---------------------
OUTPUT FORMAT (JSON ONLY)
---------------------
{{
  \"outcomes\": [
    {{
      \"name\": \"Mortality\",
      \"rate\": \"12%\",
      \"timepoint\": \"1 year\",
      \"text\": \"1-year mortality was 12% in the TTVR group\"
    }}
  ]
}}
")
            
            e_res <- ask_json(e_prompt, extract_schema)
            
            row <- eligible_df[j, ]
            for (oc in outcomes_list) {
              match <- detect(e_res$outcomes, ~ sanitize_colname(.x$name) == sanitize_colname(oc))
              row[[paste0(sanitize_colname(oc), "_RATE")]]      <- if (!is.null(match)) match$rate      else "NA"
              row[[paste0(sanitize_colname(oc), "_TIMEPOINT")]] <- if (!is.null(match)) match$timepoint else "NA"
              row[[paste0(sanitize_colname(oc), "_TEXT")]]      <- if (!is.null(match)) match$text      else "NA"
            }
            final_rows[[j]] <- row
          }
          final_df <- bind_rows(final_rows)
        } else {
          final_df <- triage_df[0, ]
        }
        
        # ------------------------------------------------------------------
        # 5. Excel Generation
        # ------------------------------------------------------------------
        incProgress(0.10, detail = "Saving Excel...")
        wb <- createWorkbook()
        
        addWorksheet(wb, "Parameters")
        writeData(
          wb, "Parameters",
          tibble(
            Field = c("Question", "Query_Mode", "Query", "Min_N", "Requested_Outcomes"),
            Value = c(input$q, input$query_mode, q_final, input$min_n, input$o)
          )
        )
        
        addWorksheet(wb, "Triage_All")
        writeData(wb, "Triage_All", triage_df)
        apply_row_colors(wb, "Triage_All", triage_df)
        
        addWorksheet(wb, "Final_Data")
        writeData(wb, "Final_Data", final_df)
        if (nrow(final_df) > 0) apply_row_colors(wb, "Final_Data", final_df)
        
        fname <- paste0("CardioLit_Review_", format(Sys.time(), "%Y%m%d_%H%M"), ".xlsx")
        saveWorkbook(wb, fname, overwrite = TRUE)
        res_val(fname)
        
        output$log <- renderText(
          paste0(
            "Success! Processed ", nrow(df), " abstracts.\n\n",
            "Query mode: ", input$query_mode, "\n\n",
            "Final Query:\n", q_final, "\n\n",
            "Saved file: ", fname
          )
        )
      })
    }, error = function(e) {
      output$log <- renderText(paste("Error:", e$message))
    })
    
    enable("run")
  })
  
  # FIX 3: guard download handler when no file is available yet
  output$download <- downloadHandler(
    filename = function() {
      req(res_val())
      res_val()
    },
    content = function(file) {
      req(res_val())
      file.copy(res_val(), file)
    }
  )
}

shinyApp(
  ui,
  server,
  options = list(
    host = "0.0.0.0",
    port = as.numeric(Sys.getenv("PORT", 3838))
  )
)

