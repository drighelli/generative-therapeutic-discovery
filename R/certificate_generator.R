# app.R

library(shiny)
library(grid)

# Optional packages:
# install.packages(c("bslib", "rvest", "xml2", "zip", "png", "jpeg"))

has_pkg <- function(pkg) {
  requireNamespace(pkg, quietly = TRUE)
}

slugify <- function(x) {
  x <- iconv(x, to = "ASCII//TRANSLIT")
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "-", x)
  x <- gsub("^-|-$", "", x)
  ifelse(nchar(x) == 0, "certificate", x)
}

read_names_from_file <- function(path) {
  ext <- tolower(tools::file_ext(path))

  if (ext %in% c("csv")) {
    dat <- tryCatch(read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL)
    if (is.null(dat) || ncol(dat) == 0) return(character(0))

    if ("name" %in% tolower(names(dat))) {
      idx <- which(tolower(names(dat)) == "name")[1]
      return(dat[[idx]])
    }

    return(dat[[1]])
  }

  if (ext %in% c("txt", "tsv")) {
    x <- readLines(path, warn = FALSE)
    return(x)
  }

  character(0)
}

read_names_from_url <- function(url) {

  url <- trimws(url)

  if (!nzchar(url)) {
    stop("Please enter a valid URL.")
  }

  if (!requireNamespace("rvest", quietly = TRUE)) {
    stop("Please install rvest: install.packages('rvest')")
  }

  page <- tryCatch(
    rvest::read_html(url),
    error = function(e) {
      stop("Unable to access webpage: ", conditionMessage(e))
    }
  )

  nodes <- rvest::html_elements(
    page,
    ".speaker-profile h3"
  )

  names <- trimws(rvest::html_text2(nodes))
  names <- unique(names[nzchar(names)])

  if (length(names) == 0) {
    stop("No speaker names found. Check the webpage structure.")
  }

  names
}

clean_names <- function(x) {
  x <- unlist(strsplit(x, "\n|;|,", perl = TRUE))
  x <- trimws(x)
  x <- x[nzchar(x)]
  unique(x)
}

draw_certificate <- function(
    name,
    role,
    outfile,
    organisers = c("Dario Righelli", "Cristian Taccioli", "Pietro Liò"),
    workshop_title = "Generative Models for Therapeutic Discovery",
    workshop_subtitle = "Learning molecules, targets and cell-state responses",
    dates = "29–30 September 2026",
    venue = "University of Cambridge · Computer Laboratory",
    logo_path = NULL
) {
  pdf(outfile, width = 11.69, height = 8.27, paper = "special")
  on.exit(dev.off(), add = TRUE)

  grid.newpage()

  # Background
  grid.rect(gp = gpar(fill = "#111827", col = NA))

  # Main panel
  grid.roundrect(
    x = 0.5, y = 0.5,
    width = 0.88, height = 0.78,
    r = unit(0.04, "npc"),
    gp = gpar(fill = "#1f2937", col = "#4f46e5", lwd = 2)
  )

  # Soft decorative circles
  grid.circle(
    x = 0.12, y = 0.83, r = 0.16,
    gp = gpar(fill = "#312e81", col = NA, alpha = 0.55)
  )

  grid.circle(
    x = 0.88, y = 0.18, r = 0.18,
    gp = gpar(fill = "#1d4ed8", col = NA, alpha = 0.35)
  )

  # Optional logo
  if (!is.null(logo_path) && file.exists(logo_path)) {
    ext <- tolower(tools::file_ext(logo_path))

    img <- NULL
    if (ext == "png" && has_pkg("png")) {
      img <- png::readPNG(logo_path)
    }
    if (ext %in% c("jpg", "jpeg") && has_pkg("jpeg")) {
      img <- jpeg::readJPEG(logo_path)
    }

    if (!is.null(img)) {
      grid.raster(
        img,
        x = 0.5, y = 0.78,
        width = 0.11,
        interpolate = TRUE
      )
    }
  }

  # Header
  grid.text(
    "Certificate of Attendance",
    x = 0.5, y = 0.72,
    gp = gpar(col = "white", fontsize = 34, fontface = "bold")
  )

  grid.text(
    "This certifies that",
    x = 0.5, y = 0.63,
    gp = gpar(col = "#d1d5db", fontsize = 18)
  )

  # Name
  grid.text(
    name,
    x = 0.5, y = 0.55,
    gp = gpar(col = "#93c5fd", fontsize = 32, fontface = "bold")
  )

  # Role sentence
  role_label <- if (role == "Invited speaker") {
    "participated as an invited speaker in"
  } else {
    "participated in"
  }

  grid.text(
    role_label,
    x = 0.5, y = 0.47,
    gp = gpar(col = "#d1d5db", fontsize = 17)
  )

  grid.text(
    workshop_title,
    x = 0.5, y = 0.40,
    gp = gpar(col = "white", fontsize = 24, fontface = "bold")
  )

  grid.text(
    workshop_subtitle,
    x = 0.5, y = 0.35,
    gp = gpar(col = "#c7d2fe", fontsize = 16)
  )

  grid.text(
    paste0(venue, " · ", dates),
    x = 0.5, y = 0.29,
    gp = gpar(col = "#d1d5db", fontsize = 15)
  )

  # Divider
  grid.lines(
    x = c(0.22, 0.78), y = c(0.23, 0.23),
    gp = gpar(col = "#4f46e5", lwd = 1.5)
  )

  # Organisers
  grid.text(
    "Organising committee",
    x = 0.5, y = 0.18,
    gp = gpar(col = "#d1d5db", fontsize = 13)
  )

  grid.text(
    paste(organisers, collapse = " · "),
    x = 0.5, y = 0.145,
    gp = gpar(col = "white", fontsize = 15, fontface = "bold")
  )

  invisible(outfile)
}

ui <- fluidPage(
  theme = if (has_pkg("bslib")) {
    bslib::bs_theme(
      version = 5,
      bootswatch = "darkly",
      primary = "#4f46e5"
    )
  } else {
    NULL
  },

  titlePanel("GMTD 2026 Certificate Generator"),

  sidebarLayout(
    sidebarPanel(
      h4("Input names"),

      textInput(
        "single_name",
        "Single name",
        placeholder = "e.g. Esther Wershof"
      ),

      textAreaInput(
        "name_list",
        "Or paste a list of names",
        placeholder = "One name per line",
        height = "140px"
      ),

      fileInput(
        "file",
        "Or upload CSV/TXT file",
        accept = c(".csv", ".txt", ".tsv")
      ),

      textInput(
        "url",
        "Or read names from webpage",
        placeholder = "https://www.gmtd2026.org/speakers.html"
      ),

      actionButton("load_url", "Load names from URL"),

      hr(),

      selectInput(
        "role",
        "Certificate type",
        choices = c("Participant", "Invited speaker"),
        selected = "Participant"
      ),

      textInput(
        "organisers",
        "Organisers",
        value = "Dario Righelli; Cristian Taccioli; Pietro Liò"
      ),

      textInput(
        "logo_path",
        "Optional logo path",
        value = "www/logo.png"
      ),

      hr(),

      downloadButton("download_zip", "Download certificates")
    ),

    mainPanel(
      h4("Names to be included"),
      tableOutput("names_table"),
      hr(),
      h4("Notes"),
      tags$ul(
        tags$li("CSV files should preferably contain a column named 'name'."),
        tags$li("If no 'name' column is found, the first column is used."),
        tags$li("For webpage import, the app tries to read speaker names from '.speaker-profile h3' or '.speaker-card h3'."),
        tags$li("Certificates are generated as PDF files and downloaded as a ZIP archive.")
      )
    )
  )
)

server <- function(input, output, session) {

  url_names <- reactiveVal(character(0))

  observeEvent(input$load_url, {

      tryCatch({

        x <- read_names_from_url(input$url)

        url_names(x)

        showNotification(
          paste("Successfully loaded", length(x), "speakers."),
          type = "message"
        )

      }, error = function(e) {

        showNotification(
          conditionMessage(e),
          type = "error",
          duration = 10
        )

      })

    })

  all_names <- reactive({
    x <- character(0)

    if (nzchar(input$single_name)) {
      x <- c(x, input$single_name)
    }

    if (nzchar(input$name_list)) {
      x <- c(x, clean_names(input$name_list))
    }

    if (!is.null(input$file)) {
      x <- c(x, read_names_from_file(input$file$datapath))
    }

    x <- c(x, url_names())

    x <- trimws(x)
    x <- x[nzchar(x)]
    unique(x)
  })

  output$names_table <- renderTable({
    data.frame(
      Name = all_names(),
      Role = input$role,
      stringsAsFactors = FALSE
    )
  })

  output$download_zip <- downloadHandler(
    filename = function() {
      paste0("gmtd2026_certificates_", Sys.Date(), ".zip")
    },

    content = function(file) {
      names <- all_names()

      validate(
        need(length(names) > 0, "Please provide at least one name.")
      )

      tmp <- tempfile("certificates_")
      dir.create(tmp)

      organisers <- clean_names(gsub(";", "\n", input$organisers))

      pdf_files <- character(0)

      for (nm in names) {
        out <- file.path(
          tmp,
          paste0("GMTD2026_certificate_", slugify(nm), ".pdf")
        )

        draw_certificate(
          name = nm,
          role = input$role,
          outfile = out,
          organisers = organisers,
          logo_path = input$logo_path
        )

        pdf_files <- c(pdf_files, out)
      }

      oldwd <- getwd()
      setwd(tmp)
      on.exit(setwd(oldwd), add = TRUE)

      if (has_pkg("zip")) {
        zip::zipr(zipfile = file, files = basename(pdf_files))
      } else {
        utils::zip(zipfile = file, files = basename(pdf_files))
      }
    },

    contentType = "application/zip"
  )
}



shinyApp(ui, server)
