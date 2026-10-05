# app.R

library(shiny)
library(grid)

# Install these ONCE from the R console if necessary:
# install.packages(c("bslib", "rvest", "xml2", "zip", "png", "jpeg"))

library(shiny)
library(grid)

# Directory in cui viene eseguita l'app
APP_DIR <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

WWW_DIR <- file.path(APP_DIR, "www")

LOGO_PATH <- file.path(WWW_DIR, "logo.png")

SIGNATURE_PATHS <- c(
  file.path(WWW_DIR, "signature_dario.png"),
  file.path(WWW_DIR, "signature_cristian.png"),
  file.path(WWW_DIR, "signature_pietro.png")
)

# Controllo all'avvio
message("APP_DIR: ", APP_DIR)
message("Logo: ", LOGO_PATH, " -> ", file.exists(LOGO_PATH))
message("Dario signature: ", SIGNATURE_PATHS[1], " -> ", file.exists(SIGNATURE_PATHS[1]))
message("Cristian signature: ", SIGNATURE_PATHS[2], " -> ", file.exists(SIGNATURE_PATHS[2]))
message("Pietro signature: ", SIGNATURE_PATHS[3], " -> ", file.exists(SIGNATURE_PATHS[3]))

# ------------------------------------------------------------
# Utility functions
# ------------------------------------------------------------

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


clean_names <- function(x) {

  x <- unlist(strsplit(x, "\n|;|,", perl = TRUE))
  x <- trimws(x)
  x <- x[nzchar(x)]

  unique(x)
}


read_names_from_file <- function(path) {

  ext <- tolower(tools::file_ext(path))

  if (ext == "csv") {

    dat <- tryCatch(
      read.csv(path, stringsAsFactors = FALSE),
      error = function(e) NULL
    )

    if (is.null(dat) || ncol(dat) == 0) {
      return(character(0))
    }

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

  if (!has_pkg("rvest") || !has_pkg("xml2")) {
    stop(
      "Packages 'rvest' and 'xml2' are required. ",
      "Install them with: install.packages(c('rvest', 'xml2'))"
    )
  }

  page <- tryCatch(
    rvest::read_html(url),
    error = function(e) {
      stop("Unable to access webpage: ", conditionMessage(e))
    }
  )

  # Speaker names in the GMTD Quarto website
  nodes <- rvest::html_elements(
    page,
    ".speaker-profile h3, .speaker-card h3"
  )

  # Fallback
  if (length(nodes) == 0) {
    nodes <- rvest::html_elements(page, "h3")
  }

  x <- rvest::html_text2(nodes)

  x <- trimws(x)
  x <- x[nzchar(x)]
  x <- unique(x)

  if (length(x) == 0) {
    stop("No speaker names found on the webpage.")
  }

  x
}


# ------------------------------------------------------------
# Image reader
# ------------------------------------------------------------

read_image <- function(path) {

  if (
    is.null(path) ||
    length(path) == 0 ||
    is.na(path) ||
    !nzchar(path) ||
    !file.exists(path)
  ) {
    return(NULL)
  }

  ext <- tolower(tools::file_ext(path))

  if (ext == "png" && has_pkg("png")) {
    return(png::readPNG(path))
  }

  if (ext %in% c("jpg", "jpeg") && has_pkg("jpeg")) {
    return(jpeg::readJPEG(path))
  }

  NULL
}

prepare_signature <- function(path) {

  img <- read_image(path)

  if (is.null(img)) {
    return(NULL)
  }

  # Ensure RGB/RGBA array
  if (length(dim(img)) != 3) {
    return(NULL)
  }

  nr <- dim(img)[1]
  nc <- dim(img)[2]
  nch <- dim(img)[3]

  # RGB channels
  r <- img[, , 1]
  g <- img[, , 2]
  b <- img[, , 3]

  # Existing alpha, if present
  if (nch >= 4) {
    old_alpha <- img[, , 4]
  } else {
    old_alpha <- matrix(1, nr, nc)
  }

  # Distance from white:
  # white background -> alpha ~0
  # dark/coloured ink -> alpha increases
  ink_alpha <- pmax(
    1 - r,
    1 - g,
    1 - b
  )

  # Increase visibility of light strokes
  ink_alpha <- pmin(
    1,
    ink_alpha * 1.6
  )

  # Combine with original transparency
  new_alpha <- ink_alpha * old_alpha

  # Remove very faint background noise
  new_alpha[new_alpha < 0.08] <- 0

  # Create WHITE signature with transparent background
  out <- array(
    1,
    dim = c(nr, nc, 4)
  )

  out[, , 1] <- 1
  out[, , 2] <- 1
  out[, , 3] <- 1
  out[, , 4] <- new_alpha

  out
}

# ------------------------------------------------------------
# Certificate generation
# ------------------------------------------------------------

draw_certificate <- function(

  name,

  role,

  outfile,

  organisers = c(
    "Dario Righelli",
    "Cristian Taccioli",
    "Pietro Liò"
  ),

  workshop_title =
    "Generative Models for Therapeutic Discovery",

  workshop_subtitle =
    "Learning molecules, targets and cell-state responses",

  dates =
    "29–30 September 2026",

  venue =
    "University of Cambridge · Computer Laboratory",

  logo_path =
    "www/logo.png",

  signature_paths = c(
    "www/signature_dario.png",
    "www/signature_cristian.png",
    "www/signature_pietro.png"
  )

) {


  pdf(
    outfile,
    width = 11.69,
    height = 8.27,
    paper = "special"
  )

  on.exit(dev.off(), add = TRUE)


  grid.newpage()


  # ----------------------------------------------------------
  # Background
  # ----------------------------------------------------------

  grid.rect(
    gp = gpar(
      fill = "#111827",
      col = NA
    )
  )


  # Main panel

  grid.roundrect(

    x = 0.5,
    y = 0.5,

    width = 0.88,
    height = 0.78,

    r = unit(0.04, "npc"),

    gp = gpar(
      fill = "#1f2937",
      col = "#4f46e5",
      lwd = 2
    )
  )


  # Decorative circles

  grid.circle(

    x = 0.12,
    y = 0.83,
    r = 0.16,

    gp = gpar(
      fill = "#312e81",
      col = NA,
      alpha = 0.55
    )
  )


  grid.circle(

    x = 0.88,
    y = 0.18,
    r = 0.18,

    gp = gpar(
      fill = "#1d4ed8",
      col = NA,
      alpha = 0.35
    )
  )


  # ----------------------------------------------------------
  # Logo
  # ----------------------------------------------------------

  logo <- read_image(logo_path)

  if (!is.null(logo)) {

    grid.raster(

      logo,

      x = 0.5,
      y = 0.79,

      width = 0.10,

      interpolate = TRUE
    )
  }


  # ----------------------------------------------------------
  # Certificate title
  # ----------------------------------------------------------

  grid.text(

    "Certificate of Attendance",

    x = 0.5,
    y = 0.71,

    gp = gpar(
      col = "white",
      fontsize = 34,
      fontface = "bold"
    )
  )


  grid.text(

    "This certifies that",

    x = 0.5,
    y = 0.63,

    gp = gpar(
      col = "#d1d5db",
      fontsize = 18
    )
  )


  # ----------------------------------------------------------
  # Participant name
  # ----------------------------------------------------------

  grid.text(

    name,

    x = 0.5,
    y = 0.55,

    gp = gpar(
      col = "#93c5fd",
      fontsize = 32,
      fontface = "bold"
    )
  )


  # ----------------------------------------------------------
  # Role
  # ----------------------------------------------------------

  role_label <- if (role == "Invited speaker") {

    "participated as an invited speaker in"

  } else {

    "participated in"
  }


  grid.text(

    role_label,

    x = 0.5,
    y = 0.47,

    gp = gpar(
      col = "#d1d5db",
      fontsize = 17
    )
  )


  # ----------------------------------------------------------
  # Workshop
  # ----------------------------------------------------------

  grid.text(

    workshop_title,

    x = 0.5,
    y = 0.40,

    gp = gpar(
      col = "white",
      fontsize = 24,
      fontface = "bold"
    )
  )


  grid.text(

    workshop_subtitle,

    x = 0.5,
    y = 0.35,

    gp = gpar(
      col = "#c7d2fe",
      fontsize = 16
    )
  )


  grid.text(

    paste0(
      venue,
      " · ",
      dates
    ),

    x = 0.5,
    y = 0.29,

    gp = gpar(
      col = "#d1d5db",
      fontsize = 15
    )
  )


  # ----------------------------------------------------------
  # Divider
  # ----------------------------------------------------------

  grid.lines(

    x = c(0.22, 0.78),

    y = c(
      0.245,
      0.245
    ),

    gp = gpar(
      col = "#4f46e5",
      lwd = 1.5
    )
  )


  # ----------------------------------------------------------
  # Organising committee
  # ----------------------------------------------------------

  grid.text(

    "Organising committee",

    x = 0.5,
    y = 0.215,

    gp = gpar(
      col = "#d1d5db",
      fontsize = 12
    )
  )


  # Horizontal positions of the three organisers

  organiser_x <- c(
    0.30,
    0.50,
    0.70
  )


  # ----------------------------------------------------------
  # Names + signatures
  # ----------------------------------------------------------

  for (i in seq_along(organisers)) {


    # Organiser name

    grid.text(

      organisers[i],

      x = organiser_x[i],
      y = 0.175,

      gp = gpar(
        col = "white",
        fontsize = 13,
        fontface = "bold"
      )
    )


    # Signature BELOW the organiser name

    if (
      !is.null(signature_paths) &&
      length(signature_paths) >= i
    ) {

      signature <- prepare_signature(
        signature_paths[i]
      )


      if (!is.null(signature)) {

        grid.raster(

          signature,

          x = organiser_x[i],
          y = 0.135,

          width = 0.115,
          height = 0.055,

          interpolate = TRUE
        )
      }
    }
  }


  invisible(outfile)
}


# ------------------------------------------------------------
# UI
# ------------------------------------------------------------

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


  titlePanel(
    "GMTD 2026 Certificate Generator"
  ),


  sidebarLayout(


    sidebarPanel(


      h4("Input names"),


      textInput(

        "single_name",

        "Single name",

        placeholder =
          "e.g. Esther Wershof"
      ),


      textAreaInput(

        "name_list",

        "Or paste a list of names",

        placeholder =
          "One name per line",

        height = "140px"
      ),


      fileInput(

        "file",

        "Or upload CSV/TXT file",

        accept = c(
          ".csv",
          ".txt",
          ".tsv"
        )
      ),


      textInput(

        "url",

        "Or read names from webpage",

        value =
          "https://www.gmtd2026.org/speakers.html"
      ),


      actionButton(

        "load_url",

        "Load names from URL"
      ),


      hr(),


      selectInput(

        "role",

        "Certificate type",

        choices = c(
          "Participant",
          "Invited speaker"
        ),

        selected =
          "Participant"
      ),


      textInput(

        "organisers",

        "Organisers",

        value =
          "Dario Righelli; Cristian Taccioli; Pietro Liò"
      ),


      textInput(

        "logo_path",

        "Logo path",

        value =
          "www/logo.png"
      ),


      hr(),


      helpText(
        "Signatures are automatically loaded from the www/ directory."
      ),


      tags$small(
        "signature_dario.png · signature_cristian.png · signature_pietro.png"
      ),


      br(),
      br(),


      downloadButton(

        "download_zip",

        "Download certificates"
      )
    ),


    mainPanel(


      h4(
        "Names to be included"
      ),


      tableOutput(
        "names_table"
      ),


      hr(),


      h4("Notes"),


      tags$ul(

        tags$li(
          "CSV files should preferably contain a column named 'name'."
        ),

        tags$li(
          "If no 'name' column is found, the first column is used."
        ),

        tags$li(
          "Speaker names can be imported directly from the GMTD website."
        ),

        tags$li(
          "The three organiser signatures are loaded automatically from www/."
        ),

        tags$li(
          "Certificates are generated as PDF files and downloaded as a ZIP archive."
        )
      )
    )
  )
)


# ------------------------------------------------------------
# Server
# ------------------------------------------------------------

server <- function(
  input,
  output,
  session
) {


  url_names <- reactiveVal(
    character(0)
  )


  # ----------------------------------------------------------
  # Load speaker names from URL
  # ----------------------------------------------------------

  observeEvent(
    input$load_url,
    {

      tryCatch({

        x <- read_names_from_url(
          input$url
        )


        url_names(x)


        showNotification(

          paste(
            "Successfully loaded",
            length(x),
            "speakers."
          ),

          type = "message"
        )


      }, error = function(e) {


        showNotification(

          conditionMessage(e),

          type = "error",

          duration = 10
        )

      })
    }
  )


  # ----------------------------------------------------------
  # Combine all names
  # ----------------------------------------------------------

  all_names <- reactive({


    x <- character(0)


    if (nzchar(input$single_name)) {

      x <- c(
        x,
        input$single_name
      )
    }


    if (nzchar(input$name_list)) {

      x <- c(
        x,
        clean_names(
          input$name_list
        )
      )
    }


    if (!is.null(input$file)) {

      x <- c(
        x,
        read_names_from_file(
          input$file$datapath
        )
      )
    }


    x <- c(
      x,
      url_names()
    )


    x <- trimws(x)

    x <- x[nzchar(x)]

    unique(x)
  })


  # ----------------------------------------------------------
  # Table preview
  # ----------------------------------------------------------

  output$names_table <- renderTable({


    data.frame(

      Name =
        all_names(),

      Role =
        input$role,

      stringsAsFactors =
        FALSE
    )
  })


  # ----------------------------------------------------------
  # Download ZIP
  # ----------------------------------------------------------

  output$download_zip <- downloadHandler(


    filename = function() {

      paste0(
        "gmtd2026_certificates_",
        Sys.Date(),
        ".zip"
      )
    },


    content = function(file) {


      names <- all_names()


      validate(

        need(
          length(names) > 0,
          "Please provide at least one name."
        )
      )


      tmp <- tempfile(
        "certificates_"
      )


      dir.create(tmp)


      organisers <- clean_names(

        gsub(
          ";",
          "\n",
          input$organisers
        )
      )


      # Fixed signature locations

      signature_paths <- c(

        "www/signature_dario.png",

        "www/signature_cristian.png",

        "www/signature_pietro.png"
      )


      pdf_files <- character(0)


      for (nm in names) {


        out <- file.path(

          tmp,

          paste0(
            "GMTD2026_certificate_",
            slugify(nm),
            ".pdf"
          )
        )


        draw_certificate(

          name =
            nm,

          role =
            input$role,

          outfile =
            out,

          organisers =
            organisers,

          logo_path =
            input$logo_path,

          signature_paths =
            signature_paths
        )


        pdf_files <- c(
          pdf_files,
          out
        )
      }


      oldwd <- getwd()


      setwd(tmp)


      on.exit(
        setwd(oldwd),
        add = TRUE
      )


      if (has_pkg("zip")) {

        zip::zipr(

          zipfile = file,

          files =
            basename(pdf_files)
        )

      } else {

        utils::zip(

          zipfile = file,

          files =
            basename(pdf_files)
        )
      }
    },


    contentType =
      "application/zip"
  )
}


# ------------------------------------------------------------
# Run application
# ------------------------------------------------------------

shinyApp(
  ui,
  server
)
