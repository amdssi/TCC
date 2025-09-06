#Script 5 - Aplicações Reais

library(shiny)
library(plotly)
library(dplyr)

label_pi <- function(vals) {
  sapply(vals, function(v) {
    if (abs(v) < 1e-10) return("0")
    k <- v / pi
    if (abs(k - round(k)) < 1e-10) {
      k <- round(k); s <- ifelse(k < 0, "-", ""); a <- abs(k)
      return(paste0(s, ifelse(a == 1, "π", paste0(a, "π"))))
    }
    if (abs(k*2 - round(k*2)) < 1e-10) {
      m <- round(k*2); s <- ifelse(m < 0, "-", ""); a <- abs(m)
      return(paste0(s, ifelse(a == 1, "π/2", paste0(a, "π/2"))))
    }
    sprintf("%.2fπ", k)
  }, USE.NAMES = FALSE)
}

fmt_num <- function(x, k=2) {
  x <- ifelse(abs(x) < 1e-12, 0, x)
  formatC(x, format = "f", digits = k)
}

rmse <- function(y, yhat) sqrt(mean((y - yhat)^2, na.rm = TRUE))

# UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .note { background:#f7f9fc; border:1px solid #e5e9f0; border-radius:10px; padding:10px; }
      .eqbox { background:#fff; border:1px dashed #cbd3df; border-radius:10px; padding:10px; margin-bottom:10px; }
      .side-buttons { display:flex; gap:8px; justify-content:center; flex-wrap:wrap; }
      .imgwrap { text-align:center; margin-top:8px; }
      .imgwrap img { max-width:220px; max-height:180px; width:auto; height:auto;
                     object-fit:contain; border-radius:8px; border:1px solid #e5e9f0; }
    "))
  ),
  titlePanel("Aplicações reais das funções trigonométricas"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("phenom", "Fenômeno:",
                   choices = c("Onda sonora" = "sound",
                               "Pêndulo simples" = "pendulum",
                               "Corrente alternada (AC)" = "ac"),
                   selected = "sound"),
      tags$hr(),
      
      sliderInput("A", "Amplitude (A):", min = -5, max = 5, value = 1.2, step = 0.1),
      sliderInput("B", "Frequência/ω (B):", min = 0.2, max = 10, value = 2.0, step = 0.1),
      sliderInput("C", "Fase/φ (C, rad):",
                  min = round(-2*pi, 2), max = round(2*pi, 2),
                  value = 0, step = 0.01),
      
      sliderInput("range_x", "Janela de tempo x (s ou rad):",
                  min = 0, max = 20, value = c(0, 10), step = 0.1),
      
      div(class="side-buttons",
          actionButton("reset", "Restaurar padrão")
      ),
      tags$hr(),
      div(class="note",
          strong("Dica:"),
          HTML("<br>Use <b>A</b> para altura da onda, <b>B</b> para período/velocidade de oscilação,
               e <b>C</b> para alinhar o início (fase). O erro <i>RMSE</i> ajuda a ver quão bem o modelo aproxima os dados.")
      ),
      width = 3
    ),
    mainPanel(
      div(class="eqbox",
          htmlOutput("eq_box")
      ),
      plotlyOutput("plot", height = "520px"),
      tags$br(),
      fluidRow(
        column(
          width = 6,
          div(class="note",
              htmlOutput("context_box")
          )
        ),
        column(
          width = 6,
          div(class="imgwrap",
              uiOutput("img_box")
          )
        )
      )
    )
  ),
  
  conditionalPanel(
    condition = "input.phenom == 'sound'",
    tags$hr(),
    fluidRow(
      column(
        width = 6,
        div(style="text-align:center;",
            tags$img(
              src = "onda1.png",
              style = "max-width:95%; max-height:500px; border:1px solid #ccc; border-radius:6px;"
            )
        )
      ),
      column(
        width = 6,
        div(style="text-align:center;",
            tags$img(
              src = "onda2.png",
              style = "max-width:95%; max-height:500px; border:1px solid #ccc; border-radius:6px;"
            )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Dados sintéticos
  gen_data <- reactive({
    x_full <- seq(0, 20, by = 0.01)
    
    if (input$phenom == "sound") {
      A0 <- 1.5; B0 <- 2.2; C0 <- 0.3
      y <- A0 * sin(B0 * x_full + C0) +
        0.25 * sin(2*B0 * x_full + C0/2) +
        rnorm(length(x_full), sd = 0.08)
      list(x = x_full, y = y)
      
    } else if (input$phenom == "pendulum") {
      A0 <- 1.2; B0 <- 1.6; C0 <- 0
      delta <- 0.08
      y <- A0 * cos(B0 * x_full + C0) * exp(-delta * x_full) +
        rnorm(length(x_full), sd = 0.05)
      list(x = x_full, y = y)
      
    } else { # ac
      A0 <- 2.0; B0 <- 3.2; C0 <- 0.6
      y <- A0 * sin(B0 * x_full + C0) +
        rnorm(length(x_full), sd = 0.05)
      list(x = x_full, y = y)
    }
  })
  
  observeEvent(input$reset, {
    if (input$phenom == "sound") {
      updateSliderInput(session, "A", value = 1.2)
      updateSliderInput(session, "B", value = 2.0)
      updateSliderInput(session, "C", value = 0.0)
      updateSliderInput(session, "range_x", value = c(0, 10))
    } else if (input$phenom == "pendulum") {
      updateSliderInput(session, "A", value = 1.0)
      updateSliderInput(session, "B", value = 1.5)
      updateSliderInput(session, "C", value = 0.0)
      updateSliderInput(session, "range_x", value = c(0, 10))
    } else {
      updateSliderInput(session, "A", value = 1.5)
      updateSliderInput(session, "B", value = 3.0)
      updateSliderInput(session, "C", value = 0.5)
      updateSliderInput(session, "range_x", value = c(0, 10))
    }
  }, ignoreInit = TRUE)
  
  output$eq_box <- renderUI({
    A <- input$A; B <- input$B; C <- input$C
    title <- switch(input$phenom,
                    "sound" = "Modelo de onda senoidal",
                    "pendulum" = "Modelo senoidal (aprox.) para pêndulo",
                    "ac" = "Modelo de corrente alternada")
    HTML(sprintf("<b>%s</b><br>y = <code>%s · sin(%s · x %+s)</code>",
                 title, fmt_num(A), fmt_num(B), fmt_num(C)))
  })
  
  output$context_box <- renderUI({
    if (input$phenom == "sound") {
      HTML("<b>Onda sonora:</b> pressão do ar oscila periodicamente. A amplitude está ligada ao volume percebido; B (ω) relaciona-se à frequência/pitch; C ajusta a fase.")
    } else if (input$phenom == "pendulum") {
      HTML("<b>Pêndulo simples:</b> para pequenas oscilações, o movimento é aproximadamente harmônico, com leve amortecimento.")
    } else {
      HTML("<b>Corrente alternada (AC):</b> corrente/tensão em um circuito AC ideal varia senoidalmente. A amplitude representa o valor de pico; B (ω) determina a frequência da rede; C é a fase.")
    }
  })
  
  output$img_box <- renderUI({
    url <- switch(input$phenom,
                  "sound"    = "https://cdn.pixabay.com/animation/2023/10/24/13/50/13-50-26-112_512.gif",
                  "pendulum" = "https://i.pinimg.com/originals/f9/c1/27/f9c127477be115888f23805f00f52d18.gif",
                  "ac"       = "https://blogger.googleusercontent.com/img/b/R29vZ2xl/AVvXsEjqs6RFQEiW21u4gwFNvpVo0vbQxhiOciy072qFHoitgCcAlzJb9jjdXXfRJWjkdre_kzLIA731qSwF2ScJwzm-Mm3LzhOKnnzizwJ97oL2ALm8peEn5I75Rt8INdORrOQnFstuMTkr9lO-/s1600/220px-Circular.Polarization.Circularly.Polarized.Light_Right.Handed.Animation.305x190.255Colors.gif")
    tags$img(src = url, alt = "ilustração do fenômeno")
  })
  
  # Plot
  output$plot <- renderPlotly({
    d <- gen_data()
    x_all <- d$x; y_all <- d$y
    rx <- input$range_x
    sel <- x_all >= rx[1] & x_all <= rx[2]
    x <- x_all[sel]; y_real <- y_all[sel]
    A <- input$A; B <- input$B; C <- input$C
    y_model <- A * sin(B * x + C)
    err <- rmse(y_real, y_model)
    
    step <- pi/2
    from <- floor(min(x)/step) * step
    to   <- ceiling(max(x)/step) * step
    x_ticks <- seq(from, to, by = step)
    x_labels <- label_pi(x_ticks)
    
    plot_ly() %>%
      add_markers(x = x, y = y_real, name = "Dados (real-sint.)",
                  marker = list(size = 5),
                  hovertemplate = "x=%{x:.2f}<br>y=%{y:.3f}<extra></extra>") %>%
      add_lines(x = x, y = y_model, name = "Modelo",
                line = list(color="red"),
                hovertemplate = "x=%{x:.2f}<br>ŷ=%{y:.3f}<extra></extra>") %>%
      layout(
        title = list(text = sprintf("Comparação: dados vs. modelo | RMSE = %s", fmt_num(err, 4))),
        xaxis = list(title = "x (tempo ou rad)", tickmode = "array",
                     tickvals = x_ticks, ticktext = x_labels),
        yaxis = list(title = "y"),
        margin = list(l = 60, r = 20, b = 60, t = 60),
        legend = list(orientation = "h", x = 0.02, y = 1.08)
      )
  })
}

shinyApp(ui = ui, server = server)
