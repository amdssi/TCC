# Script 4 — Transformações das Funções

library(shiny)
library(plotly)
library(ggplot2)
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

fmt_num <- function(x) {
  x <- ifelse(abs(x) < 1e-12, 0, x)
  sprintf("%.2f", x)
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      #square-plot {
        position: relative;
        width: min(100%, 95vmin);
        aspect-ratio: 16 / 10;
        margin: 0 auto;
      }
      #square-plot > div { width: 100% !important; height: 100% !important; }
      .panel-note { background:#f8f9fb; border:1px solid #e6e8ec; border-radius:8px; padding:10px; }
      .eqbox { background:#fff; border:1px dashed #cbd3df; border-radius:8px; padding:10px; margin-bottom:10px; }
      .btn-row { display:flex; gap:8px; justify-content:center; flex-wrap:wrap; margin-bottom:8px; }
    "))
  ),
  titlePanel("Transformações das Funções (A, B, C)"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("func_type", "Função:",
                   choices = c("Seno" = "sin", "Cosseno" = "cos", "Tangente" = "tan"),
                   selected = "sin"),
      
      sliderInput("A", "Amplitude (A):", min = -3, max = 3, value = 1, step = 0.1),
      sliderInput("B", "Frequência (B):", min = 0.1, max = 5, value = 1, step = 0.1),
      sliderInput("C", "Fase (C, em radianos):",
                  min = round(-2*pi, 2), max = round(2*pi, 2),
                  value = 0, step = 0.01),
      
      sliderInput("range_x", "Intervalo de x (radianos):",
                  min = round(-4*pi,1), max = round(4*pi,1),
                  value = round(c(-2*pi, 2*pi),1), step = 0.1),
      
      div(class="btn-row",
          actionButton("reset", "Restaurar padrão")
      ),
      
      hr(),
      div(class = "panel-note",
          HTML("<br>Transformação: <code>y = A · f(Bx + C)</code>.<br>
               Período: <code>2π/B</code> (sen/cos) ou <code>π/B</code> (tg).<br>
               Deslocamento: <code>φ = −C/B</code>.")
      ),
      width = 3
    ),
    
    mainPanel(
      fluidRow(
        column(
          width = 12,
          div(class = "eqbox",
              htmlOutput("equation_html")
          )
        )
      ),
      div(id="square-plot", plotlyOutput("plot", height = "100%", width = "100%"))
    )
  )
)

server <- function(input, output, session) {
  
  # Botão: Restaurar padrão
  observeEvent(input$reset, {
    updateSliderInput(session, "A", value = 1)
    updateSliderInput(session, "B", value = 1)
    updateSliderInput(session, "C", value = 0)
    updateSliderInput(session, "range_x", value = round(c(-2*pi, 2*pi), 1))
  })
  
  output$equation_html <- renderUI({
    f <- switch(input$func_type, "sin"="sin", "cos"="cos", "tan"="tan")
    A <- input$A; B <- input$B; C <- input$C
    period <- if (input$func_type == "tan") pi / B else (2*pi) / B
    shift  <- -C / B
    
    HTML(sprintf(
      "<div>
        <b>Função escolhida:</b> %s(x)<br>
        <b>Transformada:</b> y = <code>%s · %s(%s·x %+s)</code><br>
        <b>Período</b> T = <code>%s</code> rad &nbsp;|&nbsp; <b>Deslocamento</b> φ = <code>%s</code> rad
       </div>",
      f, fmt_num(A), f, fmt_num(B), fmt_num(C),
      fmt_num(period), fmt_num(shift)
    ))
  })
  
  output$plot <- renderPlotly({
    A <- input$A; B <- input$B; C <- input$C
    ftype <- input$func_type
    
    x_min <- input$range_x[1]; x_max <- input$range_x[2]
    x <- seq(x_min, x_max, by = 0.01)
    
    f_base <- switch(ftype,
                     "sin" = sin(x),
                     "cos" = cos(x),
                     "tan" = tan(x))
    arg <- B*x + C
    f_trans <- switch(ftype,
                      "sin" = A * sin(arg),
                      "cos" = A * cos(arg),
                      "tan" = A * tan(arg))
    
    df <- tibble(
      x = x,
      base = f_base,
      trans = f_trans,
      cos_arg = cos(arg)
    )
    
    show_asym <- FALSE
    if (ftype == "tan") {
      show_asym <- TRUE
      df <- df %>%
        mutate(trans = ifelse(abs(cos_arg) < 0.03, NA, trans))
    }
    
    if (ftype %in% c("sin", "cos")) {
      y_max <- max(1.1, abs(A)) * 1.2
      y_lim <- c(-y_max, y_max)
    } else {
      y_lim <- c(-5, 5)
      df <- df %>% mutate(base = ifelse(abs(cos(x)) < 0.03, NA, base))
    }
    
    p <- ggplot() +
      geom_hline(yintercept = 0, color="gray80") +
      geom_vline(xintercept = 0, color="gray80") +
      geom_line(data = df, aes(x = x, y = base), color = "gray60", linewidth = 0.8, linetype = "dashed") +
      geom_line(data = df, aes(x = x, y = trans),
                color = if (ftype=="sin") "blue" else if (ftype=="cos") "red" else "green",
                linewidth = 1.1, na.rm = TRUE) +
      coord_cartesian(xlim = c(x_min, x_max), ylim = y_lim) +
      labs(x = "x (radianos)", y = "y",
           title = "Transformações: y = A · f(Bx + C)",
           subtitle = "Curva padrão (cinza) vs. transformada (colorida)") +
      theme_minimal()
    
    if (show_asym) {
      k_vals <- seq(-50, 50)
      asym_x <- (pi/2 + k_vals*pi - C) / B
      asym_x <- asym_x[asym_x >= x_min & asym_x <= x_max]
      if (length(asym_x) > 0) {
        p <- p + geom_vline(xintercept = asym_x, color = "gray50", linetype = "dotted")
      }
    }
    
    step <- pi/2
    from <- floor(x_min/step) * step
    to   <- ceiling(x_max/step) * step
    x_ticks  <- seq(from, to, by = step)
    x_labels <- label_pi(x_ticks)
    
    ggplotly(p, tooltip = c("x","y")) %>%
      layout(
        xaxis = list(tickmode = "array", tickvals = x_ticks, ticktext = x_labels),
        yaxis = list(dtick = 1),
        margin = list(l = 60, r = 20, b = 60, t = 60),
        showlegend = FALSE
      )
  })
}

shinyApp(ui = ui, server = server)
