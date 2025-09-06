#Script 3 - Representações Gráficas

library(shiny)
library(plotly)
library(dplyr)

graus_para_rad <- function(g) g * pi / 180

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

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      #circle-wrap, #graph-wrap { position: relative; width: 100%; }
      #circle-plot {
        position: relative;
        width: min(100%, 55vmin);
        aspect-ratio: 1 / 1;
        margin: 0 auto;
      }
      #circle-plot > div { width: 100% !important; height: 100% !important; }

      @media (min-width: 992px) {
        .left-col  { padding-right: 12px; border-right: 1px solid #eee; }
        .right-col { padding-left: 12px; }
      }
    "))
  ),
  
  titlePanel("Ciclo Trigonométrico + Gráfico Sincronizados"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("modo", "Modo de exibição do ângulo:",
                   choices = c("Radianos" = "rad", "Graus" = "deg"),
                   selected = "rad"),
      
      radioButtons("func_type", "Função no gráfico da direita:",
                   choices = c("Seno" = "sin", "Cosseno" = "cos", "Tangente" = "tan", "Todas" = "all"),
                   selected = "sin"),
      
      uiOutput("angulo_ui"),
      
      tags$hr(),
      p("Controle da Animação:"),
      div(
        style = "display:flex; gap:10px; justify-content:center; flex-wrap:wrap;",
        actionButton("start", "Iniciar")
      ),
      
      sliderInput("velocidade", "Velocidade (rad por quadro):",
                  min = 0.02, max = 0.35, value = 0.08, step = 0.01),
      
      width = 3
    ),
    
    mainPanel(
      fluidRow(
        column(
          width = 5, class = "left-col",
          div(id = "circle-wrap",
              div(id = "circle-plot", plotlyOutput("cicloPlot", height = "100%", width = "100%"))
          ),
          br(),
          uiOutput("coordenadas")
        ),
        column(
          width = 7, class = "right-col",
          div(id = "graph-wrap",
              plotlyOutput("funcPlot", height = "500px")
          )
        )
      ),
      width = 9
    )
  )
)

server <- function(input, output, session) {
  running     <- reactiveVal(FALSE)  
  theta_total <- reactiveVal(0)      
  theta_mod   <- reactiveVal(0)     
  reset_tick  <- reactiveVal(0)      
  
  LIMITE_TOTAL <- 4*pi              

  output$angulo_ui <- renderUI({
    if (input$modo == "deg") {
      sliderInput("theta_display_deg", "Ângulo exibido (θ mod 360°):",
                  min = 0, max = 360, value = 0, step = 1)
    } else {
      sliderInput("theta_display_rad", "Ângulo exibido (θ mod 2π):",
                  min = 0, max = round(2*pi, 2), value = 0, step = 0.01)
    }
  })
  
  observeEvent(input$start, {
    if (theta_total() >= LIMITE_TOTAL || theta_total() == 0) {
      theta_total(0); theta_mod(0)
      if (input$modo == "deg") {
        updateSliderInput(session, "theta_display_deg", value = 0)
      } else {
        updateSliderInput(session, "theta_display_rad", value = 0)
      }
      reset_tick(reset_tick() + 1)
    }
    running(TRUE)
  })
  
  observeEvent(input$func_type, {
    if (running()) {
      running(FALSE)
      theta_total(0); theta_mod(0)
      if (input$modo == "deg") {
        updateSliderInput(session, "theta_display_deg", value = 0, min = 0, max = 360)
      } else {
        updateSliderInput(session, "theta_display_rad", value = 0, min = 0, max = round(2*pi, 2))
      }
      reset_tick(reset_tick() + 1)
    } else {
      theta_total(0); theta_mod(0)
      if (input$modo == "deg") {
        updateSliderInput(session, "theta_display_deg", value = 0)
      } else {
        updateSliderInput(session, "theta_display_rad", value = 0)
      }
      reset_tick(reset_tick() + 1)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$modo, {
    if (input$modo == "deg") {
      updateSliderInput(session, "theta_display_deg",
                        value = round((theta_mod() * 180/pi) %% 360),
                        min = 0, max = 360)
    } else {
      updateSliderInput(session, "theta_display_rad",
                        value = round(theta_mod(), 2),
                        min = 0, max = round(2*pi, 2))
    }
    reset_tick(reset_tick() + 1)
  }, ignoreInit = TRUE)
  
  observe({
    req(running())
    invalidateLater(35)  # ~28 fps
    
    isolate({
      step_rad <- input$velocidade
      novo     <- theta_total() + step_rad
      
      if (novo >= LIMITE_TOTAL) {
        theta_total(LIMITE_TOTAL)
        theta_mod(LIMITE_TOTAL %% (2*pi))
        running(FALSE)  # encerra ao fim das 2 voltas
      } else {
        theta_total(novo)
        theta_mod(novo %% (2*pi))
      }
      
      if (input$modo == "deg") {
        updateSliderInput(session, "theta_display_deg",
                          value = round((theta_mod() * 180/pi) %% 360))
      } else {
        updateSliderInput(session, "theta_display_rad",
                          value = round(theta_mod(), 2))
      }
    })
  })
  
  output$coordenadas <- renderUI({
    reset_tick()
    th <- theta_mod()
    s <- sin(th); c <- cos(th); t <- ifelse(c != 0, s/c, NA)
    
    titulo <- if (input$modo == "deg") {
      sprintf("θ (mod) = %d°", as.integer(round((th*180/pi) %% 360)))
    } else {
      sprintf("θ (mod) = %.2f rad", th)
    }
    
    linha <- function(cor, texto) {
      tags$div(
        tags$span(style = paste0(
          "display:inline-block;width:10px;height:10px;",
          "background-color:", cor, ";border:1px solid #444;margin-right:8px;"
        )),
        texto
      )
    }
    
    tags$div(
      style = "background:#f8f8f8;border:1px solid #ddd;border-radius:6px;padding:10px;display:inline-block;",
      tags$div(style="margin-bottom:6px; font-family:monospace;", titulo),
      linha("blue",   sprintf("sen(θ) = %.3f", s)),
      linha("red",    sprintf("cos(θ) = %.3f", c)),
      linha("green",  sprintf("tg(θ) ≈ %s", ifelse(is.na(t), "—", sprintf("%.3f", t))))
    )
  })
  
  output$cicloPlot <- renderPlotly({
    reset_tick()
    th <- theta_mod()
    s <- sin(th); c <- cos(th)
    t <- ifelse(c != 0, s/c, NA)
    
    circle <- data.frame(
      x = cos(seq(0, 2*pi, length.out = 360)),
      y = sin(seq(0, 2*pi, length.out = 360))
    )
    
    rotulo <- if (input$modo == "deg") {
      sprintf("θ = %d°", as.integer(round((th*180/pi) %% 360)))
    } else {
      sprintf("θ = %.2f rad", th)
    }
    
    plot_ly(type = "scatter", mode = "lines") %>%
      add_trace(data = circle, x = ~x, y = ~y, line = list(color = "gray")) %>%
      add_segments(x = 0, y = 0, xend = c, yend = s, line = list(color = "blue")) %>%
      add_segments(x = c, y = 0, xend = c, yend = s, line = list(color = "red",   dash = "dot")) %>%
      add_segments(x = 0, y = s, xend = c, yend = s, line = list(color = "green", dash = "dot")) %>%
      add_segments(x = 1, y = 0, xend = 1, yend = t, line = list(color = "orange", dash = "dash")) %>%
      add_markers(x = c, y = s, marker = list(size = 8, color = "black")) %>%
      add_text(x = c, y = s + 0.15, text = rotulo) %>%
      layout(
        shapes = list(
          list(type = "rect", x0 = 0,   x1 = 1.5, y0 = 0,   y1 = 1.5, fillcolor = "#b4ffe9", line = list(width = 0), opacity = 0.5),
          list(type = "rect", x0 = -1.5, x1 = 0,   y0 = 0,   y1 = 1.5, fillcolor = "#ffc58e", line = list(width = 0), opacity = 0.5),
          list(type = "rect", x0 = -1.5, x1 = 0,   y0 = -1.5,y1 = 0,   fillcolor = "#ffddfc", line = list(width = 0), opacity = 0.5),
          list(type = "rect", x0 = 0,   x1 = 1.5, y0 = -1.5,y1 = 0,   fillcolor = "#7097fd", line = list(width = 0), opacity = 0.5)
        ),
        xaxis = list(title = "cos(θ)", range = c(-1.5, 1.5), zeroline = FALSE),
        yaxis = list(title = "sen(θ)", range = c(-1.5, 1.5), zeroline = FALSE, scaleanchor = "x"),
        showlegend = FALSE,
        margin = list(l = 30, r = 20, b = 30, t = 30)
      )
  })
  
  output$funcPlot <- renderPlotly({
    reset_tick()
    
    th_total <- theta_total()  # 0 .. 4π
    th_mod   <- theta_mod()
    
    x_min <- 0; x_max <- 4*pi
    y_lim_sincos <- c(-3, 3)
    y_lim_tan    <- c(-3, 3)
    
    if (th_total <= 0) {
      if (input$func_type == "sin") {
        ylims <- y_lim_sincos; ytitle <- "sen(x)"; show_legend <- FALSE
      } else if (input$func_type == "cos") {
        ylims <- y_lim_sincos; ytitle <- "cos(x)"; show_legend <- FALSE
      } else if (input$func_type == "tan") {
        ylims <- y_lim_tan;    ytitle <- "tg(x)";  show_legend <- FALSE
      } else {
        ylims <- y_lim_sincos; ytitle <- "sen(x), cos(x), tg(x)"; show_legend <- FALSE
      }
      
      step <- pi/2
      x_ticks  <- seq(0, 4*pi, by = step)
      x_labels <- label_pi(x_ticks)
      
      return(
        plot_ly() %>%
          layout(
            title = "Gráfico no plano cartesiano",
            xaxis = list(
              title = "x (radianos)",
              range = c(x_min, x_max),
              tickmode = "array",
              tickvals = x_ticks,
              ticktext = x_labels
            ),
            yaxis = list(title = ytitle, range = ylims, dtick = 1),
            margin = list(l = 60, r = 20, b = 60, t = 40),
            showlegend = show_legend
          )
      )
    }
    
    xs <- seq(0, th_total, by = 0.01)
    df <- data.frame(
      x = xs,
      sin = sin(xs),
      cos = cos(xs),
      tan = tan(xs)
    )
    df$tan <- ifelse(abs(cos(df$x)) < 0.05, NA, df$tan)
    
    assim_x <- seq(-10*pi, 10*pi, by = pi/2)
    assim_x <- assim_x[round(cos(assim_x), 5) == 0 & assim_x >= x_min & assim_x <= x_max]
    
    if (input$func_type == "sin") {
      plt <- plot_ly(df, x = ~x, y = ~sin, type = "scatter", mode = "lines",
                     line = list(color = "blue"), name = "sen(x)")
      ylims <- y_lim_sincos; ytitle <- "sen(x)"; show_legend <- FALSE
      
    } else if (input$func_type == "cos") {
      plt <- plot_ly(df, x = ~x, y = ~cos, type = "scatter", mode = "lines",
                     line = list(color = "red"), name = "cos(x)")
      ylims <- y_lim_sincos; ytitle <- "cos(x)"; show_legend <- FALSE
      
    } else if (input$func_type == "tan") {
      plt <- plot_ly(df, x = ~x, y = ~tan, type = "scatter", mode = "lines",
                     line = list(color = "green"), name = "tg(x)")
      if (length(assim_x) > 0) {
        for (ax in assim_x) {
          plt <- add_segments(plt, x = ax, xend = ax, y = y_lim_tan[1], yend = y_lim_tan[2],
                              line = list(dash = "dot", color = "gray40"), showlegend = FALSE)
        }
      }
      ylims <- y_lim_tan; ytitle <- "tg(x)"; show_legend <- FALSE
      
    } else { 
      plt <- plot_ly()
      plt <- add_trace(plt, data = df, x = ~x, y = ~sin, type = "scatter", mode = "lines",
                       line = list(color = "blue"),  name = "sen(x)")
      plt <- add_trace(plt, data = df, x = ~x, y = ~cos, type = "scatter", mode = "lines",
                       line = list(color = "red"),   name = "cos(x)")
      plt <- add_trace(plt, data = df, x = ~x, y = ~tan, type = "scatter", mode = "lines",
                       line = list(color = "green"), name = "tg(x)")
      ylims <- y_lim_sincos; ytitle <- "sen(x), cos(x), tg(x)"; show_legend <- TRUE
    }
    
    step <- pi/2
    x_ticks  <- seq(0, 4*pi, by = step)
    x_labels <- label_pi(x_ticks)
    
    if (input$func_type == "all") {
      y_s <- sin(th_mod); y_c <- cos(th_mod)
      y_t <- ifelse(abs(cos(th_mod)) < 1e-6, NA, tan(th_mod))
      plt <- add_markers(plt, x = th_total, y = y_s, marker = list(size = 9, color = "blue"),
                         name = "sen(θ)", showlegend = FALSE)
      plt <- add_markers(plt, x = th_total, y = y_c, marker = list(size = 9, color = "red"),
                         name = "cos(θ)", showlegend = FALSE)
      if (!is.na(y_t)) {
        plt <- add_markers(plt, x = th_total, y = y_t, marker = list(size = 9, color = "green"),
                           name = "tg(θ)", showlegend = FALSE)
      }
      ann_text <- "θ_total"
    } else {
      y_at_now <- switch(input$func_type,
                         "sin" = sin(th_mod),
                         "cos" = cos(th_mod),
                         "tan" = ifelse(abs(cos(th_mod)) < 1e-6, NA, tan(th_mod)))
      plt <- add_markers(plt, x = th_total, y = y_at_now,
                         marker = list(size = 9, color = "black"),
                         name = "posição atual", showlegend = FALSE)
      ann_text <- sprintf("θ_total = %.2f rad", th_total)
    }
    
    plt <- layout(plt, shapes = list(
      list(type = "line", x0 = th_total, x1 = th_total,
           y0 = ylims[1], y1 = ylims[2],
           line = list(color = "black", dash = "dash"))
    ))
    
    plt %>%
      layout(
        title = "Gráfico no Plano Cartesiano",
        xaxis = list(
          title = "x (radianos)",
          range = c(x_min, x_max),
          tickmode = "array",
          tickvals = x_ticks,
          ticktext = x_labels
        ),
        yaxis = list(title = ytitle, range = ylims, dtick = 1),
        margin = list(l = 60, r = 20, b = 60, t = 40),
        showlegend = show_legend,
        annotations = list(
          list(x = th_total, y = ylims[2], yanchor = "bottom",
               text = ann_text, showarrow = FALSE)
        )
      )
  })
}

shinyApp(ui = ui, server = server)
