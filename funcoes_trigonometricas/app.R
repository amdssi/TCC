#Script 2 - Funções Trigonométricas

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

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      /* container quadrado, centralizado e responsivo */
      #square-plot {
        position: relative;
        width: min(100%, 90vmin);
        aspect-ratio: 1 / 1;
        margin: 0 auto; /* centraliza no mainPanel */
      }
      #square-plot > div {
        height: 100% !important;
        width: 100% !important;
      }
      /* centraliza conteúdo do mainPanel */
      .main-panel-center { text-align: center; }
      /* opcional: em telas muito pequenas, garante um mínimo confortável */
      @media (max-width: 480px) {
        #square-plot { width: 95vmin; }
      }
    "))
  ),
  
  titlePanel("Explorando Funções Trigonométricas: sen(x), cos(x) e tg(x)"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("func_type", "Escolha a função:",
                   choices = c("Seno" = "sin", "Cosseno" = "cos", "Tangente" = "tan", "Todas" = "all"),
                   selected = "sin"),
      sliderInput("range", "Intervalo de x (em radianos):",
                  min = round(-4*pi, 1),
                  max = round(4*pi, 1),
                  value = round(c(-2*pi, 2*pi), 1),
                  step = 0.1),
      hr(),
      helpText("Características:"),
      uiOutput("info_text"),
      width = 3   
    ),
    mainPanel(
      div(class = "main-panel-center",
          div(id = "square-plot", plotlyOutput("plot", height = "100%", width = "100%"))
      ),
      width = 9  
    )
  )
)

server <- function(input, output, session) {
  
  output$info_text <- renderUI({
    if (input$func_type == "sin") {
      HTML("<b>Função Seno:</b><br>Período: 2π<br>Amplitude: 1<br>Imagem: [-1, 1]<br>Zeros: x = nπ<br>Simetria: Ímpar")
    } else if (input$func_type == "cos") {
      HTML("<b>Função Cosseno:</b><br>Período: 2π<br>Amplitude: 1<br>Imagem: [-1, 1]<br>Zeros: x = π/2 + nπ<br>Simetria: Par")
    } else if (input$func_type == "tan") {
      HTML("<b>Função Tangente:</b><br>Período: π<br>Imagem: ℝ<br>Assíntotas: x = π/2 + nπ<br>Simetria: Ímpar")
    } else {
      HTML("<b>Visualização conjunta:</b><br>Compare período, amplitude, imagem e comportamento das funções seno (azul), cosseno (vermelho) e tangente (verde).")
    }
  })
  
  output$plot <- renderPlotly({
    x_vals <- seq(input$range[1], input$range[2], by = 0.01)
    
    df <- data.frame(
      x = x_vals,
      sin = sin(x_vals),
      cos = cos(x_vals),
      tan = tan(x_vals)
    )
    
    df <- df %>% mutate(tan = ifelse(abs(cos(x)) < 0.05, NA, tan))
    
    x_min <- input$range[1]; x_max <- input$range[2]
    assim_x <- seq(-10*pi, 10*pi, by = pi/2)
    assim_x <- assim_x[round(cos(assim_x), 2) == 0 & assim_x >= x_min & assim_x <= x_max]
    
    y_lim_sincos <- c(-1.5, 1.5)
    y_lim_tan    <- c(-3, 3)
    y_lim_all    <- c(-3, 3)
    
    p <- ggplot()
    if (input$func_type == "sin") {
      p <- p + geom_line(data = df, aes(x = x, y = sin), color = "blue") +
        coord_cartesian(xlim = c(x_min, x_max), ylim = y_lim_sincos)
      
    } else if (input$func_type == "cos") {
      p <- p + geom_line(data = df, aes(x = x, y = cos), color = "red") +
        coord_cartesian(xlim = c(x_min, x_max), ylim = y_lim_sincos)
      
    } else if (input$func_type == "tan") {
      p <- ggplot(df, aes(x = x, y = tan)) +
        geom_line(color = "green", na.rm = TRUE) +  
        geom_vline(xintercept = assim_x, linetype = "dotted", color = "gray40") +
        scale_y_continuous(breaks = seq(-3, 3, 1)) +
        coord_cartesian(xlim = c(x_min, x_max), ylim = y_lim_tan)
      
    } else {
      p <- p +
        geom_line(data = df, aes(x = x, y = sin), color = "blue") +
        geom_line(data = df, aes(x = x, y = cos), color = "red") +
        geom_line(data = df, aes(x = x, y = tan), color = "green", na.rm = TRUE) +
        geom_vline(xintercept = assim_x, linetype = "dotted", color = "gray40") +
        scale_y_continuous(breaks = seq(-3, 3, 1)) +
        coord_cartesian(xlim = c(x_min, x_max), ylim = y_lim_all)
    }
    
    p <- p + labs(x = "x (radianos)", y = "y", title = "Gráfico das Funções Trigonométricas") +
      theme_minimal()
  
    step <- pi/2
    from <- floor(x_min/step) * step
    to   <- ceiling(x_max/step) * step
    x_ticks  <- seq(from, to, by = step)
    x_labels <- label_pi(x_ticks)
    
    ggplotly(p) %>%
      layout(
        autosize = TRUE,  
        xaxis = list(
          constrain = "domain",
          tickmode  = "array",
          tickvals  = x_ticks,
          ticktext  = x_labels
        ),
        yaxis = list(
          scaleanchor = "x", scaleratio = 1,  
          constrain   = "domain",
          tickmode    = "linear",
          dtick       = 1
        ),
        margin = list(l = 60, r = 20, b = 60, t = 40)
      )
  })
}

shinyApp(ui = ui, server = server)
