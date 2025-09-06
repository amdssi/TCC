#Script 1 - Ciclo Trigonometrico

library(shiny)
library(plotly)

graus_para_rad <- function(g) g * pi / 180

ui <- fluidPage(
  titlePanel("Ciclo Trigonométrico"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("modo", "Modo de entrada do ângulo:",
                   choices = c("Radianos" = "rad", "Graus" = "deg"),
                   selected = "rad"),
      uiOutput("angulo_ui"),
      
      p("Controle da Animação:"),
      div(
        style = "display: flex; justify-content: center; gap: 10px; margin-bottom: 10px;",
        actionButton("start", "Iniciar"),
        actionButton("stop", "Parar")
      ),
      
      tags$div(
        style = "margin-top: 20px;",
        tags$strong("Legenda dos Quadrantes:"),
        tags$table(
          style = "width:100%; border-collapse: collapse;",
          tags$tr(
            tags$td(style = "background-color:#b4ffe9; padding:6px; border:1px solid #ccc;", 
                    "1º Quadrante: sen > 0, cos > 0"),
            tags$td(style = "background-color:#ffc58e; padding:6px; border:1px solid #ccc;", 
                    "2º Quadrante: sen > 0, cos < 0")
          ),
          tags$tr(
            tags$td(style = "background-color:#ffddfc; padding:6px; border:1px solid #ccc;", 
                    "3º Quadrante: sen < 0, cos < 0"),
            tags$td(style = "background-color:#7097fd; padding:6px; border:1px solid #ccc;", 
                    "4º Quadrante: sen < 0, cos > 0")
          )
        )
      )
    ),
    mainPanel(
      div(
        style = "width: 100%; max-width: 400px; margin: 0 auto;",
        plotlyOutput("cicloPlot", height = "400px")
      ),
      uiOutput("coordenadas") 
    )
  )
)

server <- function(input, output, session) {
  running <- reactiveVal(FALSE)
  theta_original <- reactiveVal(pi / 4)
  
  output$angulo_ui <- renderUI({
    if (input$modo == "deg") {
      sliderInput("theta", "Ângulo (θ):", min = 0, max = 360, value = 45, step = 1)
    } else {
      sliderInput("theta", "Ângulo (θ):",
                  min = 0,
                  max = round(2*pi, 2),
                  value = round(pi/4, 2),
                  step = 0.01)
    }
  })
  
  observeEvent(input$start, {
    theta_original(input$theta)
    running(TRUE)
  })
  
  observeEvent(input$stop, {
    running(FALSE)
    updateSliderInput(session, "theta", 
                      value = theta_original(),
                      min = if (input$modo == "deg") 0 else 0,
                      max = if (input$modo == "deg") 360 else round(2*pi, 2))
  })
  
  observe({
    req(running(), input$theta)
    invalidateLater(50)
    
    isolate({
      step   <- if (input$modo == "deg") 1 else 0.02
      limite <- if (input$modo == "deg") 360 else round(2*pi, 2)
      
      novo_valor <- input$theta + step
      if (novo_valor > limite) novo_valor <- 0
      updateSliderInput(session, "theta", value = novo_valor)
    })
  })
  
  output$coordenadas <- renderUI({
    req(input$theta)
    theta_rad <- if (input$modo == "deg") graus_para_rad(input$theta) else input$theta
    seno <- sin(theta_rad)
    coseno <- cos(theta_rad)
    tg <- ifelse(coseno != 0, seno/coseno, NA)
    
    titulo_theta <- if (input$modo == "deg") {
      sprintf("θ = %d°", as.integer(input$theta))
    } else {
      sprintf("θ = %.2f rad", theta_rad)
    }
    
    linha_legenda <- function(cor, texto) {
      tags$div(
        tags$span(style = paste0(
          "display:inline-block;width:10px;height:10px;",
          "background-color:", cor, ";",
          "border:1px solid #444;margin-right:8px;"
        )),
        texto
      )
    }
    
    tags$div(
      style = "background:#f8f8f8;border:1px solid #ddd;border-radius:6px;padding:10px;display:inline-block;",
      tags$div(style="margin-bottom:6px; font-family:monospace;", titulo_theta),
      linha_legenda("green",  sprintf("sen(θ) = %.3f", seno)),
      linha_legenda("red",    sprintf("cos(θ) = %.3f", coseno)),
      linha_legenda("orange", sprintf("tg(θ) ≈ %s", ifelse(is.na(tg), "—", sprintf("%.3f", tg))))
    )
  })
  
  output$cicloPlot <- renderPlotly({
    req(input$theta)
    theta <- if (input$modo == "deg") graus_para_rad(input$theta) else input$theta
    seno <- sin(theta)
    coseno <- cos(theta)
    tangente <- ifelse(coseno != 0, seno / coseno, NA)
    
    circle <- data.frame(
      x = cos(seq(0, 2*pi, length.out = 300)),
      y = sin(seq(0, 2*pi, length.out = 300))
    )
    
    rotulo_theta <- if (input$modo == "deg") {
      sprintf("θ = %d°", as.integer(input$theta))
    } else {
      sprintf("θ = %.2f rad", theta)
    }
    
    plot_ly(type = "scatter", mode = "lines") %>%
      add_trace(data = circle, x = ~x, y = ~y, line = list(color = "gray")) %>%
      add_segments(x = 0, y = 0, xend = coseno, yend = seno, line = list(color = "blue")) %>%
      add_segments(x = coseno, y = 0, xend = coseno, yend = seno, line = list(color = "red", dash = "dot")) %>%
      add_segments(x = 0, y = seno, xend = coseno, yend = seno, line = list(color = "green", dash = "dot")) %>%
      add_segments(x = 1, y = 0, xend = 1, yend = tangente, line = list(color = "orange", dash = "dash")) %>%
      add_markers(x = coseno, y = seno, marker = list(size = 8, color = "black")) %>%
      add_text(x = coseno, y = seno + 0.15, text = rotulo_theta) %>%
      layout(
        shapes = list(
          list(type = "rect", x0 = 0, x1 = 1.5, y0 = 0, y1 = 1.5, fillcolor = "#b4ffe9", line = list(width = 0), opacity = 0.5),
          list(type = "rect", x0 = -1.5, x1 = 0, y0 = 0, y1 = 1.5, fillcolor = "#ffc58e", line = list(width = 0), opacity = 0.5),
          list(type = "rect", x0 = -1.5, x1 = 0, y0 = -1.5, y1 = 0, fillcolor = "#ffddfc", line = list(width = 0), opacity = 0.5),
          list(type = "rect", x0 = 0, x1 = 1.5, y0 = -1.5, y1 = 0, fillcolor = "#7097fd", line = list(width = 0), opacity = 0.5)
        ),
        xaxis = list(title = "cos(θ)", range = c(-1.5, 1.5), zeroline = FALSE),
        yaxis = list(title = "sen(θ)", range = c(-1.5, 1.5), zeroline = FALSE, scaleanchor = "x"),
        showlegend = FALSE,
        title = "Ciclo Trigonométrico"
      )
  })
}

shinyApp(ui = ui, server = server)
