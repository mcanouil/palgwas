options(stringsAsFactors = FALSE)
options(encoding = "UTF-8")

library(shiny)
library(shinydashboard)

library(parallel)
library(ggplot2)
library(grid)
library(scales)

myPalettes <- rbind(
  c(Name = "dodgerblue", Hex = "#1E90FF", RGB = "rgb(30/255, 144/255, 255/255)"),
  c(Name = "firebrick2", Hex = "#EE2C2C", RGB = "rgb(238/255, 44/255, 44/255)"),
  c(Name = "springgreen3", Hex = "#008B45", RGB = "rgb(0/255, 139/255, 69/255)"),
  c(Name = "maroon2", Hex = "#EE30A7", RGB = "rgb(238/255, 48/255, 167/255)"),
  c(Name = "goldenrod2", Hex = "#EEB422", RGB = "rgb(238/255, 180/255, 34/255)"),
  c(Name = "deepskyblue", Hex = "#00BFFF", RGB = "rgb(0/255, 191/255, 255/255)"),
  c(Name = "mediumpurple1", Hex = "#AB82FF", RGB = "rgb(171/255, 130/255, 255/255)"),
  c(Name = "tan1", Hex = "#FFA54F", RGB = "rgb(255/255, 165/255, 79/255)")
)
myPalette <- myPalettes[, "Name"]

asympLMM <- function(n = 1000, p = 0.3, m = 4, delta = 3, sig2 = 0.08, sig2_b1 = 0.0008, d = 0.005) {
  ## To compare empirical with theoretical power estimation for testing H1:Beta3 = d
  # A <- (12 * sig2) / (delta^2 * m * (m^2-1)) + sig2_b1
  # A <- 1/(delta * (m^2-1)) + sig2_b1/sig2
  measures <- (seq(m) - 1) * delta
  A <- 1 / sum((measures - mean(measures))^2) + sig2_b1 / sig2
  ncp.d <- (n * 2 * p * (1 - p) * d^2) / (sig2 * A)
  return(pchisq(qchisq(0.95, 1), 1, ncp.d, lower.tail = FALSE))
}

asympGEE <- function(n = 1000,
                     m = 4,
                     p = 0.3,
                     a = 0.9, # parameter of AR(1) process
                     tau1 = 0.08, # variance associated to AR(1)
                     tau2 = 0.07, # variance associated to local effect
                     delta = 3,
                     d = 0.005) {
  ## solve equation for b
  lamb <- tau2 / tau1

  # roots.b <- polyroot( c(lamb * a, -(1-a^2)-lamb * (1+a^2), lamb * a) )
  # b1 <- as.numeric(roots.b[1])
  # b2 <- as.numeric(roots.b[2])   # always equal to 1 / b1

  a.coeff <- lamb * a
  b.coeff <- -(1 - a^2) - lamb * (1 + a^2)
  c.coeff <- lamb * a
  disc <- b.coeff^2 - 4 * a.coeff * c.coeff
  b1 <- (-b.coeff - sqrt(disc)) / (2 * a.coeff)
  b2 <- (-b.coeff + sqrt(disc)) / (2 * a.coeff)


  covARMA <- function(m, a, b) {
    x <- matrix(0, m, m)
    gam0 <- ((1 - a^2) + (a - b)^2) / (1 - a^2)
    cst <- ((1 - a * b) * (a - b)) / (1 - a^2)
    for (i in 1:m) {
      for (j in 1:m) {
        if (abs(i - j) == 0) {
          x[i, j] <- gam0
        } else {
          x[i, j] <- cst * a^(abs(i - j) - 1)
        }
      }
    }
    return(x)
  }
  b <- b1
  covARMA.eq <- (tau1 * a * (1 - a^2)) / ((1 - a * b) * (a - b)) * covARMA(m, a, b) # var-cov of ARMA(1, 1) equivalent

  ## Computation of NCP for testing H1:Beta3 = d
  # A = 1' * Mn * 1 where Mn is inverse of variance-covariance matrix of ARMA(1, 1)
  cst <- (tau1 * a * (1 - a^2)) / ((1 - a * b) * (a - b))
  A <- m * ((1 - a) / (1 - b))^2 - (2 * (1 - a * b) * (1 - a) * (b - a) * (1 - b^m)) / ((1 - b)^3 * (1 - a * b + (b - a) * b^m))
  A <- A / cst
  # B = t' * Mn * 1
  B <- delta * ((m + 1) / 2) * A
  # C = t' * Mn * t
  C <- (1 / (1 - b)^4) *
    (
      (1 - a)^2 *
        (1 - b)^2 *
        m * (m + 1) *
        (2 * m + 1) / 6 -
        (1 - a) *
          (1 - b) *
          (b - a) *
          m *
          (m + 1) +
        (b - a)^2 *
          m
    ) +
    (
      (b - a) /
        (
          (1 - b)^4 *
            (
              (1 - a * b)^2 -
                (b - a)^2 *
                  b^(2 * m)
            )
        )
    ) *
      (
        (b + a) *
          (1 - a * b)^2 -
          2 *
            a *
            (1 - b^2) *
            (1 - a * b) *
            (1 + m * (1 - a)) *
            b^m -
          (b - a) *
            (
              b^2 *
                (1 - a^2) +
                (1 - b^2) *
                  (1 + m * (1 - a))^2
            ) *
            b^(2 * m)
      )
  C <- delta^2 * C / cst

  ncp.d <- n * 2 * p * (1 - p) * d^2 * ((A * C - B^2) / A)
  return(pchisq(qchisq(0.95, 1), 1, ncp.d, lower.tail = FALSE))
}


ui <- dashboardPage(
  dashboardHeader(
    title = HTML("Power Longitudinal"),
    dropdownMenuOutput("messageMenu")
  ),
  dashboardSidebar(
    sidebarUserPanel(
      name = a(tags$i(style = "color:#1995dc", icon("envelope", lib = "glyphicon")), "Ghislain Rocheleau", href = "mailto:ghislain.rocheleau@cnrs.fr"),
      subtitle = tags$span(style = "color:#1995dc", "(Principal Investigator)")
    ),
    sidebarUserPanel(
      name = a(tags$i(style = "color:#1995dc", icon("envelope", lib = "glyphicon")), "Mickaël Canouil", href = "mailto:mickael.canouil@cnrs.fr"),
      subtitle = tags$span(style = "color:#1995dc", "(Biostatistician)")
    ),
    hr(),
    sidebarMenu(
      menuItem(
        text = "Pictures Settings",
        tabName = "PicturesSettings",
        icon = tags$i(style = "color:#1995dc", icon("picture", lib = "glyphicon"))
      ),
      hr(),
      menuItem(
        text = "Home",
        tabName = "Home",
        icon = tags$i(style = "color:#1995dc", icon("import", lib = "glyphicon")),
        selected = TRUE
      ),
      hr()
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/theme.css")
    ),
    withMathJax(),
    tabItems(
      tabItem(
        tabName = "PicturesSettings",
        fluidRow(
          box(
            HTML(
              '<div id="plotunits" class="form-group shiny-input-radiogroup shiny-input-container shiny-input-container-inline">
                                <label class="control-label" for="plotunits">Unit:</label>
                                <div class="shiny-options-group">
                                    <label class="radio-inline">
                                        <input type="radio" name="plotunits" value="in" checked="checked"/>
                                        <span>inch</span>
                                    </label>
                                    <label class="radio-inline">
                                        <input type="radio" name="plotunits" value="cm"/>
                                        <span>centimetre</span>
                                    </label>
                                    <label class="radio-inline">
                                        <input type="radio" name="plotunits" value="mm"/>
                                        <span>millimetre</span>
                                    </label>
                                </div>
                            </div>
                            <div class="form-group shiny-input-container">
                                <label for="plotwidth">Width:</label>
                                <input id="plotwidth" type="number" class="form-control" value="7.5" min="1" max="100"/>
                            </div>
                            <div class="form-group shiny-input-container">
                                <label for="plotheight">Height:</label>
                                <input id="plotheight" type="number" class="form-control" value="6" min="1" max="100"/>
                            </div>
                            <div id="plotformat" class="form-group shiny-input-radiogroup shiny-input-container shiny-input-container-inline">
                                <label class="control-label" for="plotformat">Format:</label>
                                <div class="shiny-options-group">
                                    <label class="radio-inline">
                                        <input type="radio" name="plotformat" value="jpg" checked="checked"/>
                                        <span>jpg</span>
                                    </label>
                                    <label class="radio-inline">
                                        <input type="radio" name="plotformat" value="svg"/>
                                        <span>svg</span>
                                    </label>
                                    <label class="radio-inline">
                                        <input type="radio" name="plotformat" value="tiff"/>
                                        <span>tiff</span>
                                    </label>
                                    <label class="radio-inline">
                                        <input type="radio" name="plotformat" value="eps"/>
                                        <span>eps</span>
                                    </label>
                                    <label class="radio-inline">
                                        <input type="radio" name="plotformat" value="png"/>
                                        <span>png</span>
                                    </label>
                                </div>
                            </div>
                            <div id="plotdpi" class="form-group shiny-input-radiogroup shiny-input-container shiny-input-container-inline">
                                <label class="control-label" for="plotdpi">Resolution:</label>
                                <div class="shiny-options-group">
                                    <label class="radio-inline">
                                        <input type="radio" name="plotdpi" value="150"/>
                                        <span>150 dpi</span>
                                    </label>
                                    <label class="radio-inline">
                                        <input type="radio" name="plotdpi" value="300" checked="checked"/>
                                        <span>300 dpi</span>
                                    </label>
                                    <label class="radio-inline">
                                        <input type="radio" name="plotdpi" value="600"/>
                                        <span>600 dpi</span>
                                    </label>
                                    <label class="radio-inline">
                                        <input type="radio" name="plotdpi" value="1200"/>
                                        <span>1200 dpi</span>
                                    </label>
                                </div>
                            </div>
                            <div id="plotcolour" class="form-group shiny-input-radiogroup shiny-input-container shiny-input-container-inline">
                                <label class="control-label" for="plotcolour">Colour:</label>
                                <div class="shiny-options-group">
                                    <label class="radio-inline">
                                        <input type="radio" name="plotcolour" value="Colour"/>
                                        <span>Colour</span>
                                    </label>
                                    <label class="radio-inline">
                                        <input type="radio" name="plotcolour" value="Gray" checked="checked"/>
                                        <span>Gray</span>
                                    </label>
                                    <label class="radio-inline">
                                        <input type="radio" name="plotcolour" value="Custom"/>
                                        <span>Custom</span>
                                    </label>

                                </div>
                            </div>
                             <div id="reportstars" class="form-group shiny-input-radiogroup shiny-input-container shiny-input-container-inline">
                                <label class="control-label" for="reportstars">Significativity Stars:</label>
                                <div class="shiny-options-group">
                                    <label class="radio-inline">
                                        <input type="radio" name="reportstars" value="TRUE" checked="checked"/>
                                        <span>Yes</span>
                                    </label>
                                    <label class="radio-inline">
                                        <input type="radio" name="reportstars" value="FALSE"/>
                                        <span>No</span>
                                    </label>
                                </div>
                            </div>'
            ),
            width = 6,
            collapsible = FALSE,
            title = "Pictures Settings",
            solidHeader = TRUE,
            status = "info",
            height = 515
          ),
          htmlOutput("coloursUi")
        )
      ),
      tabItem(
        tabName = "Home",
        fluidRow(
          box(
            column(
              6,
              sliderInput("n", label = strong("Population size: \\( n \\)"), min = 500, max = 10000, value = 1000, step = 500, animate = animationOptions(interval = 300, loop = TRUE)),
              sliderInput("p", label = strong("Minor Allele Frequency: \\( p \\)"), min = 0, max = 0.5, step = 0.05, value = 0.3, animate = animationOptions(interval = 300, loop = TRUE)),
              sliderInput("m", label = strong("Number of measures: \\( m \\)"), min = 2, max = 15, value = 4, animate = animationOptions(interval = 300, loop = TRUE))
            ),
            column(
              6,
              sliderInput("delta", label = strong("Time between two measures: \\( \\delta \\)"), min = 1, max = 15, value = 3, animate = animationOptions(interval = 300, loop = TRUE)),
              sliderInput("beta3", label = strong("Interaction effect: \\( \\beta_{3} \\)"), min = 0, max = 1e-2, step = 5e-4, value = 5e-3, sep = "", animate = animationOptions(interval = 300, loop = TRUE))
            ),
            width = 12,
            collapsible = TRUE,
            title = "Parameters",
            solidHeader = TRUE,
            status = "info"
          ),
          box(
            fluidRow(
              column(
                6,
                tags$div(
                  style = "text-align:center",
                  radioButtons("whichModel",
                    label = strong("Model"),
                    choices = list(
                      "Linear Mixed Model (LMM)" = "lmm",
                      "Generalized Estimating Equations (GEE)" = "gee"
                    ),
                    selected = "lmm",
                    inline = TRUE
                  )
                )
              ),
              column(
                4,
                tags$div(
                  style = "text-align:center",
                  radioButtons("xaxis",
                    label = strong("Axis"),
                    choices = list(
                      "\\( n \\)" = "n",
                      "\\( p \\)" = "p",
                      "\\( m \\)" = "m",
                      "\\( \\delta \\)" = "delta",
                      "\\( \\beta_{3} \\)" = "beta3"
                    ),
                    selected = "n",
                    inline = TRUE
                  )
                )
              ),
              column(
                2,
                HTML(
                  '<div style="text-align:center; margin-top:10px">
                                        <a id="plot.download" class="btn btn-default shiny-download-link " href="" target="_blank">
                                            <i class="fa fa-download" style="text-align:center"></i>
                                            Download
                                        </a>
                                    </div>'
                )
              )
            ),
            plotOutput("plot"),
            width = 12,
            collapsible = FALSE,
            title = "Results",
            solidHeader = TRUE,
            status = "primary"
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  plot.data <- reactive({
    switch(input$whichModel,
      "lmm" = {
        asympPower <- switch(input$xaxis,
          "n" = {
            lapply(c(100, seq(500, 10000, 500)), function(x) {
              resLMM <- asympLMM(
                n = x,
                m = input$m,
                p = input$p,
                delta = input$delta,
                sig2 = 0.08, # input$sig2,
                sig2_b1 = 0.0008, # input$sig2_b1,
                d = input$beta3
              )
              cbind(x = x, y = resLMM)
            })
          },
          "p" = {
            lapply(c(0.005, seq(0.05, 0.45, 0.05)), function(x) {
              resLMM <- asympLMM(
                n = input$n,
                m = input$m,
                p = x,
                delta = input$delta,
                sig2 = 0.08, # input$sig2,
                sig2_b1 = 0.0008, # input$sig2_b1,
                d = input$beta3
              )
              cbind(x = x, y = resLMM)
            })
          },
          "m" = {
            lapply(seq(2, 10, 1), function(x) {
              resLMM <- asympLMM(
                n = input$n,
                m = x,
                p = input$p,
                delta = input$delta,
                sig2 = 0.08, # input$sig2,
                sig2_b1 = 0.0008, # input$sig2_b1,
                d = input$beta3
              )
              cbind(x = x, y = resLMM)
            })
          },
          "delta" = {
            lapply(seq(18), function(x) {
              resLMM <- asympLMM(
                n = input$n,
                m = input$m,
                p = input$p,
                delta = x,
                sig2 = 0.08, # input$sig2,
                sig2_b1 = 0.0008, # input$sig2_b1,
                d = input$beta3
              )
              cbind(x = x, y = resLMM)
            })
          },
          "beta3" = {
            lapply(seq(1e-5, 0.01, 0.001), function(x) {
              resLMM <- asympLMM(
                n = input$n,
                m = input$m,
                p = input$p,
                delta = input$delta,
                sig2 = 0.08, # input$sig2,
                sig2_b1 = 0.0008, # input$sig2_b1,
                d = x
              )
              cbind(x = x, y = resLMM)
            })
          }
        )
        labx <- switch(input$xaxis,
          "n" = {
            expression(n)
          },
          "p" = {
            expression(p)
          },
          "m" = {
            expression(m)
          },
          "delta" = {
            expression(delta)
          },
          "beta3" = {
            expression(beta[3])
          }
        )
        dta <- data.frame(do.call("rbind", asympPower))
        ggplot(data = dta, aes(x = x, y = y)) + theme_minimal(base_size = 16) +
          geom_line(colour = "dodgerblue", linetype = 1) +
          geom_point(colour = "dodgerblue", shape = 1) +
          labs(x = labx, y = "Power") +
          scale_y_continuous(limits = c(0, 1), label = percent)
      },
      "gee" = {
        asympPower <- switch(input$xaxis,
          "n" = {
            lapply(c(100, seq(500, 10000, 500)), function(x) {
              resGEE <- asympGEE(
                n = x,
                m = input$m,
                p = input$p,
                delta = input$delta,
                a = 0.9, # input$a,
                tau1 = 0.08, # input$tau1,
                tau2 = 0.07, # input$tau2,
                d = input$beta3
              )
              cbind(x = x, y = resGEE)
            })
          },
          "p" = {
            lapply(c(0.005, seq(0.05, 0.45, 0.05)), function(x) {
              resGEE <- asympGEE(
                n = input$n,
                m = input$m,
                p = x,
                delta = input$delta,
                a = 0.9, # input$a,
                tau1 = 0.08, # input$tau1,
                tau2 = 0.07, # input$tau2,
                d = input$beta3
              )
              cbind(x = x, y = resGEE)
            })
          },
          "m" = {
            lapply(seq(2, 10, 1), function(x) {
              resGEE <- asympGEE(
                n = input$n,
                m = x,
                p = input$p,
                delta = input$delta,
                a = 0.9, # input$a,
                tau1 = 0.08, # input$tau1,
                tau2 = 0.07, # input$tau2,
                d = input$beta3
              )
              cbind(x = x, y = resGEE)
            })
          },
          "delta" = {
            lapply(seq(18), function(x) {
              resGEE <- asympGEE(
                n = input$n,
                m = input$m,
                p = input$p,
                delta = x,
                a = 0.9, # input$a,
                tau1 = 0.08, # input$tau1,
                tau2 = 0.07, # input$tau2,
                d = input$beta3
              )
              cbind(x = x, y = resGEE)
            })
          },
          "beta3" = {
            lapply(seq(1e-5, 0.01, 0.001), function(x) {
              resGEE <- asympGEE(
                n = input$n,
                m = input$m,
                p = input$p,
                delta = input$delta,
                a = 0.9, # input$a,
                tau1 = 0.08, # input$tau1,
                tau2 = 0.07, # input$tau2,
                d = x
              )
              cbind(x = x, y = resGEE)
            })
          }
        )
        labx <- switch(input$xaxis,
          "n" = {
            expression(n)
          },
          "p" = {
            expression(p)
          },
          "m" = {
            expression(m)
          },
          "delta" = {
            expression(delta)
          },
          "beta3" = {
            expression(beta[3])
          }
        )
        dta <- data.frame(do.call("rbind", asympPower))
        ggplot(data = dta, aes(x = x, y = y)) + theme_minimal(base_size = 16) +
          geom_line(colour = "dodgerblue", linetype = 1) +
          geom_point(colour = "dodgerblue", shape = 1) +
          labs(x = labx, y = "Power") +
          scale_y_continuous(limits = c(0, 1), label = percent)
      }
    )
  })
  output$plot <- renderPlot({
    plot.data()
  })
  output$plot.download <- downloadHandler(
    filename = function() {
      paste0("Power.", input$plotformat)
    },
    content = function(file) {
      ggsave(file = file, plot = plot.data(), width = input$plotwidth, height = input$plotheight, units = input$plotunits, dpi = as.numeric(input$plotdpi))
    }
  )
}

shinyApp(ui = ui, server = server)
