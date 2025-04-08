library(shiny)

ui <- fluidPage(
  titlePanel("Black-Scholes Option PnL Simulator"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("S0", "Initial Spot Price (S₀)", 100),
      numericInput("K", "Strike Price (K)", 100),
      numericInput("T", "Time to Maturity (T, in years)", 1),
      numericInput("r", "Risk-Free Rate (r)", 0.05),
      numericInput("sigma", "Volatility (σ)", 0.2),
      numericInput("mu", "Expected Return (μ)", 0.05),
      numericInput("steps", "Simulation Steps", 100),
      selectInput("type", "Option Type", c("call", "put")),
      numericInput("spread", "Market Price Spread (%)", 2, min = 0, step = 0.1)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Price Path", plotOutput("pricePlot")),
        tabPanel("PnL", plotOutput("pnlPlot")),
        tabPanel("Greeks",
                 plotOutput("deltaPlot"),
                 plotOutput("gammaPlot"),
                 plotOutput("vegaPlot"),
                 plotOutput("thetaPlot"))
      )
    )
  )
)


