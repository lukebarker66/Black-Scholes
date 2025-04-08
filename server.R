
server <- function(input, output) {
  sim_data <- reactive({
    simulate_gbm(
      S0 = input$S0,
      mu = input$mu,
      sigma = input$sigma,
      T = input$T,
      steps = input$steps
    )
  })
  
  pnl_data <- reactive({
    calc_pnl(
      asset_path = sim_data(),
      K = input$K,
      r = input$r,
      sigma = input$sigma,
      T = input$T,
      type = input$type,
      market_price_spread = input$spread / 100
    )
  })
  
  output$pricePlot <- renderPlot({
    ggplot(sim_data(), aes(x = time, y = S)) +
      geom_line(color = "darkgreen") +
      labs(title = "Simulated Asset Price Path", y = "Price", x = "Time") +
      theme_minimal()
  })
  
  output$pnlPlot <- renderPlot({
    ggplot(pnl_data(), aes(x = time, y = pnl)) +
      geom_line(color = "steelblue") +
      labs(title = "Option PnL Over Time", y = "PnL (£)", x = "Time") +
      theme_minimal()
  })
  
  output$deltaPlot <- renderPlot({
    ggplot(pnl_data(), aes(x = time, y = delta)) +
      geom_line(color = "purple") +
      labs(title = "Delta", y = "Delta", x = "Time") +
      theme_minimal()
  })
  
  output$gammaPlot <- renderPlot({
    ggplot(pnl_data(), aes(x = time, y = gamma)) +
      geom_line(color = "red") +
      labs(title = "Gamma", y = "Gamma", x = "Time") +
      theme_minimal()
  })
  
  output$vegaPlot <- renderPlot({
    ggplot(pnl_data(), aes(x = time, y = vega)) +
      geom_line(color = "orange") +
      labs(title = "Vega", y = "Vega", x = "Time") +
      theme_minimal()
  })
  
  output$thetaPlot <- renderPlot({
    ggplot(pnl_data(), aes(x = time, y = theta)) +
      geom_line(color = "brown") +
      labs(title = "Theta", y = "Theta", x = "Time") +
      theme_minimal()
  })
}
