# Portfolio Risk and Tail Dependency Analysis - AMD & NVIDIA

## Project Overview

This project focuses on the portfolio risk analysis of AMD and NVIDIA stocks. Using advanced statistical methods and copula models, the project assesses the risk exposure of a portfolio containing these two stocks by estimating Value at Risk (VaR) and Expected Shortfall (ES) at various confidence levels. The analysis includes investigating tail dependencies, which highlight how these stocks behave in extreme market conditions.

## Objectives

- To calculate and analyze risk metrics (VaR, ES) for AMD and NVIDIA stocks.
- To apply copula models for understanding the joint risk behavior of these two stocks, including tail dependencies.
- To use Monte Carlo simulations for simulating portfolio risk and assessing risk exposure under different market scenarios.
- To perform stress testing and analyze the risk associated with extreme market conditions.

## Methodology

The following techniques and models were used:

1. **Risk Metrics Calculation**: 
   - Value at Risk (VaR) and Expected Shortfall (ES) were computed using daily returns data of AMD and NVIDIA at different confidence levels (95%, 99%, 99.5%).

2. **Copula Models**: 
   - Gaussian, t-Copula, Comonotonic, and Countermonotonic copula models were applied to estimate joint risk factors between AMD and NVIDIA stocks, focusing on their tail dependencies.

3. **Monte Carlo Simulations**: 
   - Simulations were run to assess portfolio risk under various scenarios, providing a robust risk profile.

4. **Tail Dependency Analysis**: 
   - Kendall's Tau and Spearman's Rho were used to measure correlations between the loss distributions of AMD and NVIDIA, with an emphasis on extreme loss events.

5. **Stress Testing**: 
   - The analysis examined how these stocks behave during extreme market downturns using different copulas to simulate stressed market conditions.

## Results

- **Risk Metrics**: The computed VaR and ES for AMD and NVIDIA provided an understanding of the potential downside risk under normal and extreme market conditions.
- **Tail Dependence**: Copula models revealed the joint tail behavior of the two stocks, helping to quantify the risk exposure in extreme market scenarios.
- **Monte Carlo Simulations**: Simulations showed the potential portfolio loss distribution, highlighting how portfolio risk is distributed and under which conditions significant losses may occur.
- **Stress Testing**: Stress tests under different copula assumptions demonstrated the portfolio's vulnerability during extreme market events.

## Tools and Libraries

- **R Programming**: Utilized for statistical analysis, copula modeling, and Monte Carlo simulations.
- **Libraries**: `copula`, `quantmod`, `dplyr`, `ggplot2`, `reshape2`.

## Conclusion

This project demonstrates the application of quantitative risk management techniques, such as copula models, Monte Carlo simulations, and risk metrics, to assess the risk exposure of a portfolio containing AMD and NVIDIA stocks. The analysis helps in understanding joint risk behavior, particularly during extreme market conditions, and provides valuable insights for effective risk management.
