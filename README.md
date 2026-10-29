# Brent-crude-forecast-
# 🛢️ Brent Crude Running Weekly Forecast Dashboard

An automated time-series forecasting engine and dynamic interactive dashboard built with **R** and **Shiny**. This production application ingests weekly Brent crude oil prices from Federal Reserve Economic Data (FRED), fits rolling statistical and volatility models, maintains an append-only out-of-sample forecast ledger, and continuously evaluates real-time performance.

🚀 **Live Dashboard:** [https://rart2i-neel-nanavati.shinyapps.io/brent_project/](https://rart2i-neel-nanavati.shinyapps.io/brent_project/)

---

## 📌 Executive Summary

Energy commodities exhibit strong non-linear dynamics, structural breaks, and volatility clustering (periods of extreme movement followed by relative stability). Standard point forecasts often fail during geopolitical or supply shocks. 

This dashboard addresses these challenges by:
1. **Comparing Multiple Model Paradigms:** Evaluating benchmark naive models against state-space exponential smoothing, autoregressive frameworks, and GARCH conditional variance models.
2. **Quantifying Downside & Upside Tail Risk:** Modeling log-normal prediction intervals to evaluate tail-risk probabilities (e.g., price exceeding $\$130$/barrel in $H$ weeks).
3. **Out-of-Sample Scoreboarding:** Maintaining an immutable forecast log to score predictions against real-world market outcomes as new data arrives.

---

## 📈 Model Methodology

The dashboard estimates four dynamic statistical models every week over an 8-week horizon ($H = 8$) using natural log prices ($y_t = \ln(P_t)$) to maintain variance stability:

| Model | Specification & Purpose | Volatility Estimation |
| :--- | :--- | :--- |
| **Naive** | Baseline random-walk benchmark ($y_{t+h} = y_t$). Serves as the threshold for model value-add (`vs_naive`). | Constant historical standard deviation. |
| **ETS** | Automated Error-Trend-Seasonal state-space framework. Models underlying trends and smoothed price trajectories. | State-space innovation variance. |
| **ARIMA** | Automated Autoregressive Integrated Moving Average (`auto.arima`). Captures autocorrelation patterns in first differences. | Constant white-noise error variance $\sigma^2$. |
| **ARIMA-GARCH** | AR(1)-sGARCH(1,1) specification with Student’s $t$-distribution errors. | Dynamic time-varying conditional variance ($\sigma_t^2$). Captures volatility clustering. |

### Probability & Risk Calculations
Using log-transformed predictions $\ln(\hat{P}) \sim \mathcal{N}(\mu, \sigma^2)$, the risk calculator models price probability thresholds $K$:

$$P(P_{t+h} > K) = 1 - \Phi\left(\frac{\ln(K) - \mu}{\sigma}\right)$$

Users can dynamically adjust a **Volatility Multiplier** ($0.5\times - 2.0\times$) to stress-test scenarios under stormier or calmer market regimes.

---

## ⚙️ Architecture & Features
