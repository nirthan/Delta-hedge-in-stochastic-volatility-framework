# ⚡ Delta Hedging in a Stochastic Volatility Framework

</div>

## 📘 Overview

This project investigates the pricing of European Call options, with a specific focus on AAPL. We compare two popular option pricing models:

- **Black & Scholes (B&S) Model** – assumes constant volatility.
- **Heston Model** – incorporates stochastic volatility, allowing more flexibility in capturing market patterns.

## 🎯 Objectives

- Compare the pricing accuracy of B&S and Heston models.
- Evaluate the impact of initial parameter guesses on Heston model calibration.
- Analyze the Greeks (∆, Γ, Θ, Vega) under both models.
- Test the effectiveness of Delta hedging in reducing portfolio risk.

## 🧪 Methods

- **Model Calibration**:
  - Used the Levenberg-Marquardt algorithm for parameter fitting in the Heston model.
  - Adjusted initial parameter guesses to minimize errors.
- **Evaluation Metrics**:
  - Mean Squared Error (MSE) for prices and implied volatility.
  - Mean Absolute Percentage Error (MAPE).
- **Greeks Analysis**:
  - Calculated ∆, Γ, Θ, and Vega to understand option sensitivity.
- **Hedging Effectiveness**:
  - Implemented ∆-hedging for AAPL options and measured portfolio variance and absolute hedging error.

## 📊 Key Findings

### Model Performance

| Metric       | Old Initial Values (Heston) | New Initial Values (Heston) | B&S Model |
|-------------|------------------|------------------|-----------|
| MSE Price    | 1.160            | 0.868            | 2.321     |
| MSE IV       | 0.0024           | 0.0016           | 0.0021    |
| MAPE Price   | 11.23%           | 9.07%            | 13.38%    |
| MAPE IV      | 10.96%           | 8.61%            | 9.78%     |

### Greeks Comparison
  - **Delta (∆)**: Options are more sensitive under Heston compared to B&S.
  - **Gamma (Γ)**: Differences between models shrink for larger maturities.
  - **Theta (Θ)**: Heston has larger absolute values, especially for long-dated options.

### Delta Hedging Effectiveness
  - Tested a call option with strike $140 and maturity Feb 17, 2023.
  - Observed ∆ changes when options transitioned ITM ↔ OTM.
  - Hedged portfolio variance:
    - Heston: 2.68
    - B&S: 3.69
  - Absolute hedging error:
    - Heston: 19.34
    - B&S: 23.27
### Conclusion

Heston provides better hedging performance due to its stochastic volatility framework, reducing both portfolio variance and hedging error compared to B&S.

## 🔍 Insights

- Initial parameter guesses strongly affect Heston calibration results.
- Heston captures the volatility smile and stochastic behavior more accurately than B&S.
- ∆ sensitivity is higher in Heston, which affects hedging strategies.
- Delta hedging reduces risk effectively, with Heston outperforming B&S in variance reduction and error metrics.

## 🌍 Implications

- Stochastic volatility models (like Heston) provide more realistic option pricing for trading and risk management.
- Careful calibration of model parameters is crucial for accurate option pricing and hedging.
- Delta hedging remains an effective risk mitigation tool, especially when model assumptions reflect market dynamics.
