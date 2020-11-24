# cevi
This repository includes a body of scripts and data used to estimate our proposed Climate Extreme Vulnerability Index (CEVI). Estimation is based on the Alkire-Foster method (Alkire and Foster, 2011), combined with a newly proposed method on forward variance for the identification of the multidimensional deprivation cutoff.

# Repository content
In this respository you will find the following material:
* Script for the estimation of the CEVI index (<tt>Final Script.R</tt>);
* Script for the production of radar charts (<tt>Radar Charts.R</tt>)
* Script for the production of dimensional decompositions (<tt>Barplot with Multi Legend.R</tt>)
* Script for unidimensional cutoffs (<tt>Cutoffs.R</tt>)
* Script for the optimal identification of multidimensional cutoffs (sensitivity and dominance analysis) (<tt>CEVI Functions.R</tt>)
* Data on health, socioeconomic, infrastructure, and climate extreme variables (<tt>CEVI data.RData</tt>)

# How to cite

Please, when you use CEVI material give the appropriate credit to this repository at GitHub and the journal article that provides the methodology and formulas.

* **Traditional format**:

  - For the <tt>CEVI</tt> <tt>R</tt> material:\
  \
  Epopea Research Group (2020). *The estimation of a Climate Extreme Vulnerability Index (CEVI)*. Available at: <https://github.com/epopea/cevi.git>.
  
  - For the formulas and methodology that supports the cognitive affinity network approach:\
  \
    Andrade, L., Guedes, G., Noronha, K., Silva, C., Silva, P., Spyrides, M. H. (forthcoming). Health vulnerability related to climate extremes in Amazonia and the Northeast region of Brazil. *PlosOne*.
    
* **Bibtex format**:

  - For the <tt>CEVI</tt> <tt>R</tt> material:\
  \
  <code>@misc{epopea_2020,</code>\
  <code>author = {EPOPEA Research Group},</code>\
  <code>title = {The estimation of a Climate Extreme Vulnerability Index (CEVI)},</code>\
  <code>year = {2020},</code>\
  <code>publisher = {GitHub},</code>\
  <code>journal = {GitHub repository},</code>\
  <code>howpublished = {\url{https://github.com/epopea/cevi.git}},</code>\
  <code>commit = {38d4d72}</code>\
  <code>}</code>
  
  - For the formulas and methodology that support CEVI:\
  \
  <code>@article{andrade_2020_cevi,</code>\
  <code>title={Health vulnerability related to climate extremes in Amazonia and the Northeast region of Brazil},</code>\
  <code>author={Andrade, L. and Guedes, G. and Noronha, K. and Silva, C. and Silva, P. and Spyrides, M. H.},</code>\
  <code>journal={PlosOne},</code>\
  <code>volume={XX},</code>\
  <code>pages={XX-XX},</code>\
  <code>year={2020},</code>\
  <code>publisher={Public Library of Science}</code>\
  <code>}</code>

# References
* Alkire, S., & Foster, J. (2011). Counting and multidimensional poverty measurement. *Journal of public economics*, 95(7-8), 476-487.
