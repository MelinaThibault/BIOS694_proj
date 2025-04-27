# Replication and Extension of Naess et al. (2024): Impact of Ambulance Busyness on Emergency Response Times

## Overview
This project replicates and extends the study by Naess et al., which assessed how ambulance business affects emergency response times in Central Norway between 2013 and 2022. Using emergency medical services (EMS) call data from Urgence-Santé (Montreal and Laval, Canada) from 2024, the replication adapts the original methodology to a different context and timeframe.

## Objectives
- Replicate the key findings of Naess et al. using independent EMS data.
- Validate the relationship between ambulance business scores and response time delays.
- Implement and evaluate a true neural network classifier for ambulance candidacy prediction.

## Methods
Data Source: 2024 Urgence-Santé acute emergency requests (Priorities 0 and 1), excluding cases with missing ambulance arrival times.

## Study Design

Identification of candidate ambulances based on proximity (within 1000 meters) and historical incident similarity (within 2 months).
Weighting of historical incidents by temporal proximity.
Calculation of busy probability score per incident.

## Modeling Approaches:

Original Method: Multinomial logistic regression (nnet package in R) for candidate ambulance prediction.
Improved Method: Feed-forward neural network (keras package in R).
Outcome Modeling: Fixed-effects linear regression (fixest package) to estimate the effect of busy ambulance probability on response times.

## Key Findings
Higher busy ambulance scores were associated with longer response times, replicating the original study’s qualitative findings.
Neural network models captured more complex spatial-temporal patterns, improving predictive performance compared to multinomial regression.
