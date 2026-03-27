# Project Title

A research and engineering repository that contains the **code**, **Saudi diplomatic corpus**, and supporting materials for the project "From Riyadh to Cyberspace: KeyATM Analysis of Saudi Arabia’s Geopolitical Ventures in Digital Diplomacy".

## Overview

This repository keeps both the implementation and the dataset resources in one place so experiments can be reproduced more easily. It includes source code, corpus files, output folders for results.

## Repository Structure

```text
project-root/
├── README.md
├── corpus/
│   ├── all.csv
│   ├── news.csv
│   └── newsletter_report_statements.csv
|   └── preses_release.csv

├── keyATM/
│   ├── preprocessing/
│   ├── models/
│   ├── retrieval/
│   ├── routing/
│   ├── evaluation/
│   └── utils/
├── scripts/
│   ├── prepare_data.py
│   ├── train.py
│   ├── evaluate.py
│   └── run_pipeline.py
├── notebooks/
├── outputs/
│   ├── logs/
│   ├── figures/
│   └── results/
└── docs/
```

## Corpus

The corpus used in this project should be placed under the `corpus/` or `data/raw/` directory depending on your workflow.

### Corpus Contents

Typical contents may include:

- raw text documents
- PDF files
- JSON / JSONL records
- metadata files
- train / validation / test splits
- annotations or labels
