# Indicator Reference

## Common Indicator IDs

The app maps various source-specific indicator IDs to standardized `indicator_common_id` values.

## Survey Indicators

These indicators are stored in `survey_data_unified.csv`.

### Maternal & Reproductive Health

| Common ID | Description | DHS ID | UNICEF ID |
|-----------|-------------|--------|-----------|
| `anc1` | At least 1 ANC visit | RH_ANCP_W_SKP | MNCH_ANC1 |
| `anc4` | At least 4 ANC visits | RH_ANCN_W_N4P | MNCH_ANC4 |
| `delivery` | Institutional delivery | RH_DELP_C_DHF | MNCH_INSTDEL |
| `csection` | C-section rate | RH_DELC_C_SEC | MNCH_CSEC |
| `pnc1` | Postnatal care (1st day) | RH_PNCP_W_2DY | - |
| `sba` | Skilled birth attendance | RH_DELP_C_SKP | MNCH_SAB |

### Child Immunization

| Common ID | Description | DHS ID | UNICEF ID | WUENIC |
|-----------|-------------|--------|-----------|--------|
| `bcg` | BCG vaccine | CH_VACC_C_BCG | IM_BCG | BCG |
| `penta1` | DTP/Penta 1st dose | CH_VACC_C_DP1 | IM_DTP1 | DTP1 |
| `penta2` | DTP/Penta 2nd dose | CH_VACC_C_DP2 | IM_DTP2 | DTP2 |
| `penta3` | DTP/Penta 3rd dose | CH_VACC_C_DP3 | IM_DTP3 | DTP3 |
| `polio1` | Polio 1st dose | CH_VACC_C_OP1 | IM_POL1 | Pol1 |
| `polio2` | Polio 2nd dose | CH_VACC_C_OP2 | IM_POL2 | Pol2 |
| `polio3` | Polio 3rd dose | CH_VACC_C_OP3 | IM_POL3 | Pol3 |
| `measles1` | Measles 1st dose | CH_VACC_C_MSL | IM_MCV1 | MCV1 |
| `measles2` | Measles 2nd dose | CH_VACC_C_MS2 | IM_MCV2 | MCV2 |
| `rotavirus1` | Rotavirus 1st dose | CH_VACC_C_RV1 | IM_ROTA1 | RotaC1 |
| `rotavirus2` | Rotavirus 2nd dose | - | IM_ROTA2 | RotaC2 |
| `pcv1` | Pneumococcal 1st dose | CH_VACC_C_PC1 | IM_PCV1 | PCV1 |
| `pcv2` | Pneumococcal 2nd dose | CH_VACC_C_PC2 | IM_PCV2 | PCV2 |
| `pcv3` | Pneumococcal 3rd dose | CH_VACC_C_PC3 | IM_PCV3 | PCV3 |
| `fully_immunized` | All basic vaccines | CH_VACC_C_BAS | - | - |

### Malaria Prevention

| Common ID | Description | DHS ID |
|-----------|-------------|--------|
| `iptp1` | IPTp 1st dose | ML_IPTP_W_SPF |
| `iptp2` | IPTp 2nd dose | ML_IPTP_W_2SP |
| `iptp3` | IPTp 3rd dose | ML_IPTP_W_3SP |

### Mortality Rates

| Common ID | Description | DHS ID | UNICEF ID | UNWPP |
|-----------|-------------|--------|-----------|-------|
| `imr` | Infant mortality rate | CM_ECMR_C_IMR | CME_MRY0 | 22 |
| `u5mr` | Under-5 mortality rate | CM_ECMR_C_U5M | CME_MRY0T4 | - |
| `nmr` | Neonatal mortality rate | CM_ECMR_C_NNR | CME_MRY0T27D | - |

### Other Health Indicators

| Common ID | Description | DHS ID |
|-----------|-------------|--------|
| `mcpr` | Modern contraceptive prevalence | FP_CUSA_W_MOD |
| `ebf` | Exclusive breastfeeding | NT_EBFR_C_BF |
| `vitamina` | Vitamin A supplementation | CN_MIAC_C_VAS |
| `deworming` | Deworming treatment | CN_MIAC_C_DWM |
| `total_fertility_rate` | Total fertility rate | FE_FRTR_W_TFR |
| `crude_birth_rate` | Crude birth rate | FE_FRTR_W_CBR |

---

## Population Indicators

These indicators are stored in `population_estimates_only.csv`.

| Common ID | Description | UNWPP Code |
|-----------|-------------|------------|
| `poptot` | Total population | 49 |
| `popu5` | Population under 5 | - |
| `totu1pop` | Annual population under 1 year | - |
| `totu5pop` | Annual population 0-4 years | - |
| `livebirth` | Live births per year | 80 |
| `womenrepage` | Women of reproductive age (15-49) | 52 |
| `popgrowth` | Population growth rate | - |

---

## Indicator Types

| Type | Description | Example |
|------|-------------|---------|
| `percent` | Percentage (stored as 0-1) | Coverage rates |
| `rate` | Rate per 1,000 | Mortality rates |
| `number` | Absolute count | Population |
| `population_estimate` | Population count | Total population |

---

## Auto-Generated IDs

For indicators not in the curated list, the app automatically generates a `indicator_common_id`:

1. **Pattern matching**: Looks for patterns like "DTP3", "Measles 1st"
2. **Label conversion**: Converts label to snake_case with source prefix

Example:
```
"New Vaccination Indicator" (from DHS) → dhs_new_vaccination_indicator
```
