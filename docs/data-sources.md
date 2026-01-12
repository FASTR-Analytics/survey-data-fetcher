# Data Sources

The Survey Data Fetcher connects to three major international health data APIs.

## DHS - Demographic and Health Surveys

The DHS Program provides nationally representative household survey data from over 90 countries.

### Connection

- **API**: DHS Program API (via `rdhs` package)
- **Documentation**: [api.dhsprogram.com](https://api.dhsprogram.com/)

### Available Data

- **Geographic Levels**: National and Subnational (regions/provinces)
- **Survey Types**: Standard DHS, MIS (Malaria), AIS (AIDS), SPA (Service Provision)

### Favorite Indicators

| Category | Indicators |
|----------|------------|
| **ANC & Maternal** | ANC1 (skilled provider), ANC4+, Institutional delivery, Postnatal care |
| **Vaccinations** | BCG, Penta1/2/3, Polio1/2/3, Measles1/2, Rotavirus, PCV |
| **IPTp** | IPTp1, IPTp2, IPTp3 |
| **Mortality** | Infant mortality, Under-5 mortality, Neonatal mortality |

### Example Indicator IDs

```
RH_ANCP_W_SKP  → ANC1 from skilled provider
CH_VACC_C_BCG  → BCG vaccination
CH_VACC_C_DP1  → DPT/Penta 1
CM_ECMR_C_IMR  → Infant mortality rate
```

---

## UNICEF - Multiple Data Sources

UNICEF provides data through SDMX web services, including MICS surveys, WUENIC estimates, and mortality data.

### Connection

- **API**: UNICEF SDMX API
- **Base URL**: `https://sdmx.data.unicef.org/ws/public/sdmxapi/rest`

### Available Datasets

| Dataset | Description |
|---------|-------------|
| **MICS** | Multiple Indicator Cluster Surveys |
| **WUENIC** | WHO/UNICEF Estimates of National Immunization Coverage |
| **UN IGME** | Inter-agency Group for Child Mortality Estimation |

### Favorite Indicators

| Category | Indicators |
|----------|------------|
| **Maternal Health** | ANC1, ANC4, Skilled birth attendance, C-section |
| **Child Immunization** | BCG, DTP1/3, Polio3, MCV1/2, Rotavirus, PCV3 |
| **Child Mortality** | Under-5 mortality, Infant mortality, Neonatal mortality |

### Example Indicator IDs

```
MNCH_ANC1     → At least one ANC visit
IM_DTP1       → DTP1 coverage (WUENIC)
CME_MRY0T4    → Under-5 mortality rate
```

---

## UNWPP - UN World Population Prospects

The UN Population Division provides demographic estimates and projections.

### Connection

- **API**: UN Data API
- **Documentation**: [population.un.org/dataportal](https://population.un.org/dataportal/)

### Available Data

- **Years**: 2000-2030 (estimates and projections)
- **Geographic Level**: National only

### Indicator Categories

| Category | Indicators |
|----------|------------|
| **Health & Mortality** | Infant mortality rate, Under-5 mortality, Life expectancy |
| **Demographics** | Total population, Crude birth rate, Total fertility rate |
| **Social Structure** | Women of reproductive age, Population growth rate |

### Example Indicator IDs (UNWPP codes)

```
22  → Infant mortality rate
49  → Total population
80  → Live births
52  → Women of reproductive age (15-49)
```

---

## Data Source Comparison

| Feature | DHS | UNICEF | UNWPP |
|---------|-----|--------|-------|
| Geographic level | National + Subnational | National | National |
| Data type | Survey (household) | Survey + Modeled | Modeled |
| Update frequency | Per survey | Annual | Every 2 years |
| Historical data | By survey year | Varies | 1950-2100 |
