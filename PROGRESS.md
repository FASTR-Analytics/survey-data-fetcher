# Multi-Fetch Session Enhancement - Progress Report

**Date**: 2025-10-02

## Project Goal
Enable users to:
1. Fetch multiple datasets in a single session (without overwriting)
2. Browse ALL indicators, not just favorites
3. Auto-generate standardized labels for non-favorite indicators
4. Visualize data from multiple sources together

---

## ✅ Phase 1: COMPLETED (UI & Data Structures)

### 1. Indicator Auto-Labeling System
**File**: `R/indicator_mappings.R` (NEW)
- ✅ Curated mappings for favorite indicators (DHS, MICS, WUENIC, UNWPP)
- ✅ Pattern-based auto-generation rules (e.g., "DTP3" → "penta3")
- ✅ Fallback generation from labels
- ✅ `get_or_generate_common_id()` main function
- ✅ `generate_indicator_common_id()` helper
- ✅ `add_common_ids()` batch processing

### 2. Data Structure Updates
**File**: `app.R`
- ✅ Added `fetch_collection` and `cleaned_collection` to `values`
- ✅ Added `next_dataset_id` for auto-incrementing IDs
- ✅ Sourced new `R/indicator_mappings.R` file
- ✅ Kept legacy `fetched_data` and `cleaned_data` for backward compatibility

### 3. Auto-Labeling Integration
**File**: `R/cleaning_functions.R`
- ✅ Updated `clean_dhs_data()` to use auto-generation for unmapped indicators
- ✅ Updated `clean_mics_data()` with fallback auto-labeling
- ✅ Updated `clean_mics_wuenic_data()` with auto-labeling support

### 4. Data Cart UI
**File**: `R/ui_components.R` - `create_results_tab()`
- ✅ Added "Session Data Cart" box showing all fetched datasets
- ✅ Cart table with dataset info (name, source, records, date)
- ✅ "Remove Selected" and "Clear All" buttons
- ✅ Cart summary display
- ✅ Download all cart data option
- ✅ Renamed existing boxes to "Latest Fetch Preview"

### 5. Browse Mode UI
**File**: `R/ui_components.R` - `create_indicator_selection_box()`
- ✅ Added indicator mode toggle: Favorites vs Browse
- ✅ Conditional panels for each mode
- ✅ Updated DHS, MICS, UNWPP to support both modes
- ✅ Warning message in Browse mode about full catalog

### 6. Add to Cart Option
**File**: `R/ui_components.R` - `create_fetch_data_box()`
- ✅ Added "Add to cart" checkbox (default: TRUE)
- ✅ Info message explaining accumulation vs replacement

---

## 🔨 Phase 2: TODO (Server Logic)

### 7. Fetch Event Handler (app.R)
**Status**: PENDING
**What needs to be done**:
```r
# In observeEvent(input$fetch_data, {...})
# After successful fetch:

if (input$add_to_cart) {
  # Generate dataset label
  dataset_label <- generate_dataset_label(
    source = input$data_source,
    indicators = input$indicators,
    countries = input$countries
  )

  # Add to collection
  values$fetch_collection[[values$next_dataset_id]] <- list(
    id = values$next_dataset_id,
    label = dataset_label,
    source = input$data_source,
    timestamp = Sys.time(),
    indicators = input$indicators,
    countries = input$countries,
    n_records = nrow(data),
    data = data
  )

  values$next_dataset_id <- values$next_dataset_id + 1
}

# Always update legacy storage for backward compatibility
values$fetched_data <- data
```

### 8. Cart Management Logic (app.R)
**Status**: PENDING
**What needs to be done**:
```r
# Render cart table
output$cart_table <- renderDT({
  # Convert fetch_collection to data frame
  # Show: ID, Label, Source, Records, Timestamp
})

# Cart summary
output$cart_summary <- renderUI({
  total_datasets <- length(values$fetch_collection)
  total_records <- sum(sapply(...))
  HTML(paste0(
    "<strong>", total_datasets, "</strong> datasets<br>",
    "<strong>", total_records, "</strong> total records"
  ))
})

# Remove selected
observeEvent(input$remove_selected_from_cart, {
  # Get selected rows from cart_table
  # Remove from fetch_collection
})

# Clear all
observeEvent(input$clear_cart, {
  values$fetch_collection <- list()
  values$next_dataset_id <- 1
})
```

### 9. Browse Mode Indicator Selector (app.R)
**Status**: PENDING
**What needs to be done**:
```r
# Update output$indicator_selector to detect mode
output$indicator_selector <- renderUI({
  if (input$indicator_mode == "browse") {
    # Show ALL indicators with search enabled
    pickerInput("indicators",
                "All Indicators:",
                choices = all_indicators,
                options = list(
                  `live-search` = TRUE,
                  `actions-box` = TRUE
                ),
                multiple = TRUE)
  } else {
    # Show only favorites (current behavior)
    # ...
  }
})
```

### 10. Dataset Selector in Visualization Tab
**Status**: PENDING
**File**: `R/ui_components.R` - `create_visualization_tab()`
**What needs to be done**:
```r
# Add before plot generation:
checkboxGroupInput("selected_datasets",
                   "Select datasets to visualize:",
                   choices = NULL)  # Updated dynamically

# In server:
observe({
  dataset_choices <- sapply(values$fetch_collection, function(d) d$label)
  updateCheckboxGroupInput(session, "selected_datasets",
                          choices = dataset_choices)
})
```

### 11. Combine Datasets Helper
**Status**: PENDING
**File**: `R/data_functions.R`
**What needs to be done**:
```r
combine_datasets <- function(dataset_list, dataset_ids = NULL) {
  # If dataset_ids specified, filter to those
  if (!is.null(dataset_ids)) {
    dataset_list <- dataset_list[dataset_ids]
  }

  # Extract data frames
  data_frames <- lapply(dataset_list, function(d) {
    d$data %>%
      mutate(
        dataset_id = d$id,
        dataset_label = d$label,
        dataset_source = d$source
      )
  })

  # Combine
  combined <- bind_rows(data_frames)
  return(combined)
}
```

### 12. Update Visualization Logic
**Status**: PENDING
**File**: `app.R` - visualization event handlers
**What needs to be done**:
```r
observeEvent(input$generate_plot, {
  # Get selected datasets
  selected_ids <- input$selected_datasets

  # Combine datasets
  combined_data <- combine_datasets(
    values$cleaned_collection,
    dataset_ids = selected_ids
  )

  # Update plot to differentiate by dataset_source
  # Add source to legend
  # Use different colors/shapes per source
})
```

### 13. Testing Checklist
**Status**: PENDING
- [ ] Test favorites mode (existing functionality)
- [ ] Test browse mode (shows all indicators)
- [ ] Test auto-labeling for non-favorite indicators
- [ ] Test add to cart (accumulate datasets)
- [ ] Test replace mode (uncheck add to cart)
- [ ] Test cart management (remove, clear)
- [ ] Test multi-source visualization (DHS + MICS + UNWPP)
- [ ] Test download cart data
- [ ] Test deployment to HuggingFace

---

## Files Modified

### New Files
- ✅ `R/indicator_mappings.R` - Auto-labeling system

### Modified Files
- ✅ `app.R` - Updated values structure, sourced new file
- ✅ `R/cleaning_functions.R` - Integrated auto-labeling
- ✅ `R/ui_components.R` - Data cart UI, browse mode, add to cart

### Files to Modify (Phase 2)
- 🔨 `app.R` - Server logic for cart management, browse mode, multi-dataset viz
- 🔨 `R/data_functions.R` - Add combine_datasets() helper
- 🔨 `R/ui_components.R` - Add dataset selector in viz tab

---

## Next Steps

1. **Test current UI changes locally**
   - Run `shiny::runApp()` in development
   - Verify new UI elements appear correctly
   - Check that existing functionality still works

2. **Implement Phase 2 server logic**
   - Start with cart management (view, remove, clear)
   - Then fetch event handler updates
   - Then browse mode support
   - Finally multi-dataset visualization

3. **Deploy and test**
   - Use `./deploy_to_hf.sh` to deploy
   - Test full workflow on HuggingFace

---

## Known Issues / Notes

- Legacy `fetched_data` and `cleaned_data` maintained for backward compatibility
- Browse mode UI is ready but needs server-side implementation
- Cart UI is ready but needs server-side implementation
- Auto-labeling is fully integrated and ready to use
- Some UNWPP cleaning functions not yet updated with auto-labeling (can be added later)

---

## Benefits Already Achieved (Phase 1)

Even without Phase 2 server logic:
- ✅ Auto-labeling system is ready for any indicator
- ✅ UI clearly shows new features to users
- ✅ Data structure supports collections
- ✅ Code is more modular and maintainable
- ✅ Foundation for multi-fetch sessions is complete
