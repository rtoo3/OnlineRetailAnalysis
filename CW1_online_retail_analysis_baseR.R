# Set the coursework folder and create a separate location for generated outputs.

PROJECT_DIR <- getwd()
OUTPUT_DIR <- file.path(PROJECT_DIR, "outputs")

if (!dir.exists(PROJECT_DIR)) {
  stop(
    "Coursework folder not found at:\n",
    PROJECT_DIR,
    "\n\nMove this script back into that folder or update PROJECT_DIR at the top of the script."
  )
}

# Find the raw CSV in the expected input folder, with a named-file fallback.
find_retail_csv <- function() {
  expected_path <- file.path(
    PROJECT_DIR,
    "input_data",
    "online_retail_2010_2011_raw.csv"
  )
  
  if (file.exists(expected_path)) {
    return(expected_path)
  }
  
  csv_files <- list.files(
    PROJECT_DIR,
    pattern = "\\.csv$",
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  )
  
  csv_files <- csv_files[
    !grepl("[/\\\\]outputs[/\\\\]", csv_files)
  ]
  
  retail_candidates <- csv_files[
    grepl(
      "online.*retail|retail.*online",
      basename(csv_files),
      ignore.case = TRUE
    )
  ]
  
  if (length(retail_candidates) == 1) {
    return(retail_candidates)
  }
  
  stop(
    "Raw Online Retail CSV not found. Put it here:\n",
    expected_path,
    "\n\nThe filename should be online_retail_2010_2011_raw.csv."
  )
}


create_output_dir <- function() {
  dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
}

write_output_csv <- function(data, filename) {
  write.csv(
    data,
    file = file.path(OUTPUT_DIR, filename),
    row.names = FALSE
  )
}

# Open a PNG device, draw the plot and close the device safely.
save_base_plot <- function(filename, plot_function,
                           width = 2800, height = 1800,
                           res = 300) {
  png(
    filename = file.path(OUTPUT_DIR, filename),
    width = width,
    height = height,
    res = res,
    pointsize = 14
  )
  on.exit(dev.off(), add = TRUE)
  plot_function()
}

format_count <- function(x) {
  formatC(
    round(x, 0),
    format = "f",
    digits = 0,
    big.mark = ","
  )
}

format_percentage <- function(x, digits = 0) {
  paste0(
    formatC(
      x,
      format = "f",
      digits = digits,
      big.mark = ","
    ),
    "%"
  )
}

# Wrap long category names so they remain readable on figures.
wrap_axis_labels <- function(x, width = 18) {
  vapply(
    x,
    function(label) {
      paste(
        strwrap(as.character(label), width = width),
        collapse = "\n"
      )
    },
    character(1)
  )
}

# Apply the same basic plotting settings across figures.
set_clean_plot_par <- function(
    mar = c(7, 7, 4.5, 2) + 0.1
) {
  par(
    mar = mar,
    las = 1,
    cex.axis = 0.95,
    cex.lab = 1.10,
    cex.main = 1.20,
    mgp = c(2.6, 0.8, 0)
  )
}

add_readable_axis <- function(
    side,
    upper_limit,
    formatter = format_count,
    n = 6
) {
  tick_values <- pretty(c(0, upper_limit), n = n)
  tick_values <- tick_values[
    tick_values >= 0 & tick_values <= upper_limit
  ]
  
  axis(
    side,
    at = tick_values,
    labels = formatter(tick_values),
    las = 1
  )
}


# Draw category labels manually when base R would otherwise suppress them.
add_manual_x_category_labels <- function(
    midpoints,
    labels,
    x_axis_title,
    label_cex = 0.85,
    label_offset = 0.05,
    title_line = 5.5
) {
  axis(
    side = 1,
    at = midpoints,
    labels = FALSE,
    tcl = -0.25
  )
  
  plot_limits <- par("usr")
  label_y <- plot_limits[3] -
    diff(plot_limits[3:4]) * label_offset
  
  text(
    x = midpoints,
    y = label_y,
    labels = labels,
    adj = c(0.5, 1),
    cex = label_cex,
    xpd = NA
  )
  
  mtext(
    x_axis_title,
    side = 1,
    line = title_line,
    cex = par("cex.lab")
  )
}


load_retail_data <- function(
    raw_csv_path = find_retail_csv()
) {
  if (!file.exists(raw_csv_path)) {
    stop(
      "Input CSV not found. Expected: ",
      raw_csv_path
    )
  }
  
  retail_raw <- read.csv(
    # Keep text fields as character data and recognise blank values as missing.
    raw_csv_path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = c("", "NA")
  )
  
  expected_columns <- c(
    "Invoice",
    "StockCode",
    # Confirm that all eight source variables are present.
    "Description",
    "Quantity",
    "InvoiceDate",
    "Price",
    "Customer ID",
    "Country"
  )
  
  missing_columns <- setdiff(
    expected_columns,
    names(retail_raw)
  )
  
  if (length(missing_columns) > 0) {
    stop(
      "Input CSV is missing expected columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }
  
  # Parse invoice dates and set the main columns to suitable data types.
  retail_raw$InvoiceDate <- as.POSIXct(
    retail_raw$InvoiceDate,
    format = "%Y-%m-%d %H:%M:%S",
    tz = "UTC"
  )
  
  retail_raw$Invoice <- as.character(retail_raw$Invoice)
  retail_raw$StockCode <- as.character(retail_raw$StockCode)
  retail_raw$Description <- as.character(retail_raw$Description)
  retail_raw$`Customer ID` <- as.character(
    retail_raw$`Customer ID`
  )
  retail_raw$Country <- as.character(retail_raw$Country)
  retail_raw$Quantity <- as.numeric(retail_raw$Quantity)
  retail_raw$Price <- as.numeric(retail_raw$Price)
  
  if (any(is.na(retail_raw$InvoiceDate))) {
    warning(
      "Some invoice dates could not be parsed. Check the input CSV."
    )
  }
  
  if (nrow(retail_raw) != 541910) {
    warning(
      "Expected 541,910 rows but loaded ",
      nrow(retail_raw),
      ". Check that the correct CSV was used."
    )
  }
  
  retail_raw
}


# Count the main raw-data issues. Categories can overlap.
audit_raw_data <- function(retail_raw) {
  total_rows <- nrow(retail_raw)
  
  raw_audit <- data.frame(
    issue = c(
      "Missing customer ID",
      "Missing description",
      "Repeated exact copy",
      "Cancellation line",
      "Negative quantity",
      "Zero quantity",
      "Zero or negative price"
    ),
    records = c(
      sum(is.na(retail_raw[["Customer ID"]])),
      sum(is.na(retail_raw$Description)),
      sum(duplicated(retail_raw)),
      sum(grepl("^C", retail_raw$Invoice), na.rm = TRUE),
      sum(retail_raw$Quantity < 0, na.rm = TRUE),
      sum(retail_raw$Quantity == 0, na.rm = TRUE),
      sum(retail_raw$Price <= 0, na.rm = TRUE)
    )
  )
  
  raw_audit$percentage <- round(
    raw_audit$records / total_rows * 100,
    2
  )
  
  # Check whether negative quantities also occur outside cancellation invoices.
  non_cancellation_negative <- retail_raw[
    retail_raw$Quantity < 0 &
      !grepl("^C", retail_raw$Invoice),
  ]
  
  raw_pattern_check <- data.frame(
    measure = c(
      "Zero-price rows",
      "Negative-price rows",
      "Zero-price rows with missing description",
      "Zero-price rows with missing customer ID",
      "Negative quantity rows outside cancellation invoices"
    ),
    records = c(
      sum(retail_raw$Price == 0, na.rm = TRUE),
      sum(retail_raw$Price < 0, na.rm = TRUE),
      sum(
        retail_raw$Price == 0 &
          is.na(retail_raw$Description),
        na.rm = TRUE
      ),
      sum(
        retail_raw$Price == 0 &
          is.na(retail_raw[["Customer ID"]]),
        na.rm = TRUE
      ),
      nrow(non_cancellation_negative)
    )
  )
  
  capture.output(
    # Save supporting structure, summary and audit outputs.
    str(retail_raw),
    file = file.path(OUTPUT_DIR, "raw_data_structure.txt")
  )
  
  capture.output(
    summary(retail_raw),
    file = file.path(OUTPUT_DIR, "raw_data_summary.txt")
  )
  
  write_output_csv(raw_audit, "raw_data_audit.csv")
  write_output_csv(raw_pattern_check, "raw_pattern_check.csv")
  
  list(
    raw_audit = raw_audit,
    raw_pattern_check = raw_pattern_check
  )
}

# Plot the percentage of raw transaction lines affected by each issue.
plot_raw_quality <- function(raw_audit) {
  save_base_plot(
    "01_raw_quality_indicators.png",
    function() {
      old_par <- par(no.readonly = TRUE)
      on.exit(par(old_par), add = TRUE)
      set_clean_plot_par(
        mar = c(11.5, 10.5, 4.5, 2) + 0.1
      )
      par(mgp = c(4.2, 0.8, 0))
      
      heights <- raw_audit$percentage
      y_limit <- max(heights) * 1.25
      category_labels <- wrap_axis_labels(
        raw_audit$issue,
        width = 11
      )
      
      mids <- barplot(
        heights,
        names.arg = rep("", length(heights)),
        axes = FALSE,
        ylim = c(0, y_limit),
        xlab = "",
        ylab = "Percentage of raw transaction lines affected",
        main = "Raw data-quality indicators",
        col = "grey80",
        border = "grey30"
      )
      
      add_readable_axis(
        side = 2,
        upper_limit = y_limit,
        formatter = function(x) {
          format_percentage(x, digits = 0)
        }
      )
      
      add_manual_x_category_labels(
        midpoints = mids,
        labels = category_labels,
        x_axis_title = "Data-quality indicator",
        label_cex = 0.78,
        label_offset = 0.07,
        title_line = 6.2
      )
      box(bty = "l")
      
      text(
        x = mids,
        y = heights,
        labels = format_percentage(heights, digits = 2),
        pos = 3,
        offset = 0.35,
        cex = 0.80
      )
    }
  )
}


# Add transparent flags first, then construct the cleaned dataset.
clean_retail_data <- function(retail_raw) {
  original_columns <- c(
    "Invoice",
    "StockCode",
    "Description",
    "Quantity",
    "InvoiceDate",
    "Price",
    "Customer ID",
    "Country"
  )
  
  retail_flagged <- retail_raw
  # Work on a copy so the original raw data remains unchanged.
  
  retail_flagged$CustomerID <- as.character(
    retail_flagged[["Customer ID"]]
  )
  
  retail_flagged$is_cancellation <- grepl(
    "^C",
    retail_flagged$Invoice
    # Create flags for cancellations, valid values, known customers and duplicates.
  )
  
  retail_flagged$has_positive_quantity <-
    !is.na(retail_flagged$Quantity) &
    retail_flagged$Quantity > 0
  
  retail_flagged$has_positive_price <-
    !is.na(retail_flagged$Price) &
    retail_flagged$Price > 0
  
  retail_flagged$has_customer_id <-
    !is.na(retail_flagged$CustomerID) &
    nzchar(retail_flagged$CustomerID)
  
  retail_flagged$is_exact_duplicate <- duplicated(
    retail_flagged[, original_columns]
  )
  
  retail_flagged$line_value <-
    retail_flagged$Quantity * retail_flagged$Price
  
  retail_flagged$record_type <- "administrative_or_invalid"
  
  sale_rows <-
    !retail_flagged$is_cancellation &
    retail_flagged$Quantity > 0 &
    retail_flagged$Price > 0
  
  cancellation_rows <-
    retail_flagged$is_cancellation &
    retail_flagged$Quantity < 0 &
    retail_flagged$Price > 0
  
  retail_flagged$record_type[sale_rows] <- "sale"
  retail_flagged$record_type[cancellation_rows] <- "cancellation"
  
  # Classify each row as a sale, cancellation or administrative/invalid record.
  commercial_rows <-
    retail_flagged$record_type != "administrative_or_invalid"
  
  keep_rows <-
    commercial_rows &
    !retail_flagged$is_exact_duplicate
  
  retail_clean <- data.frame(
    invoice = retail_flagged$Invoice[keep_rows],
    stock_code = retail_flagged$StockCode[keep_rows],
    description = retail_flagged$Description[keep_rows],
    quantity = retail_flagged$Quantity[keep_rows],
    invoice_date = retail_flagged$InvoiceDate[keep_rows],
    unit_price = retail_flagged$Price[keep_rows],
    customer_id = retail_flagged$CustomerID[keep_rows],
    country = retail_flagged$Country[keep_rows],
    transaction_type = retail_flagged$record_type[keep_rows],
    stringsAsFactors = FALSE
    # Retain commercial records and remove repeated exact copies.
  )
  
  retail_clean$line_value <-
    retail_clean$quantity * retail_clean$unit_price
  
  retail_clean$transaction_month <- as.Date(
    format(retail_clean$invoice_date, "%Y-%m-01")
  )
  
  retail_clean$has_customer_id <-
    # Build a clearly named cleaned dataset containing only required fields.
    !is.na(retail_clean$customer_id) &
    nzchar(retail_clean$customer_id)
  
  cleaning_summary <- data.frame(
    measure = c(
      "Raw rows",
      "Repeated exact duplicates removed",
      "Commercially interpretable rows before deduplication",
      "Final cleaned rows",
      "Final sale rows",
      "Final cancellation rows"
    ),
    records = c(
      nrow(retail_flagged),
      sum(retail_flagged$is_exact_duplicate),
      sum(commercial_rows),
      nrow(retail_clean),
      sum(retail_clean$transaction_type == "sale"),
      sum(retail_clean$transaction_type == "cancellation")
    )
  )
  
  final_checks <- data.frame(
    missing_descriptions = sum(is.na(retail_clean$description)),
    missing_customer_ids = sum(!retail_clean$has_customer_id),
    duplicate_rows_remaining = sum(
      duplicated(
        retail_clean[
          c(
            "invoice",
            # Record the effect of cleaning and verify the final result.
            "stock_code",
            "description",
            "quantity",
            "invoice_date",
            "unit_price",
            "customer_id",
            "country"
          )
        ]
      )
    ),
    zero_or_negative_prices_remaining = sum(
      retail_clean$unit_price <= 0
    )
  )
  
  retain_duplicates <- retail_flagged[commercial_rows, ]
  remove_duplicates <- retail_flagged[keep_rows, ]
  
  duplicate_comparison <- data.frame(
    approach = c(
      "Retain duplicate candidates",
      "Remove repeated exact copies"
    ),
    rows = c(
      nrow(retain_duplicates),
      # Compare the retained value before and after exact duplicate removal.
      nrow(remove_duplicates)
    ),
    net_line_value = c(
      sum(retain_duplicates$line_value, na.rm = TRUE),
      sum(remove_duplicates$line_value, na.rm = TRUE)
    )
  )
  
  write_output_csv(cleaning_summary, "cleaning_summary.csv")
  write_output_csv(final_checks, "final_cleaning_checks.csv")
  write_output_csv(
    duplicate_comparison,
    "duplicate_handling_comparison.csv"
  )
  
  list(
    retail_flagged = retail_flagged,
    retail_clean = retail_clean,
    cleaning_summary = cleaning_summary,
    final_checks = final_checks,
    duplicate_comparison = duplicate_comparison
  )
}

# Show how many transaction lines remain at each cleaning stage.
plot_cleaning_retention <- function(
    retail_raw,
    retail_flagged,
    retail_clean
) {
  retained_counts <- c(
    nrow(retail_raw),
    sum(
      retail_flagged$record_type !=
        "administrative_or_invalid"
    ),
    nrow(retail_clean)
  )
  
  save_base_plot(
    "02a_records_retained_after_cleaning.png",
    function() {
      old_par <- par(
        mar = c(8, 10, 4.5, 2) + 0.1
      )
      on.exit(par(old_par), add = TRUE)
      
      y_limit <- max(retained_counts) * 1.15
      
      y_ticks <- pretty(
        c(0, y_limit),
        n = 6
      )
      
      y_ticks <- y_ticks[
        y_ticks >= 0 &
          y_ticks <= y_limit
      ]
      
      mids <- barplot(
        retained_counts,
        names.arg = c(
          "Raw data",
          "Commercially\ninterpretable",
          "Final cleaned\ndata"
        ),
        ylim = c(0, y_limit),
        axes = FALSE,
        ylab = "",
        xlab = "",
        main = "Records retained through data cleaning",
        cex.names = 0.9,
        col = "grey80",
        border = "grey30"
      )
      
      axis(
        side = 2,
        at = y_ticks,
        labels = format_count(y_ticks),
        las = 1,
        cex.axis = 0.85
      )
      
      mtext(
        "Number of transaction lines",
        side = 2,
        line = 6.5,
        cex = 1.05
      )
      
      mtext(
        "Cleaning stage",
        side = 1,
        line = 5.2,
        cex = 1.05
      )
      
      text(
        x = mids,
        y = retained_counts,
        labels = format_count(retained_counts),
        pos = 3,
        cex = 0.85
      )
      
      box(bty = "l")
    }
  )
}

plot_before_after_quality <- function(
    raw_audit,
    final_checks
) {
  issue_labels <- c(
    "Missing descriptions",
    "Exact duplicate rows",
    "Zero or negative prices"
  )
  
  raw_counts <- c(
    raw_audit$records[
      raw_audit$issue == "Missing description"
    ],
    raw_audit$records[
      raw_audit$issue == "Repeated exact copy"
    ],
    raw_audit$records[
      raw_audit$issue == "Zero or negative price"
    ]
  )
  
  final_counts <- c(
    final_checks$missing_descriptions,
    final_checks$duplicate_rows_remaining,
    final_checks$zero_or_negative_prices_remaining
  )
  
  comparison <- rbind(
    "Raw data" = raw_counts,
    "Final cleaned data" = final_counts
  )
  
  save_base_plot(
    "02b_quality_before_after_cleaning.png",
    function() {
      old_par <- par(no.readonly = TRUE)
      on.exit(par(old_par), add = TRUE)
      set_clean_plot_par(
        mar = c(9.5, 11, 4.5, 2) + 0.1
      )
      
      y_limit <- max(comparison) * 1.20
      
      mids <- barplot(
        comparison,
        beside = TRUE,
        names.arg = wrap_axis_labels(
          issue_labels,
          width = 16
        ),
        axes = FALSE,
        ylim = c(0, y_limit),
        xlab = "Data-quality issue",
        ylab = "",
        main = "Data-quality issues before and after cleaning",
        col = c("grey65", "grey90"),
        border = "grey30",
        legend.text = rownames(comparison),
        args.legend = list(
          bty = "n",
          x = "topright"
        )
      )
      
      add_readable_axis(
        side = 2,
        upper_limit = y_limit
      )
      
      mtext(
        "Number of transaction lines",
        side = 2,
        line = 6.7,
        las = 0,
        cex = par("cex.lab")
      )
      
      text(
        x = mids,
        y = comparison,
        labels = format_count(comparison),
        pos = 3,
        offset = 0.35,
        cex = 0.78
      )
      
      box(bty = "l")
    }
  )
}


# Summarise cleaned transactions by month and country.
explore_transactions <- function(retail_clean) {
  sales_only <- retail_clean[
    retail_clean$transaction_type == "sale",
  ]
  
  identified_customer_sales <- sales_only[
    sales_only$has_customer_id,
  ]
  
  eda_overview <- data.frame(
    measure = c(
      "Cleaned commercial transaction lines",
      "Sale transaction lines",
      "Cancellation transaction lines",
      "Sale lines with customer ID",
      "Distinct countries",
      "Distinct stock codes",
      "Distinct identified customers"
    ),
    value = c(
      nrow(retail_clean),
      nrow(sales_only),
      sum(retail_clean$transaction_type == "cancellation"),
      nrow(identified_customer_sales),
      length(unique(retail_clean$country)),
      length(unique(retail_clean$stock_code)),
      length(unique(identified_customer_sales$customer_id))
    )
  )
  
  monthly_value <- aggregate(
    line_value ~ transaction_month,
    # Aggregate monthly value, invoice count and transaction-line count.
    data = retail_clean,
    FUN = sum
  )
  
  monthly_invoices <- aggregate(
    invoice ~ transaction_month,
    data = retail_clean,
    FUN = function(x) length(unique(x))
  )
  
  monthly_lines <- aggregate(
    invoice ~ transaction_month,
    data = retail_clean,
    FUN = length
  )
  
  names(monthly_value)[2] <- "net_transaction_value"
  names(monthly_invoices)[2] <- "commercial_invoice_count"
  names(monthly_lines)[2] <- "transaction_lines"
  
  monthly_summary <- merge(
    monthly_value,
    monthly_invoices,
    by = "transaction_month"
  )
  
  monthly_summary <- merge(
    monthly_summary,
    monthly_lines,
    by = "transaction_month"
  )
  
  monthly_summary <- monthly_summary[
    order(monthly_summary$transaction_month),
  ]
  
  country_value <- aggregate(
    line_value ~ country,
    data = retail_clean,
    FUN = sum
  )
  
  country_invoices <- aggregate(
    invoice ~ country,
    data = retail_clean,
    # Create the equivalent summary for each country.
    FUN = function(x) length(unique(x))
  )
  
  country_lines <- aggregate(
    invoice ~ country,
    data = retail_clean,
    FUN = length
  )
  
  names(country_value)[2] <- "net_transaction_value"
  names(country_invoices)[2] <- "commercial_invoice_count"
  names(country_lines)[2] <- "transaction_lines"
  
  country_summary <- merge(
    country_value,
    country_invoices,
    by = "country"
  )
  
  country_summary <- merge(
    country_summary,
    country_lines,
    by = "country"
  )
  
  country_summary <- country_summary[
    order(-country_summary$net_transaction_value),
  ]
  
  write_output_csv(eda_overview, "eda_overview.csv")
  write_output_csv(monthly_summary, "monthly_summary.csv")
  write_output_csv(country_summary, "country_summary.csv")
  
  list(
    sales_only = sales_only,
    identified_customer_sales = identified_customer_sales,
    eda_overview = eda_overview,
    monthly_summary = monthly_summary,
    country_summary = country_summary
  )
}

# Produce the monthly trend and country comparison figures.
plot_transaction_exploration <- function(
    monthly_summary,
    country_summary
) {
  save_base_plot(
    "03_monthly_net_transaction_value.png",
    function() {
      old_par <- par(
        mar = c(8.5, 12.5, 4.5, 2) + 0.1
      )
      on.exit(par(old_par), add = TRUE)
      
      values <- monthly_summary$net_transaction_value
      
      y_limit <- max(values) * 1.15
      
      y_ticks <- pretty(
        c(0, y_limit),
        n = 7
      )
      
      y_ticks <- y_ticks[
        y_ticks >= 0 &
          y_ticks <= y_limit
      ]
      
      plot(
        monthly_summary$transaction_month,
        values,
        type = "b",
        pch = 19,
        xaxt = "n",
        yaxt = "n",
        xlab = "",
        ylab = "",
        ylim = c(0, y_limit),
        main = "Monthly net transaction value after cleaning"
      )
      
      axis(
        side = 1,
        at = monthly_summary$transaction_month,
        labels = format(
          monthly_summary$transaction_month,
          "%b\n%Y"
        ),
        cex.axis = 0.85
      )
      
      axis(
        side = 2,
        at = y_ticks,
        labels = format_count(y_ticks),
        las = 1,
        cex.axis = 0.85
      )
      
      mtext(
        "Invoice month",
        side = 1,
        line = 5.2,
        cex = 1.05
      )
      
      mtext(
        "Net transaction value",
        side = 2,
        line = 8,
        cex = 1.05
      )
      
      box(bty = "l")
    }
  )
  
  top_countries <- country_summary[
    country_summary$country != "United Kingdom",
  ]
  
  top_countries <- head(top_countries, 10)
  
  save_base_plot(
    "04_top_countries_net_value.png",
    function() {
      old_par <- par(no.readonly = TRUE)
      on.exit(par(old_par), add = TRUE)
      set_clean_plot_par(
        mar = c(6, 18, 4.5, 2) + 0.1
      )
      par(mgp = c(5.0, 0.8, 0))
      
      values <- rev(top_countries$net_transaction_value)
      x_limit <- max(values) * 1.15
      
      barplot(
        values,
        names.arg = rev(top_countries$country),
        horiz = TRUE,
        axes = FALSE,
        xlim = c(0, x_limit),
        xlab = "Net transaction value",
        ylab = "Country",
        main = paste(
          "Top 10 countries by net transaction value",
          "United Kingdom excluded",
          sep = "\n"
        ),
        col = "grey80",
        border = "grey30",
        cex.names = 0.90
      )
      
      add_readable_axis(
        side = 1,
        upper_limit = x_limit
      )
      box(bty = "l")
    }
  )
}


# Build customer-level behavioural measures for known customers.
explore_customers <- function(retail_clean) {
  # Keep paid sales and cancellations separate while calculating customer measures.
  customer_sales <- retail_clean[
    retail_clean$has_customer_id &
      retail_clean$transaction_type == "sale",
  ]
  
  customer_cancellations <- retail_clean[
    retail_clean$has_customer_id &
      retail_clean$transaction_type == "cancellation",
  ]
  
  customer_ids <- sort(unique(customer_sales$customer_id))
  
  # Use vectorised group calculations instead of row-by-row loops.
  purchase_invoice_count <- tapply(
    customer_sales$invoice,
    customer_sales$customer_id,
    function(x) length(unique(x))
  )
  
  gross_spending <- tapply(
    customer_sales$line_value,
    customer_sales$customer_id,
    sum
  )
  
  gross_units <- tapply(
    customer_sales$quantity,
    customer_sales$customer_id,
    sum
  )
  
  product_diversity <- tapply(
    customer_sales$stock_code,
    customer_sales$customer_id,
    function(x) length(unique(x))
  )
  
  first_purchase_numeric <- tapply(
    as.numeric(customer_sales$invoice_date),
    customer_sales$customer_id,
    min
  )
  
  last_purchase_numeric <- tapply(
    as.numeric(customer_sales$invoice_date),
    customer_sales$customer_id,
    max
  )
  
  known_customer_rows <- retail_clean[
    retail_clean$has_customer_id,
  ]
  
  net_spending <- tapply(
    known_customer_rows$line_value,
    known_customer_rows$customer_id,
    sum
  )
  
  net_units <- tapply(
    known_customer_rows$quantity,
    known_customer_rows$customer_id,
    sum
  )
  
  cancellation_invoice_count <- tapply(
    customer_cancellations$invoice,
    customer_cancellations$customer_id,
    function(x) length(unique(x))
  )
  
  cancellation_invoice_count <- cancellation_invoice_count[
    customer_ids
  ]
  
  cancellation_invoice_count[
    is.na(cancellation_invoice_count)
  ] <- 0
  
  customer_summary <- data.frame(
    customer_id = customer_ids,
    first_purchase = as.POSIXct(
      first_purchase_numeric[customer_ids],
      origin = "1970-01-01",
      tz = "UTC"
    ),
    last_purchase = as.POSIXct(
      last_purchase_numeric[customer_ids],
      origin = "1970-01-01",
      tz = "UTC"
    ),
    purchase_invoice_count = as.numeric(
      purchase_invoice_count[customer_ids]
    ),
    gross_spending = as.numeric(
      gross_spending[customer_ids]
    ),
    gross_units = as.numeric(
      # Combine the customer measures into one customer-level table.
      gross_units[customer_ids]
    ),
    product_diversity = as.numeric(
      product_diversity[customer_ids]
    ),
    net_spending = as.numeric(
      net_spending[customer_ids]
    ),
    net_units = as.numeric(net_units[customer_ids]),
    cancellation_invoice_count = as.numeric(
      cancellation_invoice_count
    ),
    stringsAsFactors = FALSE
  )
  
  analysis_end_date <- max(retail_clean$invoice_date)
  
  customer_summary$recency_days <- as.numeric(
    difftime(
      analysis_end_date,
      customer_summary$last_purchase,
      units = "days"
    )
  )
  
  customer_analysis <- customer_summary[
    customer_summary$net_spending > 0,
  ]
  
  top_10_percent_n <- ceiling(
    nrow(customer_analysis) * 0.10
  )
  
  top_10_percent_spending <- sum(
    head(
      sort(
        customer_analysis$net_spending,
        decreasing = TRUE
        # Calculate recency from the final date in the cleaned dataset.
      ),
      top_10_percent_n
    )
  )
  
  top_10_percent_share <- top_10_percent_spending /
    sum(customer_analysis$net_spending) * 100
  
  customer_overview <- data.frame(
    measure = c(
      "Identified customers with purchases",
      "Customers with positive net spending",
      "Median purchase invoice count",
      # Use customers with positive net spending for the planned segmentation analysis.
      "Median net spending",
      "Top 10 percent spending share",
      "Analysis end date"
    ),
    value = c(
      nrow(customer_summary),
      nrow(customer_analysis),
      median(customer_analysis$purchase_invoice_count),
      round(median(customer_analysis$net_spending), 2),
      round(top_10_percent_share, 2),
      format(analysis_end_date, "%Y-%m-%d")
    ),
    stringsAsFactors = FALSE
  )
  
  rfm_summary <- data.frame(
    feature = c(
      "Recency days",
      "Purchase invoice count",
      "Net spending",
      "Product diversity"
    ),
    minimum = c(
      min(customer_analysis$recency_days),
      min(customer_analysis$purchase_invoice_count),
      min(customer_analysis$net_spending),
      min(customer_analysis$product_diversity)
    ),
    median = c(
      median(customer_analysis$recency_days),
      median(customer_analysis$purchase_invoice_count),
      median(customer_analysis$net_spending),
      # Summarise the four planned segmentation features.
      median(customer_analysis$product_diversity)
    ),
    mean = c(
      mean(customer_analysis$recency_days),
      mean(customer_analysis$purchase_invoice_count),
      mean(customer_analysis$net_spending),
      mean(customer_analysis$product_diversity)
    ),
    maximum = c(
      max(customer_analysis$recency_days),
      max(customer_analysis$purchase_invoice_count),
      max(customer_analysis$net_spending),
      max(customer_analysis$product_diversity)
    )
  )
  
  write_output_csv(customer_summary, "customer_summary.csv")
  write_output_csv(customer_overview, "customer_overview.csv")
  write_output_csv(rfm_summary, "rfm_summary.csv")
  
  list(
    customer_summary = customer_summary,
    customer_analysis = customer_analysis,
    customer_overview = customer_overview,
    rfm_summary = rfm_summary
  )
}

# Visualise customer spending and the highest-value customers.
plot_customer_exploration <- function(customer_analysis) {
  save_base_plot(
    "05_customer_spending_distribution.png",
    function() {
      old_par <- par(no.readonly = TRUE)
      on.exit(par(old_par), add = TRUE)
      set_clean_plot_par()
      
      log_spending <- log10(customer_analysis$net_spending)
      histogram_preview <- hist(
        log_spending,
        breaks = 50,
        plot = FALSE
      )
      y_limit <- max(histogram_preview$counts) * 1.12
      
      hist(
        log_spending,
        breaks = 50,
        yaxt = "n",
        ylim = c(0, y_limit),
        xlab = "Log10 of positive customer net spending",
        ylab = "Number of customers",
        main = "Distribution of positive customer net spending",
        col = "grey80",
        border = "grey30"
      )
      
      add_readable_axis(
        side = 2,
        upper_limit = y_limit
      )
      box(bty = "l")
    }
  )
  
  top_customers <- head(
    customer_analysis[
      order(
        -customer_analysis$net_spending
      ),
    ],
    10
  )
  
  save_base_plot(
    "06_top_customers_net_spending.png",
    function() {
      old_par <- par(no.readonly = TRUE)
      on.exit(par(old_par), add = TRUE)
      set_clean_plot_par(
        mar = c(6, 12, 4.5, 2) + 0.1
      )
      
      values <- rev(top_customers$net_spending)
      x_limit <- max(values) * 1.15
      
      barplot(
        values,
        names.arg = rev(top_customers$customer_id),
        horiz = TRUE,
        axes = FALSE,
        xlim = c(0, x_limit),
        xlab = "Net customer spending",
        ylab = "",
        main = "Top 10 customers by net spending",
        col = "grey80",
        border = "grey30",
        cex.names = 0.90
      )
      
      add_readable_axis(
        side = 1,
        upper_limit = x_limit
      )
      mtext(
        "Customer ID",
        side = 2,
        line = 6.5,
        las = 0,
        cex = par("cex.lab")
      )
      box(bty = "l")
    }
  )
}


# Aggregate sales value and units by stock-code/description combination.
explore_products <- function(retail_clean) {
  sales_only <- retail_clean[
    retail_clean$transaction_type == "sale",
  ]
  
  product_value <- aggregate(
    line_value ~ stock_code + description,
    data = sales_only,
    FUN = sum
  )
  
  product_units <- aggregate(
    quantity ~ stock_code + description,
    data = sales_only,
    FUN = sum
  )
  
  product_summary <- merge(
    product_value,
    product_units,
    by = c("stock_code", "description")
  )
  
  names(product_summary)[3] <- "gross_sales_value"
  names(product_summary)[4] <- "units_sold"
  
  product_summary <- product_summary[
    order(-product_summary$gross_sales_value),
  ]
  
  top_10_percent_n <- ceiling(
    nrow(product_summary) * 0.10
  )
  
  top_10_percent_share <- sum(
    head(
      product_summary$gross_sales_value,
      top_10_percent_n
    )
  ) / sum(product_summary$gross_sales_value) * 100
  
  product_overview <- data.frame(
    measure = c(
      "Distinct stock-code/description combinations with sales",
      "Top 10 percent combination sales share"
    ),
    value = c(
      nrow(product_summary),
      round(top_10_percent_share, 2)
    )
  )
  
  write_output_csv(product_summary, "product_summary.csv")
  write_output_csv(product_overview, "product_overview.csv")
  
  list(
    product_summary = product_summary,
    product_overview = product_overview
  )
}

# Plot the ten product combinations with the highest gross sales value.
plot_product_exploration <- function(product_summary) {
  top_products <- head(product_summary, 10)
  
  save_base_plot(
    "07_top_product_combinations_sales_value.png",
    function() {
      old_par <- par(no.readonly = TRUE)
      on.exit(par(old_par), add = TRUE)
      set_clean_plot_par(
        mar = c(7, 17, 4.5, 2) + 0.1
      )
      
      values <- rev(top_products$gross_sales_value)
      x_limit <- max(values) * 1.15
      
      barplot(
        values,
        names.arg = wrap_axis_labels(
          rev(top_products$description),
          width = 30
        ),
        horiz = TRUE,
        axes = FALSE,
        xlim = c(0, x_limit),
        xlab = "Gross sales value",
        ylab = "",
        main = "Top 10 stock-code/description combinations",
        col = "grey80",
        border = "grey30",
        cex.names = 0.75
      )
      
      add_readable_axis(
        side = 1,
        upper_limit = x_limit
      )
      mtext(
        "Product description",
        side = 2,
        line = 12,
        las = 0,
        cex = par("cex.lab")
      )
      box(bty = "l")
    },
    width = 3200,
    height = 2000
  )
}


# Return a logical flag for values outside the standard 1.5-IQR limits.
iqr_flag_function <- function(x) {
  lower_quartile <- quantile(x, 0.25, na.rm = TRUE)
  upper_quartile <- quantile(x, 0.75, na.rm = TRUE)
  interquartile_range <- IQR(x, na.rm = TRUE)
  
  x < lower_quartile - 1.5 * interquartile_range |
    x > upper_quartile + 1.5 * interquartile_range
}

# Analyse unusual combinations of quantity, price, value and purchase hour.
analyse_multivariate_outliers <- function(retail_clean) {
  sales_for_outliers <- retail_clean[
    retail_clean$transaction_type == "sale" &
      retail_clean$quantity > 0 &
      retail_clean$unit_price > 0 &
      retail_clean$line_value > 0,
  ]
  
  set.seed(110)
  
  # Use a reproducible sample to control runtime and memory use.
  sample_size <- min(
    50000,
    nrow(sales_for_outliers)
  )
  
  sample_rows <- sample(
    seq_len(nrow(sales_for_outliers)),
    size = sample_size,
    replace = FALSE
  )
  
  outlier_sample <- sales_for_outliers[sample_rows, ]
  
  outlier_features <- data.frame(
    log_quantity = log1p(outlier_sample$quantity),
    # Log-transform skewed monetary and quantity features before comparison.
    log_unit_price = log1p(outlier_sample$unit_price),
    log_line_value = log1p(outlier_sample$line_value),
    hour_of_day = as.integer(
      format(outlier_sample$invoice_date, "%H")
    )
  )
  
  outlier_matrix <- as.matrix(outlier_features)
  
  mahalanobis_distance <- mahalanobis(
    x = outlier_matrix,
    center = colMeans(outlier_matrix),
    cov = cov(outlier_matrix)
    # Calculate squared Mahalanobis distances and apply a 99% chi-square threshold.
  )
  
  mahalanobis_threshold <- qchisq(
    0.99,
    df = ncol(outlier_matrix)
  )
  
  mahalanobis_flag <-
    mahalanobis_distance > mahalanobis_threshold
  
  iqr_flag <- iqr_flag_function(
    outlier_features$log_quantity
  ) |
    iqr_flag_function(
      outlier_features$log_unit_price
      # Apply the IQR rule separately and compare agreement between the methods.
    ) |
    iqr_flag_function(
      outlier_features$log_line_value
    )
  
  outlier_results <- data.frame(
    invoice = outlier_sample$invoice,
    stock_code = outlier_sample$stock_code,
    description = outlier_sample$description,
    quantity = outlier_sample$quantity,
    unit_price = outlier_sample$unit_price,
    line_value = outlier_sample$line_value,
    hour_of_day = outlier_features$hour_of_day,
    mahalanobis_distance = mahalanobis_distance,
    mahalanobis_flag = mahalanobis_flag,
    iqr_flag = iqr_flag
  )
  
  outlier_comparison <- data.frame(
    method = c(
      "Univariate IQR rule",
      "Mahalanobis distance above chi-square 99% threshold"
    ),
    flagged_records = c(
      sum(iqr_flag),
      sum(mahalanobis_flag)
    ),
    percentage_of_sample = round(
      c(
        mean(iqr_flag) * 100,
        mean(mahalanobis_flag) * 100
      ),
      2
    )
  )
  
  outlier_agreement <- data.frame(
    measure = c(
      "Sample size",
      "Flagged by both methods",
      "Flagged only by IQR",
      "Flagged only by Mahalanobis distance"
    ),
    records = c(
      nrow(outlier_results),
      sum(iqr_flag & mahalanobis_flag),
      sum(iqr_flag & !mahalanobis_flag),
      sum(!iqr_flag & mahalanobis_flag)
    )
  )
  
  write_output_csv(
    outlier_comparison,
    "outlier_method_comparison.csv"
  )
  
  write_output_csv(
    outlier_agreement,
    "outlier_method_agreement.csv"
  )
  
  write_output_csv(
    outlier_results,
    "mahalanobis_sample_results.csv"
  )
  
  list(
    outlier_results = outlier_results,
    outlier_comparison = outlier_comparison,
    outlier_agreement = outlier_agreement,
    mahalanobis_threshold = mahalanobis_threshold
  )
}

plot_multivariate_outliers <- function(
    outlier_results,
    mahalanobis_threshold
) {
  save_base_plot(
    "08_mahalanobis_distance_distribution.png",
    function() {
      old_par <- par(no.readonly = TRUE)
      on.exit(par(old_par), add = TRUE)
      set_clean_plot_par(
        mar = c(7, 11, 4.5, 2) + 0.1
      )
      
      histogram_preview <- hist(
        outlier_results$mahalanobis_distance,
        breaks = 50,
        plot = FALSE
      )
      y_limit <- max(histogram_preview$counts) * 1.12
      
      hist(
        outlier_results$mahalanobis_distance,
        breaks = 50,
        yaxt = "n",
        ylim = c(0, y_limit),
        xlab = "Squared Mahalanobis distance",
        ylab = "",
        main = "Multivariate outlier-distance distribution",
        col = "grey80",
        border = "grey30"
      )
      
      add_readable_axis(
        side = 2,
        upper_limit = y_limit
      )
      mtext(
        "Number of sampled transaction lines",
        side = 2,
        line = 7.5,
        las = 0,
        cex = par("cex.lab")
      )
      box(bty = "l")
      
      abline(
        v = mahalanobis_threshold,
        lty = 2
      )
      
      legend(
        "topright",
        legend = "99% chi-square threshold",
        lty = 2,
        bty = "n"
      )
    }
  )
  
  set.seed(110)
  
  flagged_rows <- which(
    outlier_results$mahalanobis_flag
  )
  
  non_flagged_rows <- which(
    !outlier_results$mahalanobis_flag
  )
  
  selected_non_flagged_rows <- sample(
    non_flagged_rows,
    size = min(10000, length(non_flagged_rows)),
    replace = FALSE
  )
  
  plot_rows <- c(
    selected_non_flagged_rows,
    flagged_rows
  )
  
  plot_data <- outlier_results[plot_rows, ]
  
  save_base_plot(
    "09_mahalanobis_quantity_price.png",
    function() {
      old_par <- par(no.readonly = TRUE)
      on.exit(par(old_par), add = TRUE)
      set_clean_plot_par()
      
      plot(
        log10(plot_data$quantity),
        log10(plot_data$unit_price),
        pch = ifelse(
          plot_data$mahalanobis_flag,
          19,
          1
        ),
        cex = 0.55,
        xlab = "Log10 quantity per transaction line",
        ylab = "Log10 unit price",
        main = "Quantity and unit-price combinations",
        bty = "l"
      )
      
      legend(
        "topright",
        legend = c(
          "Not flagged",
          "Mahalanobis review flag"
        ),
        pch = c(1, 19),
        bty = "n"
      )
    }
  )
}


# Reproduce a stricter benchmark that removes unknown customers and returns.
compare_study_style_cleaning <- function(
    retail_raw,
    retail_clean
) {
  study_known_customer <- retail_raw[
    !is.na(retail_raw[["Customer ID"]]) &
      nzchar(as.character(retail_raw[["Customer ID"]])),
  ]
  
  study_positive_quantity <- study_known_customer[
    study_known_customer$Quantity > 0,
  ]
  
  study_style_baseline <- study_positive_quantity[
    !duplicated(study_positive_quantity),
  ]
  
  study_style_baseline$line_value <-
    study_style_baseline$Quantity *
    study_style_baseline$Price
  
  study_reproduction <- data.frame(
    step = c(
      "Raw data",
      "After blank customer IDs removed",
      "After non-positive quantities removed",
      "After exact duplicates removed"
    ),
    records = c(
      nrow(retail_raw),
      nrow(study_known_customer),
      nrow(study_positive_quantity),
      nrow(study_style_baseline)
    )
  )
  
  master_sales <- retail_clean[
    retail_clean$transaction_type == "sale",
  ]
  
  known_customer_sales <- master_sales[
    master_sales$has_customer_id,
  ]
  
  anonymous_sales <- master_sales[
    !master_sales$has_customer_id,
  ]
  # Compare the benchmark with the purpose-aware cleaned master dataset.
  
  master_cancellations <- retail_clean[
    retail_clean$transaction_type == "cancellation",
  ]
  
  known_customer_cancellations <- master_cancellations[
    master_cancellations$has_customer_id,
  ]
  
  comparison_summary <- data.frame(
    measure = c(
      "Commercial transaction lines retained",
      "Known-customer paid-sale lines available",
      "Valid anonymous paid-sale lines retained",
      "Cancellation lines retained separately",
      "Gross paid-sales value",
      "Net transaction value after cancellations",
      "Customer net spending can account for returns"
    ),
    published_study_style = c(
      nrow(study_style_baseline),
      nrow(study_style_baseline),
      0,
      0,
      round(
        sum(study_style_baseline$line_value),
        2
      ),
      round(
        sum(study_style_baseline$line_value),
        2
      ),
      "No"
    ),
    purpose_aware_pipeline = c(
      nrow(retail_clean),
      nrow(known_customer_sales),
      nrow(anonymous_sales),
      nrow(master_cancellations),
      round(sum(master_sales$line_value), 2),
      round(sum(retail_clean$line_value), 2),
      "Yes"
    ),
    stringsAsFactors = FALSE
  )
  
  retention_gain_percent <- round(
    (
      nrow(retail_clean) -
        nrow(study_style_baseline)
    ) /
      nrow(study_style_baseline) * 100,
    2
  )
  
  contribution_summary <- data.frame(
    measure = c(
      "Literature benchmark final lines",
      "Purpose-aware master dataset lines",
      "Additional retained lines",
      "Additional retained lines percentage",
      "Known-customer cancellations available for net spending"
    ),
    value = c(
      nrow(study_style_baseline),
      nrow(retail_clean),
      nrow(retail_clean) -
        nrow(study_style_baseline),
      retention_gain_percent,
      nrow(known_customer_cancellations)
    )
  )
  
  write_output_csv(
    study_reproduction,
    "section7_study_reproduction.csv"
  )
  
  write_output_csv(
    comparison_summary,
    "section7_literature_comparison.csv"
  )
  
  write_output_csv(
    contribution_summary,
    "section7_contribution_summary.csv"
  )
  
  list(
    study_reproduction = study_reproduction,
    comparison_summary = comparison_summary,
    contribution_summary = contribution_summary,
    study_style_baseline = study_style_baseline
  )
}

# Plot the number of records retained by the two preprocessing approaches.
plot_study_style_comparison <- function(
    study_style_baseline,
    retail_clean
) {
  retained_lines <- c(
    nrow(study_style_baseline),
    nrow(retail_clean)
  )
  
  save_base_plot(
    "10_section7_literature_comparison.png",
    function() {
      old_par <- par(no.readonly = TRUE)
      on.exit(par(old_par), add = TRUE)
      set_clean_plot_par(
        mar = c(9.5, 11, 4.5, 2) + 0.1
      )
      
      y_limit <- max(retained_lines) * 1.15
      category_labels <- c(
        "Study-style\nbenchmark",
        "Purpose-aware\nmaster dataset"
      )
      
      mids <- barplot(
        retained_lines,
        names.arg = rep("", length(retained_lines)),
        axes = FALSE,
        ylim = c(0, y_limit),
        xlab = "",
        ylab = "",
        main = "Transaction lines retained by preprocessing approach",
        col = "grey80",
        border = "grey30"
      )
      
      add_readable_axis(
        side = 2,
        upper_limit = y_limit
      )
      mtext(
        "Number of transaction lines retained",
        side = 2,
        line = 7.5,
        las = 0,
        cex = par("cex.lab")
      )
      
      add_manual_x_category_labels(
        midpoints = mids,
        labels = category_labels,
        x_axis_title = "Preprocessing approach",
        label_cex = 0.95,
        label_offset = 0.05,
        title_line = 5.5
      )
      box(bty = "l")
      
      text(
        x = mids,
        y = retained_lines,
        labels = format_count(retained_lines),
        pos = 3,
        offset = 0.35,
        cex = 0.90
      )
    }
  )
}


# Prepare the final tidy object without changing the analytical clean dataset.
create_final_tidy_data <- function(
    retail_clean,
    raw_csv_path = find_retail_csv()
) {
  retail_final <- retail_clean
  
  retail_final$customer_id_known <-
    retail_final$has_customer_id
  # Label anonymous customer IDs explicitly in the exported final object.
  
  retail_final$customer_id[
    !retail_final$customer_id_known
  ] <- "UNKNOWN"
  
  retail_final$has_customer_id <- NULL
  
  retail_final <- retail_final[
    c(
      "invoice",
      "stock_code",
      "description",
      "quantity",
      "unit_price",
      "line_value",
      "invoice_date",
      "transaction_month",
      "customer_id",
      "customer_id_known",
      "country",
      "transaction_type"
    )
  ]
  
  raw_csv_size_mb <- round(
    file.info(raw_csv_path)$size / 1024^2,
    2
  )
  
  final_validation <- data.frame(
    measure = c(
      # Complete final checks, including the coursework file-size limit.
      "Raw input CSV size in MB",
      "Final tidy dataset rows",
      "Final tidy dataset columns",
      "Missing values remaining",
      "Rows with UNKNOWN customer ID",
      "Zero or negative prices remaining",
      "Exact duplicates remaining"
    ),
    value = c(
      raw_csv_size_mb,
      nrow(retail_final),
      ncol(retail_final),
      sum(is.na(retail_final)),
      sum(retail_final$customer_id == "UNKNOWN"),
      sum(retail_final$unit_price <= 0),
      sum(duplicated(retail_final))
    )
  )
  
  if (raw_csv_size_mb > 100) {
    stop(
      "Input CSV exceeds the 100 MB coursework limit."
    )
  }
  
  write_output_csv(final_validation, "final_validation.csv")
  
  # Save the final dataset in R's compact RDS format.
  saveRDS(
    retail_final,
    file = file.path(
      OUTPUT_DIR,
      "retail_final_tidy.rds"
    )
  )
  
  list(
    retail_final = retail_final,
    final_validation = final_validation
  )
}


# Run every stage in the required order and record the total runtime.
run_coursework_workflow <- function() {
  start_time <- Sys.time()
  
  create_output_dir()
  
  retail_raw <- load_retail_data()
  
  raw_check <- audit_raw_data(retail_raw)
  plot_raw_quality(raw_check$raw_audit)
  
  cleaned <- clean_retail_data(retail_raw)
  plot_cleaning_retention(
    retail_raw,
    cleaned$retail_flagged,
    cleaned$retail_clean
  )
  
  plot_before_after_quality(
    raw_check$raw_audit,
    cleaned$final_checks
  )
  
  transaction_eda <- explore_transactions(
    cleaned$retail_clean
  )
  
  plot_transaction_exploration(
    transaction_eda$monthly_summary,
    transaction_eda$country_summary
  )
  
  customer_eda <- explore_customers(
    cleaned$retail_clean
  )
  
  plot_customer_exploration(
    customer_eda$customer_analysis
  )
  
  product_eda <- explore_products(
    cleaned$retail_clean
  )
  
  plot_product_exploration(
    product_eda$product_summary
  )
  
  outlier_analysis <- analyse_multivariate_outliers(
    cleaned$retail_clean
  )
  
  plot_multivariate_outliers(
    outlier_analysis$outlier_results,
    outlier_analysis$mahalanobis_threshold
  )
  
  literature_comparison <- compare_study_style_cleaning(
    retail_raw,
    cleaned$retail_clean
  )
  
  plot_study_style_comparison(
    literature_comparison$study_style_baseline,
    cleaned$retail_clean
  )
  
  final_data <- create_final_tidy_data(
    cleaned$retail_clean
  )
  
  elapsed_seconds <- round(
    as.numeric(
      difftime(
        Sys.time(),
        start_time,
        units = "secs"
      )
    ),
    2
  )
  
  runtime_summary <- data.frame(
    measure = "Full workflow runtime in seconds",
    value = elapsed_seconds
  )
  
  write_output_csv(runtime_summary, "runtime_summary.csv")
  
  cat("\nDSM110 base-R workflow completed successfully.\n\n")
  
  cat("Cleaning summary:\n")
  print(cleaned$cleaning_summary)
  
  cat("\nFinal cleaning checks:\n")
  print(cleaned$final_checks)
  
  cat("\nCustomer overview:\n")
  print(customer_eda$customer_overview)
  
  cat("\nMultivariate outlier comparison:\n")
  print(outlier_analysis$outlier_comparison)
  
  cat("\nMultivariate outlier agreement:\n")
  print(outlier_analysis$outlier_agreement)
  
  cat("\nStudy-style cleaning comparison:\n")
  print(literature_comparison$comparison_summary)
  
  cat("\nFinal validation:\n")
  print(final_data$final_validation)
  
  cat(
    "\nRuntime in seconds: ",
    elapsed_seconds,
    "\n",
    sep = ""
  )
  
  list(
    retail_raw = retail_raw,
    retail_flagged = cleaned$retail_flagged,
    retail_clean = cleaned$retail_clean,
    retail_final = final_data$retail_final,
    raw_check = raw_check,
    cleaned = cleaned,
    transaction_eda = transaction_eda,
    customer_eda = customer_eda,
    product_eda = product_eda,
    outlier_analysis = outlier_analysis,
    literature_comparison = literature_comparison,
    final_validation = final_data$final_validation,
    runtime_seconds = elapsed_seconds
  )
}


# Execute the full analysis.
results <- run_coursework_workflow()

retail_raw <- results$retail_raw
retail_clean <- results$retail_clean
retail_final <- results$retail_final
