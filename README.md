# WJPr

WJPr is an R package developed to streamline data analysis and visualization for the Data Analytics Unit at The World Justice Project (WJP). This package includes essential data and tools for replicating visualizations from WJP Country Reports and analyzing Rule of Law Index scores.

## Features

**Version 1.0.0** of WJPr offers:

- A wide range of visualization functions to recreate WJP Country Report charts, such as bar plots, line graphs, and radar charts.
- Access to Rule of Law Index scores data, including detailed information for all factors and subfactors.
- Streamlined tools for generating publication-ready graphics.

## Installation

WJPr is hosted on GitHub. To install the package, ensure you have the `devtools` package installed and use the following commands:

```R
# Install WJPr from GitHub
devtools::install_github("ctoruno/WJPr")
```

## Usage

Load the package into your R session:

```R
library(WJPr)
```

### Example: Accessing Rule of Law Index Data

The package provides built-in datasets for analysis:

```R
# View the first few rows of the dataset
head(WJPr::gpp)
```

### Example: Creating a Visualization

Here is an example of how to use WJPr to create a bar chart:

```R
# Always load the WJP fonts if not passing a custom theme to function
wjp_fonts()

# Loading data
gpp_data <- WJPr::gpp

# Prepare the data
data4bars <- gpp_data %>%
  select(country, year, q1a) %>%
  group_by(country, year) %>%
  mutate(
    q1a = as.double(q1a),
    trust = case_when(
      q1a <= 2  ~ 1,
      q1a <= 4  ~ 0,
      q1a == 99 ~ NA_real_
    ),
    year = as.character(year)
  ) %>%
  summarise(
    trust   = mean(trust, na.rm = TRUE),
    .groups = "keep"
  ) %>%
  mutate(
    trust = trust*100
  ) %>%
  filter(year == "2022")

# Draw the chart
wjp_bars(
    data4bars,              
    target    = "trust",        
    grouping  = "country",
    colors    = "year",
    cvec      = c("2022" = "#8789C0")
)
```

## Documentation

Comprehensive documentation is available for all functions and datasets. Use the R help system to access it:

```R
?WJPr::wjp_lines
```

## Contributing

Contributions are welcome! If you have suggestions, bug reports, or new feature ideas, please open an issue or submit a pull request on GitHub.

## License

This project is licensed under the MIT License. See the LICENSE.md file for details.

## Acknowledgments

WJPr was developed by the Data Analytics Unit at The World Justice Project. Special thanks to the team for their invaluable input in creating this package.

