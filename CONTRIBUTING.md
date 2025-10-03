# Contributing to MiteMapTools

Thank you for your interest in contributing to MiteMapTools! We welcome contributions from the community.

## How to Contribute

### Reporting Bugs

If you find a bug, please open an issue on GitHub with:
- A clear description of the problem
- Steps to reproduce the issue
- Your R version and MiteMapTools version
- Example data or code that demonstrates the issue (if possible)

### Suggesting Enhancements

We welcome suggestions for new features or improvements. Please open an issue on GitHub describing:
- The enhancement you'd like to see
- Why it would be useful
- Any implementation ideas you might have

### Code Contributions

We welcome pull requests! Here's how to contribute code:

1. **Fork the repository** and create a new branch for your feature or bug fix
2. **Write code** following the existing style and conventions
3. **Add tests** for new functionality using testthat
4. **Update documentation** using roxygen2 comments
5. **Run checks** to ensure your code passes all tests and R CMD check
6. **Submit a pull request** with a clear description of your changes

#### Code Style

- Follow the tidyverse style guide: https://style.tidyverse.org/
- Use meaningful variable and function names
- Add comments for complex logic
- Document all exported functions with roxygen2

#### Testing

- Add unit tests for new functions in `tests/testthat/`
- Ensure all existing tests still pass
- Test your changes with different data types and edge cases

#### Documentation

- Update function documentation with roxygen2 comments
- Update vignettes if adding major new features
- Update NEWS.md with a description of your changes

## Development Setup

To set up your development environment:

```r
# Install development dependencies
install.packages(c("devtools", "testthat", "roxygen2"))

# Clone your fork
# git clone https://github.com/YOUR_USERNAME/MiteMapTools.git

# Install package dependencies
devtools::install_dev_deps()

# Build and check the package
devtools::check()

# Run tests
devtools::test()
```

## Questions?

If you have questions about contributing, please open an issue on GitHub or contact the maintainers:
- Adrien Taudière: adrien.taudiere@zaclys.net
- Lise Roy: lise.roy@cefe.cnrs.fr

## Code of Conduct

Please note that this project is released with a Contributor Code of Conduct (see CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.
