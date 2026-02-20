# ContributorGuidelines

``` r
library(gsm.timez)
```

## Branches

The core branches that are used in this repository are:

- `main`: Contains the production version of the package (protected).
- `{issue#}`: Contains the code for a specific issue.

## Development Process

Describe the development step in a github issue and create a new branch
from `main` named with the issue number as a prefix.

Use Test-driven development (TDD) to develop the code. Write the test
first, then write the code to pass the test.

All github actions need to pass before merging the code to the `main`
branch.
