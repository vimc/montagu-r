# montagu

[![Travis build status](https://travis-ci.org/vimc/montagu-r.svg?branch=master)](https://travis-ci.org/vimc/montagu-r)

This is the R client for the montagu API.  In the first instance, both the core API and the reporting API will be included here; the reporting API will probably later be spun out into its own thing so that it can be used for with orderly outside of montagu (but we have no timeline for that).

Note that the package name is `montagu` but this repo is [`vimc/montagu-r`](https://github.com/vimc/montagu-r) to avoid colliding with the [main montagu repository](https://github.com/vimc/montagu).

## Implemented:

### Core api

- [ ] `/v1/authenticate/`
- [ ] `/v1/diseases/`
- [ ] `/v1/diseases/:id/`
- [ ] `/v1/touchstones/`
- [ ] `/v1/touchstones/:touchstone-id/scenarios/`
- [ ] `/v1/touchstones/:touchstone-id/scenarios/:scenario-id/`
- [ ] `/v1/touchstones/:touchstone-id/demographics/`
- [ ] `/v1/modelling-groups/`
- [ ] `/v1/modelling-groups/:group-id/`
- [ ] `/v1/modelling-groups/:group-id/responsibilities/:touchstone-id/`
- [ ] `/v1/modelling-groups/:group-id/responsibilities/:touchstone-id/:scenario-id/`
- [ ] `/v1/modelling-groups/:group-id/responsibilities/:touchstone-id/:scenario-id/coverage_sets/`
- [ ] `/v1/modelling-groups/:group-id/responsibilities/:touchstone-id/:scenario-id/coverage/`
- [ ] `/v1/modelling-groups/:group-id/responsibilities/:touchstone-id/:scenario-id/coverage/`
- [ ] `/v1/modelling-groups/:group-id/responsibilities/:touchstone-id/:scenario-id/coverage/get_onetime_link/`
- [ ] `/v1/users/:username/`
- [ ] `/v1/users/`
- [ ] `/v1/users/`
- [ ] `/v1/models/`
- [ ] `/v1/models/:id/`
- [ ] `/v1/onetime_link/:token/`

### Reporting API

- [x] `/v1/reports/`
- [x] `/v1/reports/:name/`
- [x] `/v1/reports/:name/:version/`
- [x] `/v1/reports/:name/:version/all/`
- [x] `/v1/reports/:name/:version/artefacts/`
- [x] `/v1/reports/:name/:version/artefacts/:artefact/`
- [x] `/v1/reports/:name/:version/resources/`
- [ ] `/v1/reports/:name/:version/resources/:resource/`
- [x] `/v1/reports/:name/:version/data/`
- [x] `/v1/reports/:name/:version/data/:data/`
- [x] `/v1/data/csv/:id/`
- [x] `/v1/data/rds/:id/`
- [ ] `/v1/onetime_token/`
- [ ] `/v1/`

## Installation

```
drat:::add("vimc")
install.packages("montagu")
```


## Testing


Set environment variables via `.Renviron`

``` r
MONTAGU_TEST_HOST="support.montagu.dide.ic.ac.uk"
MONTAGU_TEST_PORT="10443"
MONTAGU_TEST_USERNAME=<username> (test user is a good idea)
MONTAGU_TEST_PASSWORD=<password>
```
