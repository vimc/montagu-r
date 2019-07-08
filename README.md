# montagu

[![Travis build status](https://travis-ci.org/vimc/montagu-r.svg?branch=master)](https://travis-ci.org/vimc/montagu-r)

This is the R client for the montagu API.  In the first instance, both the core API and the reporting API will be included here; the reporting API will probably later be spun out into its own thing so that it can be used for with orderly outside of montagu (but we have no timeline for that).

Note that the package name is `montagu` but this repo is [`vimc/montagu-r`](https://github.com/vimc/montagu-r) to avoid colliding with the [main montagu repository](https://github.com/vimc/montagu).

## Implemented:

### Core api

- [ ] `/v1/authenticate/` (POST)
- [x] `/v1/diseases/` (GET)
- [x] `/v1/diseases/:id/` (GET)
- [ ] `/v1/logout/` (GET)
- [x] `/v1/modelling-groups/` (GET)
- [ ] `/v1/modelling-groups/` (POST)
- [x] `/v1/modelling-groups/:group-id/` (GET)
- [ ] `/v1/modelling-groups/:group-id/actions/associate-memeber/` (POST)
- [x] `/v1/modelling-groups/:group-id/expectations/:touchstone-id/:expectation-id/` (GET)
- [x] `/v1/modelling-groups/:group-id/model-run-parameters/:touchstone-id/` (GET)
- [x] `/v1/modelling-groups/:group-id/model-run-parameters/:touchstone-id/` (POST)
- [x] `/v1/modelling-groups/:group-id/model-run-parameters/:touchstone-id/model-run-parameter-set-id/` (GET)
- [x] `/v1/modelling-groups/:group-id/responsibilities/` (GET)
- [x] `/v1/modelling-groups/:group-id/responsibilities/:touchstone-id/` (GET)
- [ ] `/v1/modelling-groups/:group-id/responsibilities/:touchstone-id/:scenario-id/` (GET)
- [x] `/v1/modelling-groups/:group-id/responsibilities/:touchstone-id/:scenario-id/coverage/` (GET)
- [ ] `/v1/modelling-groups/:group-id/responsibilities/:touchstone-id/:scenario-id/coverage/get_onetime_link/`
- [x] `/v1/modelling-groups/:group-id/responsibilities/:touchstone-id/:scenario-id/coverage_sets/` (GET)
- [x] `/v1/modelling-groups/:group-id/responsibilities/:touchstone-id/:scenario-id/estimate-sets/` (GET)
- [x] `/v1/modelling-groups/:group-id/responsibilities/:touchstone-id/:scenario-id/estimate-sets/` (POST)
- [x] `/v1/modelling-groups/:group-id/responsibilities/:touchstone-id/:scenario-id/estimate-sets/:estimate-set-id/` (GET)
- [x] `/v1/modelling-groups/:group-id/responsibilities/:touchstone-id/:scenario-id/estimate-sets/:estimate-set-id/` (POST)
- [x] `/v1/modelling-groups/:group-id/responsibilities/:touchstone-id/:scenario-id/estimate-sets/:estimate-set-id/estimates/` (GET)
- [x] `/v1/modelling-groups/:group-id/responsibilities/:touchstone-id/:scenario-id/estimate-sets/:estimate-set-id/actions/clear` (POST)
- [x] `/v1/modelling-groups/:group-id/responsibilities/:touchstone-id/:scenario-id/estimate-sets/:estimate-set-id/actions/close` (POST)
- [ ] `/v1/modelling-groups/:group-id/responsibilities/:touchstone-id/:scenario-id/estimate-sets/:estimate-set-id/actions/populate/{token}/` (GET)
- [ ] `/v1/modelling-groups/:group-id/responsibilities/:touchstone-id/:scenario-id/estimate-sets/:estimate-set-id/actions/request-upload/` (GET)
- [ ] `/v1/modelling-groups/:group-id/responsibilities/:touchstone-id/:scenario-id/estimate-sets/:estimate-set-id/actions/upload/{token}/` (POST)
- [x] `/v1/modelling-groups/:group-id/responsibilities/:touchstone-id/:scenario-id/estimate-sets/:estimate-set-id/estimates/:outcome-code/` (GET)
- [x] `/v1/models/` (GET)
- [x] `/v1/models/:id/` (GET)
- [ ] `/v1/onetime_link/:token/`
- [ ] `/v1/password/request/link/?email=:email/` (POST)
- [ ] `/v1/password/set/` (POST)
- [ ] `/v1/set-cookies/` (GET)
- [x] `/v1/touchstones/` (GET)
- [x] `/v1/touchstones/:touchstone-id/demographics/` (GET)
- [x] `/v1/touchstones/:touchstone-id/demographics/:source_code/:demographic-type-code/` (GET)
- [ ] `/v1/touchstones/:touchstone-id/demographics/:source_code/:demographic-type-code/csv/` (GET)
- [x] `/v1/touchstones/:touchstone-id/responsibilities/` (GET)
- [x] `/v1/touchstones/:touchstone-id/scenario-id/coverage/` (GET)
- [ ] `/v1/touchstones/:touchstone-id/scenarios/` (GET)
- [ ] `/v1/touchstones/:touchstone-id/scenarios/:scenario-id/` (GET)
- [ ] `/v1/users/` (GET)
- [ ] `/v1/users/` (POST)
- [ ] `/v1/users/:username/` (GET)
- [ ] `/v1/users/:username/actions/associate-role/` (POST)
- [ ] `/v1/users/report-headers/:reportname/` (GET)
- [ ] `/v1/users/rfp/agree-confidentiality/` (GET)
- [ ] `/v1/users/rfp/agree-confidentiality/` (POST)

### Reporting API

- [ ] `/v1/`
- [x] `/v1/data/csv/:id/`
- [x] `/v1/data/rds/:id/`
- [ ] `/v1/onetime_token/`
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
