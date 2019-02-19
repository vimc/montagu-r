RSCRIPT = Rscript


test:
	${RSCRIPT} -e 'library(methods); devtools::test()'

roxygen:
	@mkdir -p man
	${RSCRIPT} -e "library(methods); devtools::document()"

install:
	R CMD INSTALL .

build:
	R CMD build .

check:
	_R_CHECK_CRAN_INCOMING_=FALSE make check_all

check_all:
	${RSCRIPT} -e "rcmdcheck::rcmdcheck(args = c('--as-cran', '--no-manual'))"


## We can't build vignettes on CRAN and systems without docker (and
## even when they do, it's not a great idea because we build and
## remove a bunch of containers etc.
vignettes/%.Rmd: vignettes_src/%.Rmd
	cd vignettes_src && ${RSCRIPT} -e 'knitr::knit("$(<F)", output = "../$@")'
	sed -i.bak 's/[[:space:]]*$$//' $@
	rm -f $@.bak

vignettes_install: vignettes/montagu.Rmd vignettes/montagu_user_guide.Rmd
	${RSCRIPT} -e 'tools::buildVignettes(dir = ".")'

vignettes:
	make vignettes_install

pkgdown:
	${RSCRIPT} -e "library(methods); pkgdown::build_site(lazy = TRUE)"

website: pkgdown
	./scripts/update_web.sh


.PHONY: test roxygen install build check check_all vignettes
