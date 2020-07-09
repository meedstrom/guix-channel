;; statistics.scm
;; Copyright (C) 2019 Martin Edström

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

(define-module (mee statistics)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix licenses)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system r)
  #:use-module (gnu packages base)
  #:use-module (gnu packages finance)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages gcc)
  #:use-module (srfi srfi-1)
  )

;; using (@ ...) for the external definition is necessary because we're giving
;; our local definition the same name, thus the name becomes a self-reference
;; to recurse on, not an external definition.
;; (define-public gfortran-9
;;   (package
;;    (inherit (@@ (gnu packages gcc) gfortran-9))))

(define-public gfortran-9
  (package
   (inherit ((@@ (gnu packages gcc) custom-gcc)
             gcc-9 "gfortran" '("fortran")
             (@@ (gnu packages gcc) %generic-search-paths)))))

(define-public r-minimal-mee1
  (package
    (inherit (@ (gnu packages statistics) r-minimal))
    (version "4-mee1")
    (inputs `(("gfortran" ,gfortran-9)
              ,@(alist-delete "gfortran"
                              (package-inputs (@ (gnu packages statistics) r-minimal)))))))

(define-public r-mee1
  (package
    (inherit (@ (gnu packages statistics) r))
    (version "4-mee1")
    (inputs `(("r-minimal" ,r-minimal-mee1)
              ,@(alist-delete "r-minimal"
                              (package-inputs (@ (gnu packages statistics) r)))))))

(define-public r-rstan-mee1
  (package
    (inherit (@ (gnu packages cran) r-rstan))
    (version "2.19.3-mee1")
    (propagated-inputs `(("gcc-toolchain" ,gcc-toolchain)
                         ("make" ,gnu-make)
                         ,@(package-propagated-inputs (@ (gnu packages cran) r-rstan))))))

(define-public r-rstanarm-mee1
  (package
    (inherit (@ (gnu packages cran) r-rstanarm))
    (version "2.19.3-mee1")
    (propagated-inputs
     `(("r-rstan" ,r-rstan-mee1)
       ("r-shinystan" ,r-shinystan-mee1)
       ("r-codetools" ,r-codetools)
       ,@(alist-delete "r-shinystan"
                       (alist-delete "r-rstan"
                                     (package-propagated-inputs
                                      (@ (gnu packages cran) r-rstanarm))))))))

(define-public r-shinystan-mee1
  (package
    (inherit (@ (gnu packages cran) r-shinystan))
    (version "2.5.0-mee1")
    (propagated-inputs
     `(("r-rstan" ,r-rstan-mee1)
       ,@(alist-delete "r-rstan"
                       (package-propagated-inputs (@ (gnu packages cran) r-shinystan)))))))

(define-public r-broom-mixed
  (package
    (name "r-broom-mixed")
    (version "0.2.4")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "broom.mixed" version))
        (sha256
          (base32
            "176817l4d31xbi7yfsjlw287va2wjllxizhi5z3p7x5mi7fkyv2c"))))
    (properties `((upstream-name . "broom.mixed")))
    (build-system r-build-system)
    (propagated-inputs
      `(("r-broom" ,r-broom)
        ("r-coda" ,r-coda)
        ("r-dplyr" ,r-dplyr)
        ("r-nlme" ,r-nlme)
        ("r-plyr" ,r-plyr)
        ("r-purrr" ,r-purrr)
        ("r-reshape2" ,r-reshape2)
        ("r-stringr" ,r-stringr)
        ("r-tibble" ,r-tibble)
        ("r-tidyr" ,r-tidyr)
        ("r-tmb" ,r-tmb)))
    (native-inputs `(("r-knitr" ,r-knitr)))
    (home-page
      "http://github.com/bbolker/broom.mixed")
    (synopsis "Tidying Methods for Mixed Models")
    (description
      "Convert fitted objects from various R mixed-model packages into tidy
data frames along the lines of the 'broom' package. The package provides three
S3 generics for each model: tidy(), which summarizes a model's statistical
findings such as coefficients of a regression; augment(), which adds columns to
the original data such as predictions, residuals and cluster assignments; and
glance(), which provides a one-row summary of model-level statistics.")
    (license gpl3)))

(define-public r-vars
  (package
    (name "r-vars")
    (version "1.5-3")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "vars" version))
        (sha256
          (base32
            "0zc6v827ll19n088n31afgjf65zqwvyzmmj4q3ab1xhqzxfsgbw6"))))
    (properties `((upstream-name . "vars")))
    (build-system r-build-system)
    (propagated-inputs
      `(("r-lmtest" ,r-lmtest)
        ("r-mass" ,r-mass)
        ("r-sandwich" ,r-sandwich)
        ("r-strucchange" ,r-strucchange)
        ("r-urca" ,r-urca)))
    (home-page "http://www.pfaffikus.de")
    (synopsis "VAR Modelling")
    (description
      "Estimation, lag selection, diagnostic testing, forecasting, causality analysis, forecast error variance decomposition and impulse response functions of VAR models and estimation of SVAR and SVEC models.")
    (license gpl2+)))

;; for rethinking
;; he ded m8
(define-public r-v8
  (package
   (name "r-v8")
   (version "3.0.1")
   (source
    (origin
     (method url-fetch)
     (uri (cran-uri "V8" version))
     (sha256
      (base32
       "0xkyja3722qbs2ndyw3171yfsakijnydm2bfqv76cf0gbzxdab5b"))))
   (properties `((upstream-name . "V8")))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-curl" ,r-curl)
      ("r-jsonlite" ,r-jsonlite)
      ("r-rcpp" ,r-rcpp)))
   (home-page
    "https://github.com/jeroen/v8")
   (synopsis
    "Embedded JavaScript and WebAssembly Engine for R")
   (description
    "An R interface to V8: Google's open source JavaScript and WebAssembly
engine. This package can be compiled either with V8 version 6 and up, a NodeJS
shared library, or the legacy 3.14/3.15 branch of V8.")
   (license expat)))

(define-public emacs-ess-next
  (package
   (name "emacs-ess-next")
   (version "18.10.3snapshot") ;; from ./VERSION as of 2020 on master
   (source (origin
            (method git-fetch)
            (uri (git-reference (url "https://github.com/emacs-ess/ESS/")
                                (commit "1c2387fdba509c1c9d072150f65ccc318a570870")))
            (sha256
             (base32 "0llsjqrvabcvpd2nhixiklwmm2lisywif77iwfrsdc6lfxp8cdd0"))
            (modules '((guix build utils)))
            (snippet
             '(begin
                ;; Stop ESS from trying to fetch an external julia-mode.el.
                (substitute* "lisp/Makefile"
                             (("JULIAS := julia-mode.el julia-mode-latexsubs.el") "")
                             (("\\$.JULIAS.") "")
                             ;; (("\\$\\(JULIAS\\)") "")
                             ;; (("\\$\\{DOWNLOAD\\}") "")
                             (("test -f ../etc/.IS.RELEASE .*") ""))
                (substitute* "Makefile"
                             ((".PHONY: julia") "")
                             (("julia:") "")
                             (("@cd lisp; \\$\\(MAKE\\) julia-mode.el") "")
                             (("cd lisp; \\$\\(MAKE\\) julia-mode.el; \\$\\(INSTALL\\) julia-mode.el ../\\$\\(ESSDIR\\)/lisp/; cd ..") "")
                             )
                (substitute* "test/Makefile"
                             (("julia:") "")
                             (("@\\$.MAKE. -C .. julia") "")
                             )

                ;; test fails: "R not running"
                (substitute* "test/run-tests"
                             (("\\(load \\(expand-file-name \"ess-test-org.el\" ess-test-path\\) nil t\\)") ""))
                (substitute* "Makefile"
                             (("\\$\\(INSTALL\\) -R \\./\\* \\$\\(ESSDIR\\)/")
                              "	cd etc; $(MAKE) $@
	cd lisp; $(MAKE) $@")
                             (("mkdir \\$\\(ESSDIR\\)") "")
                             ;; "$(INSTALL) -R ./* $(ESSDIR) || true"
                             ;; (("ESSDIR := ess-\\$\\(ESSVERSION\\)")
                             ;;  ""
                             )
                ;; No need to build docs in so many formats.  Also, skipping
                ;; pdf lets us not pull in texlive.
                (substitute* "doc/Makefile"
                             (("all  : info text html pdf")
                              "all  : info")
                             (("install: install-info install-other-docs")
                              "install: install-info"))

                #t))))
   (build-system gnu-build-system)
   (arguments
    (let ((base-directory "/share/emacs/site-lisp/guix.d/ess"))
      `(#:make-flags (list (string-append "PREFIX=" %output)
                           (string-append "ETCDIR=" %output "/"
                                          ,base-directory "/etc")
                           (string-append "LISPDIR=" %output "/"
                                          ,base-directory))
        #:phases
        (modify-phases %standard-phases
                       (delete 'configure)
                       (add-before 'build 'more-shebang-patching
                                   (lambda* (#:key inputs #:allow-other-keys)
                                     (substitute* "Makeconf"
                                                  (("SHELL = /bin/sh")
                                                   (string-append "SHELL = " (which "sh"))))
                                     (substitute* "lisp/Makefile"
                                                  (("JULIA-REPO=https://raw.githubusercontent.com/JuliaEditorSupport/julia-emacs/master")
                                                   (string-append "JULIA-REPO=" (assoc-ref inputs "emacs-julia-mode") "/share/emacs/site-lisp")))


                                     #t))
                       (replace 'check
                                (lambda _
                                  (invoke "make" "test")))))))
   (inputs
    `(("emacs" ,emacs-minimal)
      ("r-minimal" ,r-minimal)))
   (native-inputs
    `(("perl" ,perl)
      ("texinfo" ,texinfo)))
   (propagated-inputs
    `(("emacs-julia-mode" ,emacs-julia-mode)))
   (home-page "https://ess.r-project.org/")
   (synopsis "Emacs mode for statistical analysis programs")
   (description "Emacs Speaks Statistics (ESS) is an add-on package for GNU
Emacs.  It is designed to support editing of scripts and interaction with
various statistical analysis programs such as R, Julia, and JAGS.")
   (license gpl2+)))

(define-public r-whoami
  (package
   (name "r-whoami")
   (version "1.3.0")
   (source
    (origin
     (method url-fetch)
     (uri (cran-uri "whoami" version))
     (sha256
      (base32
       "19fwl7z55s4kl2xzwqwh8iwg13kdrv222vyl3kibxgwrjcjwj2y2"))))
   (properties `((upstream-name . "whoami")))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-httr" ,r-httr) ("r-jsonlite" ,r-jsonlite)))
   (home-page
    "https://github.com/r-lib/whoami#readme")
   (synopsis
    "Username, Full Name, Email Address, 'GitHub' Username of the Current User")
   (description
    "Look up the username and full name of the current user, the current user's email address and 'GitHub' username, using various sources of system and configuration information.")
   (license expat)))

;; some warnings during build (cannot find Rmd vignettes)
(define-public r-goodpractice
  (package
   (name "r-goodpractice")
   (version "1.0.2")
   (source
    (origin
     (method url-fetch)
     (uri (cran-uri "goodpractice" version))
     (sha256
      (base32
       "08rnm8z0pry5mwpc7p37r6f2rd56dry9djpcvvs9yl170w56psx0"))))
   (properties `((upstream-name . "goodpractice")))
   (build-system r-build-system)
   (inputs `(("r-knitr" ,r-knitr)
             ("r-rmarkdown" ,r-rmarkdown)))
   (propagated-inputs
    `(("r-clisymbols" ,r-clisymbols)
      ("r-covr" ,r-covr)
      ("r-crayon" ,r-crayon)
      ("r-cyclocomp" ,r-cyclocomp)
      ("r-desc" ,r-desc)
      ("r-jsonlite" ,r-jsonlite)
      ("r-lintr" ,r-lintr)
      ("r-praise" ,r-praise)
      ("r-rcmdcheck" ,r-rcmdcheck)
      ("r-rstudioapi" ,r-rstudioapi)
      ("r-whoami" ,r-whoami)
      ("r-withr" ,r-withr)
      ("r-xml2" ,r-xml2)
      ("r-xmlparsedata" ,r-xmlparsedata)))
   (home-page
    "https://github.com/mangothecat/goodpractice")
   (synopsis "Advice on R Package Building")
   (description
    "Give advice about good practices when building R packages.  Advice includes functions and syntax to avoid, package structure, code complexity, code formatting, etc.")
   (license expat)))

(define-public r-nsrfa
  (package
   (name "r-nsrfa")
   (version "0.7-14")
   (source
    (origin
     (method url-fetch)
     (uri (cran-uri "nsRFA" version))
     (sha256
      (base32
       "0sd3br2a5jf6mqbxk8s2i7px9vi9bk3pl8bd5884k40xqcj77irs"))))
   (properties `((upstream-name . "nsRFA")))
   (build-system r-build-system)
   (native-inputs `(("gfortran" ,gfortran)))
   (home-page
    "https://cran.r-project.org/web/packages/nsRFA")
   (synopsis
    "Non-Supervised Regional Frequency Analysis")
   (description
    "This package provides a collection of statistical tools for objective (non-supervised) applications of the Regional Frequency Analysis methods in hydrology.  The package refers to the index-value method and, more precisely, helps the hydrologist to: (1) regionalize the index-value; (2) form homogeneous regions with similar growth curves; (3) fit distribution functions to the empirical regional growth curves.  Most of the methods are those described in the Flood Estimation Handbook (Centre for Ecology & Hydrology, 1999, ISBN:9781906698003).  Homogeneity tests from Hosking and Wallis (1993) <doi:10.1029/92WR01980> and Viglione et al. (2007) <doi:10.1029/2006WR005095> are available.")
   (license gpl2+)))

;; for vif()
(define-public r-faraway
  (package
   (name "r-faraway")
   (version "1.0.7")
   (source
    (origin
     (method url-fetch)
     (uri (cran-uri "faraway" version))
     (sha256
      (base32
       "0lalf52y9rb4zdb4kpscwddb4zy0af7r5sm7lx8s9jaqykrwrfq6"))))
   (properties `((upstream-name . "faraway")))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-lme4" ,r-lme4) ("r-nlme" ,r-nlme)))
   (home-page "http://people.bath.ac.uk/jjf23/")
   (synopsis
    "Functions and Datasets for Books by Julian Faraway")
   (description
    "Books are \"Practical Regression and ANOVA in R\" on CRAN, \"Linear Models with R\" published 1st Ed.  August 2004, 2nd Ed.  July 2014 by CRC press, ISBN 9781439887332, and \"Extending the Linear Model with R\" published by CRC press in 1st Ed.  December 2005 and 2nd Ed.  March 2016, ISBN 9781584884248.")
   (license (list gpl2+ gpl3+))))

(define-public r-goftest
  (package
   (name "r-goftest")
   (version "1.1-1")
   (source
    (origin
     (method url-fetch)
     (uri (cran-uri "goftest" version))
     (sha256
      (base32
       "183imn6dy28ama8bywxgyh32vgakv7bsbz4k5qbhnlhqdvzv2v6v"))))
   (properties `((upstream-name . "goftest")))
   (build-system r-build-system)
   (home-page
    "https://cran.r-project.org/web/packages/goftest")
   (synopsis
    "Classical Goodness-of-Fit Tests for Univariate Distributions")
   (description
    "Cramer-Von Mises and Anderson-Darling tests of goodness-of-fit for continuous univariate distributions, using efficient algorithms.")
   (license gpl2+)))

(define-public r-olsrr
  (package
   (name "r-olsrr")
   (version "0.5.2")
   (source
    (origin
     (method url-fetch)
     (uri (cran-uri "olsrr" version))
     (sha256
      (base32
       "13qampcc07qfjj4r3ira0m1j2wgnrkf5p1wswr9g814azhm52haz"))))
   (properties `((upstream-name . "olsrr")))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-car" ,r-car)
      ("r-checkmate" ,r-checkmate)
      ("r-cli" ,r-cli)
      ("r-clisymbols" ,r-clisymbols)
      ("r-crayon" ,r-crayon)
      ("r-dplyr" ,r-dplyr)
      ("r-ggplot2" ,r-ggplot2)
      ("r-gh" ,r-gh)
      ("r-glue" ,r-glue)
      ("r-goftest" ,r-goftest)
      ("r-gridextra" ,r-gridextra)
      ("r-magrittr" ,r-magrittr)
      ("r-nortest" ,r-nortest)
      ("r-purrr" ,r-purrr)
      ("r-rcpp" ,r-rcpp)
      ("r-recipes" ,r-recipes)
      ("r-rlang" ,r-rlang)
      ("r-shiny" ,r-shiny)
      ("r-stringr" ,r-stringr)
      ("r-tibble" ,r-tibble)
      ("r-tidyr" ,r-tidyr)))
   (home-page "https://olsrr.rsquaredacademy.com/")
   (synopsis
    "Tools for Building OLS Regression Models")
   (description
    "Tools designed to make it easier for users, particularly beginner/intermediate R users to build ordinary least squares regression models.  Includes comprehensive regression output, heteroskedasticity tests, collinearity diagnostics, residual diagnostics, measures of influence, model fit assessment and variable selection procedures.")
   (license expat)))

(define-public r-gapminder
  (package
   (name "r-gapminder")
   (version "0.3.0")
   (source
    (origin
     (method url-fetch)
     (uri (cran-uri "gapminder" version))
     (sha256
      (base32
       "067cra1ca4ngwjx8d1y9pyzwcpsfi1wcal0glzyy6ghd1k6jflpv"))))
   (properties `((upstream-name . "gapminder")))
   (build-system r-build-system)
   (propagated-inputs `(("r-tibble" ,r-tibble)))
   (home-page
    "https://github.com/jennybc/gapminder")
   (synopsis "Data from Gapminder")
   (description
    "An excerpt of the data available at Gapminder.org.  For each of 142 countries, the package provides values for life expectancy, GDP per capita, and population, every five years, from 1952 to 2007.")
   (license #f)))

(define-public r-nycflights13
  (package
   (name "r-nycflights13")
   (version "1.0.1")
   (source
    (origin
     (method url-fetch)
     (uri (cran-uri "nycflights13" version))
     (sha256
      (base32
       "0h4jzg4q5qpvjp5b2wlk3a2mh0jhyz9c1acdjcmg70pap7fyh0fj"))))
   (properties `((upstream-name . "nycflights13")))
   (build-system r-build-system)
   (propagated-inputs `(("r-tibble" ,r-tibble)))
   (home-page
    "http://github.com/hadley/nycflights13")
   (synopsis "Flights that Departed NYC in 2013")
   (description
    "Airline on-time data for all flights departing NYC in 2013.  Also includes useful 'metadata' on airlines, airports, weather, and planes.")
   (license cc0)))

(define-public r-ledger
  (package
   (name "r-ledger")
   (version "2.0.4")
   (source
    (origin
     (method url-fetch)
     (uri (cran-uri "ledger" version))
     (sha256
      (base32
       "19jqy24c8xkpcs9d5w4a1ih474xpnxjnq1q8940mfkx1vq2bz1m5"))))
   (properties `((upstream-name . "ledger")))
   (build-system r-build-system)
   (inputs
    `(;; ("beancount" ,beancount)
      ;; ("hledger" ,hledger)
      ("ledger" ,ledger)))
   (propagated-inputs
    `(("r-dplyr" ,r-dplyr)
      ("r-rio" ,r-rio)
      ("r-rlang" ,r-rlang)
      ("r-stringr" ,r-stringr)
      ("r-tibble" ,r-tibble)
      ("r-tidyr" ,r-tidyr)
      ("r-tidyselect" ,r-tidyselect)))
   (home-page
    "https://github.com/trevorld/r-ledger")
   (synopsis
    "Utilities for Importing Data from Plain Text Accounting Files")
   (description
    "Utilities for querying plain text accounting files of the 'Ledger' format.")
   (license expat)))

(define-public r-countrycode
  (package
    (name "r-countrycode")
    (version "1.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "countrycode" version))
        (sha256
          (base32
            "1ys76s1sa6xj0mr5kryscwsh3dlrn9w35fx3fnr325j35xalyka4"))))
    (properties `((upstream-name . "countrycode")))
    (build-system r-build-system)
    (home-page
      "https://github.com/vincentarelbundock/countrycode")
    (synopsis
      "Convert Country Names and Country Codes")
    (description
      "Standardize country names, convert them into one of eleven coding schemes, convert between coding schemes, and assign region descriptors.")
    (license gpl3)))

(define-public r-eurostat
  (package
    (name "r-eurostat")
    (version "3.6.1")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "eurostat" version))
        (sha256
          (base32
            "1l2j9fdq33ivb3wns5bdr1dccaf494vwbyya3f2gz2g8mc2qpn0a"))))
    (properties `((upstream-name . "eurostat")))
    (build-system r-build-system)
    (propagated-inputs
      `(("r-broom" ,r-broom)
        ("r-classint" ,r-classint)
        ("r-countrycode" ,r-countrycode)
        ("r-curl" ,r-curl)
        ("r-dplyr" ,r-dplyr)
        ("r-httr" ,r-httr)
        ("r-jsonlite" ,r-jsonlite)
        ("r-lubridate" ,r-lubridate)
        ("r-rcolorbrewer" ,r-rcolorbrewer)
        ("r-readr" ,r-readr)
        ("r-refmanager" ,r-refmanager)
        ("r-sf" ,r-sf)
        ("r-sp" ,r-sp)
        ("r-stringi" ,r-stringi)
        ("r-stringr" ,r-stringr)
        ("r-tibble" ,r-tibble)
        ("r-tidyr" ,r-tidyr)
        ("r-tidyselect" ,r-tidyselect)))
    (home-page "https://ropengov.github.io/eurostat")
    (synopsis "Tools for Eurostat Open Data")
    (description
      "Tools to download data from the Eurostat database <http://ec.europa.eu/eurostat> together with search and manipulation utilities.")
    (license bsd-2)))

(define-public r-tvthemes
  (package
    (name "r-tvthemes")
    (version "1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "tvthemes" version))
        (sha256
          (base32
            "0bc9li8vgmm4r6dshm52ll2d69gcynv1xrv8n7jhbyn7xmx3q3kz"))))
    (properties `((upstream-name . "tvthemes")))
    (build-system r-build-system)
    (propagated-inputs
      `(("r-extrafont" ,r-extrafont)
        ("r-ggplot2" ,r-ggplot2)
        ("r-glue" ,r-glue)
        ("r-magick" ,r-magick)
        ("r-scales" ,r-scales)))
    (native-inputs `(("r-knitr" ,r-knitr)))
    (home-page "https://github.com/Ryo-N7/tvthemes")
    (synopsis
      "TV Show Themes and Color Palettes for 'ggplot2' Graphics")
    (description
      "Contains various 'ggplot2' themes and color palettes based on TV shows such as 'Game of Thrones', 'Brooklyn Nine-Nine', 'Avatar: The Last Airbender', 'Spongebob Squarepants', and more.")
    (license gpl3)))

(define-public r-wesanderson
  (package
    (name "r-wesanderson")
    (version "0.3.6")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "wesanderson" version))
        (sha256
          (base32
            "09mr6p2jmqdjq27cz974w5hyxgn929zp9z3inhxqmmh1582fmdi2"))))
    (properties `((upstream-name . "wesanderson")))
    (build-system r-build-system)
    (home-page
      "https://github.com/karthik/wesanderson")
    (synopsis "A Wes Anderson Palette Generator")
    (description
      "Palettes generated mostly from 'Wes Anderson' movies.")
    (license expat)))

(define-public r-ggdark
  (package
    (name "r-ggdark")
    (version "0.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "ggdark" version))
        (sha256
          (base32
            "1w93g2j4g45x9s841v9zi18lxzda81ipa13fajqc6p9xk8frvgrf"))))
    (properties `((upstream-name . "ggdark")))
    (build-system r-build-system)
    (propagated-inputs `(("r-ggplot2" ,r-ggplot2)))
    (home-page
      "https://cran.r-project.org/web/packages/ggdark")
    (synopsis "Dark Mode for 'ggplot2' Themes")
    (description
      "Activate dark mode on your favorite 'ggplot2' theme with dark_mode() or use the dark versions of 'ggplot2' themes, including dark_theme_gray(), dark_theme_minimal(), and others.  When a dark theme is applied, all geom color and geom fill defaults are changed to make them visible against a dark background.  To restore the defaults to their original values, use invert_geom_defaults().")
    (license expat)))

(define-public r-ghibli
  (package
    (name "r-ghibli")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "ghibli" version))
        (sha256
          (base32
            "1wnz26n0a3pgzigb2vp08v7swk061whi2zvfm6bih0aniqv5n7ml"))))
    (properties `((upstream-name . "ghibli")))
    (build-system r-build-system)
    (native-inputs `(("r-knitr" ,r-knitr)))
    (home-page "https://github.com/ewenme/ghibli")
    (synopsis "Studio Ghibli Colour Palettes")
    (description
      "Colour palettes inspired by Studio Ghibli <https://en.wikipedia.org/wiki/Studio_Ghibli> films, ported to R for your enjoyment.")
    (license expat)))

(define-public r-ggmcmc
  (package
    (name "r-ggmcmc")
    (version "1.3")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "ggmcmc" version))
        (sha256
          (base32
            "0p3akjbi0044nma8ynvqa42bkrgrb0jjx323k2pbrnlkq8x3ma1b"))))
    (properties `((upstream-name . "ggmcmc")))
    (build-system r-build-system)
    (propagated-inputs
      `(("r-dplyr" ,r-dplyr)
        ("r-ggally" ,r-ggally)
        ("r-ggplot2" ,r-ggplot2)
        ("r-tidyr" ,r-tidyr)))
    (native-inputs `(("r-knitr" ,r-knitr)))
    (home-page
      "http://xavier-fim.net/packages/ggmcmc")
    (synopsis
      "Tools for Analyzing MCMC Simulations from Bayesian Inference")
    (description
      "Tools for assessing and diagnosing convergence of Markov Chain Monte Carlo simulations, as well as for graphically display results from full MCMC analysis.  The package also facilitates the graphical interpretation of models by providing flexible functions to plot the results against observed variables.")
    (license gpl2)))

(define-public r-rcartocolor
  (package
    (name "r-rcartocolor")
    (version "2.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "rcartocolor" version))
        (sha256
          (base32
            "08ljaag2mnhz2671zvwji1sp003z94lz30vjidmybm9fp8piqw5g"))))
    (properties `((upstream-name . "rcartocolor")))
    (build-system r-build-system)
    (propagated-inputs
      `(("r-ggplot2" ,r-ggplot2) ("r-scales" ,r-scales)))
    (home-page
      "https://github.com/Nowosad/rcartocolor")
    (synopsis "'CARTOColors' Palettes")
    (description
      "This package provides color schemes for maps and other graphics designed by 'CARTO' as described at <https://carto.com/carto-colors/>.  It includes four types of palettes: aggregation, diverging, qualitative, and quantitative.")
    (license expat)))

;; h4xor3d
(define-public r-ggextra
  (package
    (name "r-ggextra")
    (version "0.9-mee1")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "ggExtra" version))
        (sha256
          (base32
            "18mbi6gblqmrsciad1d2c9ngllk6mayaqj43k40hjq9ydqnvjbgj"))))
    (properties `((upstream-name . "ggExtra")))
    (build-system r-build-system)
    (propagated-inputs
      `(("r-colourpicker" ,r-colourpicker)
        ("r-ggplot2" ,r-ggplot2)
        ("r-gtable" ,r-gtable)
        ("r-miniui" ,r-miniui)
        ("r-r6" ,r-r6)
        ("r-scales" ,r-scales)
        ("r-shiny" ,r-shiny)
        ("r-shinyjs" ,r-shinyjs)
        ("r-knitr" ,r-knitr))) ;; for the test phase
    (native-inputs `(("r-knitr" ,r-knitr)))
    (home-page "https://github.com/daattali/ggExtra")
    (synopsis
      "Add Marginal Histograms to 'ggplot2', and More 'ggplot2' Enhancements")
    (description
      "Collection of functions and layers to enhance 'ggplot2'.  The flagship function is 'ggMarginal()', which can be used to add marginal histograms/boxplots/density plots to 'ggplot2' scatterplots.")
    (license expat)))


;; =============================================================================
;; TIDYMODELS & DEPENDENCIES
;; custom: tidyposterior (see comments)

(define-public r-gpfit
  (package
    (name "r-gpfit")
    (version "1.0-8")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "GPfit" version))
        (sha256
          (base32
            "05mpiyi2vxv0wqp422n1mnxa8msc4daq40cwpnpngbcwqhlgqkby"))))
    (properties `((upstream-name . "GPfit")))
    (build-system r-build-system)
    (propagated-inputs
      `(("r-lattice" ,r-lattice) ("r-lhs" ,r-lhs)))
    (home-page
      "https://cran.r-project.org/web/packages/GPfit")
    (synopsis "Gaussian Processes Modeling")
    (description
     "This package provides a computationally stable approach of fitting a Gaussian Process (GP) model to a deterministic simulator.")
    (license gpl2)))

(define-public r-hardhat
  (package
    (name "r-hardhat")
    (version "0.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "hardhat" version))
        (sha256
          (base32
            "10x8fw0skaqci03v2qqpbradbra9arm3s5pskcwm4wricd2imr40"))))
    (properties `((upstream-name . "hardhat")))
    (build-system r-build-system)
    (propagated-inputs
      `(("r-glue" ,r-glue)
        ("r-rlang" ,r-rlang)
        ("r-tibble" ,r-tibble)
        ("r-vctrs" ,r-vctrs)))
    (native-inputs `(("r-knitr" ,r-knitr)))
    (home-page
      "https://github.com/tidymodels/hardhat")
    (synopsis "Construct Modeling Packages")
    (description
      "Building modeling packages is hard.  A large amount of effort generally goes into providing an implementation for a new method that is efficient, fast, and correct, but often less emphasis is put on the user interface.  A good interface requires specialized knowledge about S3 methods and formulas, which the average package developer might not have.  The goal of 'hardhat' is to reduce the burden around building new modeling packages by providing functionality for preprocessing, predicting, and validating input.")
    (license expat)))

(define-public r-tokenizers
  (package
   (name "r-tokenizers")
   (version "0.2.1")
   (source
    (origin
     (method url-fetch)
     (uri (cran-uri "tokenizers" version))
     (sha256
      (base32
       "006xf1vdrmp9skhpss9ldhmk4cwqk512cjp1pxm2gxfybpf7qq98"))))
   (properties `((upstream-name . "tokenizers")))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-rcpp" ,r-rcpp)
      ("r-snowballc" ,r-snowballc)
      ("r-stringi" ,r-stringi)))
   (home-page
    "https://lincolnmullen.com/software/tokenizers/")
   (synopsis
    "Fast, Consistent Tokenization of Natural Language Text")
   (description
    "Convert natural language text into tokens.  Includes tokenizers for shingled n-grams, skip n-grams, words, word stems, sentences, paragraphs, characters, shingled characters, lines, tweets, Penn Treebank, regular expressions, as well as functions for counting characters, words, and sentences, and a function for splitting longer texts into separate documents, each with the same number of words.  The tokenizers have a consistent interface, and the package is built on the 'stringi' and 'Rcpp' packages for  fast yet correct tokenization in 'UTF-8'.")
   (license expat)))

(define-public r-janeaustenr
  (package
   (name "r-janeaustenr")
   (version "0.1.5")
   (source
    (origin
     (method url-fetch)
     (uri (cran-uri "janeaustenr" version))
     (sha256
      (base32
       "1wyn4qc28a3sval8shmyi2d7s4nl3jh96s8pzq871brxcmrncbwr"))))
   (properties `((upstream-name . "janeaustenr")))
   (build-system r-build-system)
   (home-page
    "https://github.com/juliasilge/janeaustenr")
   (synopsis "Jane Austen's Complete Novels")
   (description
    "Full texts for Jane Austen's 6 completed novels, ready for text analysis.  These novels are \"Sense and Sensibility\", \"Pride and Prejudice\", \"Mansfield Park\", \"Emma\", \"Northanger Abbey\", and \"Persuasion\".")
   (license expat)))

(define-public r-dicedesign
  (package
   (name "r-dicedesign")
   (version "1.8-1")
   (source
    (origin
     (method url-fetch)
     (uri (cran-uri "DiceDesign" version))
     (sha256
      (base32
       "11s1m543kxd6gv4amh8z6pph1n67sj9sfwm6hjy83wfs65syf5vp"))))
   (properties `((upstream-name . "DiceDesign")))
   (build-system r-build-system)
   (home-page "http://dice.emse.fr/")
   (synopsis "Designs of Computer Experiments")
   (description
    "Space-Filling Designs and Uniformity Criteria.")
   (license gpl3)))

(define-public r-hunspell
  (package
   (name "r-hunspell")
   (version "3.0")
   (source
    (origin
     (method url-fetch)
     (uri (cran-uri "hunspell" version))
     (sha256
      (base32
       "0mwqw5p0ph083plm2hr2hqr50bjg2dw862dpsfm4l2fgyy3rryq1"))))
   (properties `((upstream-name . "hunspell")))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-digest" ,r-digest) ("r-rcpp" ,r-rcpp)))
   (home-page
    "https://github.com/ropensci/hunspell#readmehttps://hunspell.github.io")
   (synopsis
    "High-Performance Stemmer, Tokenizer, and Spell Checker")
   (description
    "Low level spell checker and morphological analyzer based on the famous 'hunspell' library <https://hunspell.github.io>.  The package can analyze or check individual words as well as parse text, latex, html or xml documents.  For a more user-friendly interface use the 'spelling' package which builds on this package to automate checking of files, documentation and vignettes in all common formats.")
   (license #f)))

(define-public r-dials
  (package
    (name "r-dials")
    (version "0.0.6")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "dials" version))
        (sha256
          (base32
            "0hvhn8adkb8pywfh058vj681bccm4iyky11ijc01xk2w3iaslyr9"))))
    (properties `((upstream-name . "dials")))
    (build-system r-build-system)
    (propagated-inputs
      `(("r-dicedesign" ,r-dicedesign)
        ("r-dplyr" ,r-dplyr)
        ("r-glue" ,r-glue)
        ("r-lifecycle" ,r-lifecycle)
        ("r-purrr" ,r-purrr)
        ("r-rlang" ,r-rlang)
        ("r-scales" ,r-scales)
        ("r-tibble" ,r-tibble)
        ("r-withr" ,r-withr)))
    (native-inputs `(("r-knitr" ,r-knitr)))
    (home-page "https://tidymodels.github.io/dials")
    (synopsis
      "Tools for Creating Tuning Parameter Values")
    (description
      "Many models contain tuning parameters (i.e.  parameters that cannot be directly estimated from the data).  These tools can be used to define objects for creating, simulating, or validating values for such parameters.")
    (license gpl2)))

(define-public r-workflows
  (package
    (name "r-workflows")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "workflows" version))
        (sha256
          (base32
            "14lzbszz7ybfzqa5zw1hfh81b8rbwwyza6x8nhpnknl6x4adqfql"))))
    (properties `((upstream-name . "workflows")))
    (build-system r-build-system)
    (propagated-inputs
      `(("r-cli" ,r-cli)
        ("r-ellipsis" ,r-ellipsis)
        ("r-generics" ,r-generics)
        ("r-glue" ,r-glue)
        ("r-hardhat" ,r-hardhat)
        ("r-parsnip" ,r-parsnip)
        ("r-rlang" ,r-rlang)))
    (native-inputs `(("r-knitr" ,r-knitr)))
    (home-page
      "https://github.com/tidymodels/workflows")
    (synopsis "Modeling Workflows")
    (description
      "Managing both a 'parsnip' model and a preprocessor, such as a model formula or recipe from 'recipes', can often be challenging.  The goal of 'workflows' is to streamline this process by bundling the model alongside the preprocessor, all within the same object.")
    (license expat)))

(define-public r-yardstick
  (package
    (name "r-yardstick")
    (version "0.0.5")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "yardstick" version))
        (sha256
          (base32
            "0gmfq1jx8lp6b24clxyj5r1976q3znlchbqa6jk87lyd5fn43f7b"))))
    (properties `((upstream-name . "yardstick")))
    (build-system r-build-system)
    (propagated-inputs
      `(("r-dplyr" ,r-dplyr)
        ("r-generics" ,r-generics)
        ("r-proc" ,r-proc)
        ("r-rcpp" ,r-rcpp)
        ("r-rlang" ,r-rlang)
        ("r-tidyselect" ,r-tidyselect)))
    (home-page
      "https://github.com/tidymodels/yardstick")
    (synopsis
      "Tidy Characterizations of Model Performance")
    (description
      "Tidy tools for quantifying how well model fits to a data set such as confusion matrices, class probability curve summaries, and regression metrics (e.g., RMSE).")
    (license gpl2)))

(define-public r-tune
  (package
    (name "r-tune")
    (version "0.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "tune" version))
        (sha256
          (base32
            "1nfhyw06wfgwx6gkvh5h1slhwk31nwplw16aqvk1ma5g1czg9k3n"))))
    (properties `((upstream-name . "tune")))
    (build-system r-build-system)
    (propagated-inputs
      `(("r-cli" ,r-cli)
        ("r-crayon" ,r-crayon)
        ("r-dials" ,r-dials)
        ("r-dplyr" ,r-dplyr)
        ("r-foreach" ,r-foreach)
        ("r-ggplot2" ,r-ggplot2)
        ("r-glue" ,r-glue)
        ("r-gpfit" ,r-gpfit)
        ("r-hardhat" ,r-hardhat)
        ("r-parsnip" ,r-parsnip)
        ("r-purrr" ,r-purrr)
        ("r-recipes" ,r-recipes)
        ("r-rlang" ,r-rlang)
        ("r-rsample" ,r-rsample)
        ("r-tibble" ,r-tibble)
        ("r-tidyr" ,r-tidyr)
        ("r-workflows" ,r-workflows)
        ("r-yardstick" ,r-yardstick)))
    (home-page "https://github.com/tidymodels/tune")
    (synopsis "Tidy Tuning Tools")
    (description
      "The ability to tune models is important. 'tune' contains functions and classes to be used in conjunction with other 'tidymodels' packages for finding reasonable values of hyper-parameters in models, pre-processing methods, and post-processing steps.")
    (license expat)))

(define-public r-tidyposterior
  (package
    (name "r-tidyposterior")
    (version "0.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "tidyposterior" version))
        (sha256
          (base32
            "1sdbar3ycnjqyjy664zyhr9xks48l6g6mw8p5scp31x3gdh352rs"))))
    (properties `((upstream-name . "tidyposterior")))
    (build-system r-build-system)
    (propagated-inputs
      `(("r-dplyr" ,r-dplyr)
        ("r-generics" ,r-generics)
        ("r-ggplot2" ,r-ggplot2)
        ("r-purrr" ,r-purrr)
        ("r-rlang" ,r-rlang)
        ("r-rsample" ,r-rsample)
        ("r-rstanarm" ,r-rstanarm-mee1) ;; For yknow
        ("r-tibble" ,r-tibble)
        ("r-knitr" ,r-knitr) ;; For vignette builder in test phase
        ("r-tidyr" ,r-tidyr)))
    (home-page
      "https://tidymodels.github.io/tidyposterior")
    (synopsis
      "Bayesian Analysis to Compare Models using Resampling Statistics")
    (description
      "Bayesian analysis used here to answer the question: \"when looking at resampling results, are the differences between models 'real'?\" To answer this, a model can be created were the performance statistic is the resampling statistics (e.g.  accuracy or RMSE).  These values are explained by the model types.  In doing this, we can get parameter estimates for each model's affect on performance and make statistical (and practical) comparisons between models.  The methods included here are similar to Benavoli et al (2017) <http://jmlr.org/papers/v18/16-305.html>.")
    (license gpl2)))

(define-public r-tidypredict
  (package
    (name "r-tidypredict")
    (version "0.4.5")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "tidypredict" version))
        (sha256
          (base32
            "1i6zl6wjz6wbpkmkc9z9ikp8zgck3qh38lar0r6q2jzl8fxpimg4"))))
    (properties `((upstream-name . "tidypredict")))
    (build-system r-build-system)
    (propagated-inputs
      `(("r-dplyr" ,r-dplyr)
        ("r-generics" ,r-generics)
        ("r-knitr" ,r-knitr)
        ("r-purrr" ,r-purrr)
        ("r-rlang" ,r-rlang)
        ("r-tibble" ,r-tibble)))
    (home-page
      "https://tidymodels.github.io/tidypredict")
    (synopsis "Run Predictions Inside the Database")
    (description
      "It parses a fitted 'R' model object, and returns a formula in 'Tidy Eval' code that calculates the predictions.  It works with several databases back-ends because it leverages 'dplyr' and 'dbplyr' for the final 'SQL' translation of the algorithm.  It currently supports lm(), glm(), randomForest(), ranger(), earth(), xgb.Booster.complete(), cubist(), and ctree() models.")
    (license gpl3)))

(define-public r-tidytext
  (package
    (name "r-tidytext")
    (version "0.2.4")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "tidytext" version))
        (sha256
          (base32
            "0gck3f039qkpkwn92jlyfan76w0xydg17bh6nsg9qlba7c35kzs6"))))
    (properties `((upstream-name . "tidytext")))
    (build-system r-build-system)
    (propagated-inputs
      `(("r-dplyr" ,r-dplyr)
        ("r-generics" ,r-generics)
        ("r-hunspell" ,r-hunspell)
        ("r-janeaustenr" ,r-janeaustenr)
        ("r-matrix" ,r-matrix)
        ("r-purrr" ,r-purrr)
        ("r-rlang" ,r-rlang)
        ("r-stopwords" ,r-stopwords)
        ("r-stringr" ,r-stringr)
        ("r-tokenizers" ,r-tokenizers)))
    (native-inputs `(("r-knitr" ,r-knitr)))
    (home-page
      "http://github.com/juliasilge/tidytext")
    (synopsis
      "Text Mining using 'dplyr', 'ggplot2', and Other Tidy Tools")
    (description
      "Text mining for word processing and sentiment analysis using 'dplyr', 'ggplot2', and other tidy tools.")
    (license expat)))

(define-public r-rsample
  (package
    (name "r-rsample")
    (version "0.0.7")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "rsample" version))
        (sha256
          (base32
            "0s6hgq0rcv3ianyidq3n9z34y5ww51gaggqkwmwns9yyxmwfjcm8"))))
    (properties `((upstream-name . "rsample")))
    (build-system r-build-system)
    (propagated-inputs
      `(("r-dplyr" ,r-dplyr)
        ("r-furrr" ,r-furrr)
        ("r-generics" ,r-generics)
        ("r-purrr" ,r-purrr)
        ("r-rlang" ,r-rlang)
        ("r-tibble" ,r-tibble)
        ("r-tidyr" ,r-tidyr)
        ("r-tidyselect" ,r-tidyselect)
        ("r-vctrs" ,r-vctrs)))
    (native-inputs `(("r-knitr" ,r-knitr)))
    (home-page "https://rsample.tidymodels.org")
    (synopsis "General Resampling Infrastructure")
    (description
      "Classes and functions to create and summarize different types of resampling objects (e.g.  bootstrap, cross-validation).")
    (license gpl2)))

(define-public r-parsnip
  (package
    (name "r-parsnip")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "parsnip" version))
        (sha256
          (base32
            "1p33absjd2lnq5aikr42him4b724qzxr1pzvdnazg789f763i47l"))))
    (properties `((upstream-name . "parsnip")))
    (build-system r-build-system)
    (propagated-inputs
      `(("r-dplyr" ,r-dplyr)
        ("r-generics" ,r-generics)
        ("r-globals" ,r-globals)
        ("r-glue" ,r-glue)
        ("r-magrittr" ,r-magrittr)
        ("r-prettyunits" ,r-prettyunits)
        ("r-purrr" ,r-purrr)
        ("r-rlang" ,r-rlang)
        ("r-tibble" ,r-tibble)
        ("r-tidyr" ,r-tidyr)
        ("r-vctrs" ,r-vctrs)))
    (native-inputs `(("r-knitr" ,r-knitr)))
    (home-page
      "https://tidymodels.github.io/parsnip")
    (synopsis
      "A Common API to Modeling and Analysis Functions")
    (description
      "This package provides a common interface is provided to allow users to specify a model without having to remember the different argument names across different functions or computational engines (e.g. 'R', 'Spark', 'Stan', etc).")
    (license gpl2)))

(define-public r-infer
  (package
    (name "r-infer")
    (version "0.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "infer" version))
        (sha256
          (base32
            "0m22bv71hmi3p89p1cx4rjqp7547i4sma8nl12qjvq1d4955lf28"))))
    (properties `((upstream-name . "infer")))
    (build-system r-build-system)
    (propagated-inputs
      `(("r-dplyr" ,r-dplyr)
        ("r-ggplot2" ,r-ggplot2)
        ("r-glue" ,r-glue)
        ("r-lifecycle" ,r-lifecycle)
        ("r-magrittr" ,r-magrittr)
        ("r-purrr" ,r-purrr)
        ("r-rlang" ,r-rlang)
        ("r-tibble" ,r-tibble)))
    (home-page "https://github.com/tidymodels/infer")
    (synopsis "Tidy Statistical Inference")
    (description
      "The objective of this package is to perform inference using an expressive statistical grammar that coheres with the tidy design framework.")
    (license #f)))

(define-public r-tidymodels
  (package
    (name "r-tidymodels")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "tidymodels" version))
        (sha256
          (base32
            "1bi5vh80f6f2ibhyaapgnl7q1mkkx8425vj6ci0ml5rb7l8jhjm8"))))
    (properties `((upstream-name . "tidymodels")))
    (build-system r-build-system)
    (propagated-inputs
      `(("r-broom" ,r-broom)
        ("r-cli" ,r-cli)
        ("r-crayon" ,r-crayon)
        ("r-dials" ,r-dials)
        ("r-dplyr" ,r-dplyr)
        ("r-ggplot2" ,r-ggplot2)
        ("r-infer" ,r-infer)
        ("r-magrittr" ,r-magrittr)
        ("r-parsnip" ,r-parsnip)
        ("r-pillar" ,r-pillar)
        ("r-purrr" ,r-purrr)
        ("r-recipes" ,r-recipes)
        ("r-rlang" ,r-rlang)
        ("r-rsample" ,r-rsample)
        ("r-rstudioapi" ,r-rstudioapi)
        ("r-tibble" ,r-tibble)
        ("r-tidyposterior" ,r-tidyposterior)
        ("r-tidypredict" ,r-tidypredict)
        ("r-tidytext" ,r-tidytext)
        ("r-tune" ,r-tune)
        ("r-workflows" ,r-workflows)
        ("r-yardstick" ,r-yardstick)))
    (home-page
      "https://github.com/tidymodels/tidymodels")
    (synopsis
      "Easily Install and Load the 'Tidymodels' Packages")
    (description
      "The tidy modeling \"verse\" is a collection of packages for modeling and statistical analysis that share the underlying design philosophy, grammar, and data structures of the tidyverse.")
    (license #f)))



;; =============================================================================
;; BRMS AND DEPENDENCIES

(define-public r-bridgesampling
  (package
   (name "r-bridgesampling")
   (version "0.7-2")
   (source
    (origin
     (method url-fetch)
     (uri (cran-uri "bridgesampling" version))
     (sha256
      (base32
       "0kd4p5pz46vssjb8faxwy5nv6r403a89h8hylxhq50n2rj94x934"))))
   (properties
    `((upstream-name . "bridgesampling")))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-brobdingnag" ,r-brobdingnag)
      ("r-coda" ,r-coda)
      ("r-matrix" ,r-matrix)
      ("r-mvnfast" ,r-mvnfast)
      ("r-scales" ,r-scales)
      ("r-stringr" ,r-stringr)))
   (home-page
    "https://github.com/quentingronau/bridgesampling")
   (synopsis
    "Bridge Sampling for Marginal Likelihoods and Bayes Factors")
   (description
    "This package provides functions for estimating marginal likelihoods, Bayes factors, posterior model probabilities, and normalizing constants in general, via different versions of bridge sampling (Meng & Wong, 1996, <http://www3.stat.sinica.edu.tw/statistica/j6n4/j6n43/j6n43.htm>).")
   (license gpl2+)))

(define-public r-mvnfast
  (package
   (name "r-mvnfast")
   (version "0.2.5")
   (source
    (origin
     (method url-fetch)
     (uri (cran-uri "mvnfast" version))
     (sha256
      (base32
       "122zjzr0v4943cax10lp6flwfv479mncvalaj09kb173s5rgmf91"))))
   (properties `((upstream-name . "mvnfast")))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-bh" ,r-bh)
      ("r-rcpp" ,r-rcpp)
      ("r-rcpparmadillo" ,r-rcpparmadillo)))
   (home-page "https://github.com/mfasiolo/mvnfast")
   (synopsis
    "Fast Multivariate Normal and Student's t Methods")
   (description
    "This package provides computationally efficient tools related to the multivariate normal and Student's t distributions.  The main functionalities are: simulating multivariate random vectors, evaluating multivariate normal or Student's t densities and Mahalanobis distances.  These tools are very efficient thanks to the use of C++ code and of the OpenMP API.")
   (license gpl2+)))

(define-public r-brms
  (package
    (name "r-brms")
    (version "2.13.0")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "brms" version))
        (sha256
          (base32
            "0vdncdawxawi16f326qrgy9jjjipmqdjxh741y9p7xdzd4fwaxx3"))))
    (properties `((upstream-name . "brms")))
    (build-system r-build-system)
    (propagated-inputs
      `(("r-abind" ,r-abind)
        ("r-backports" ,r-backports)
        ("r-bayesplot" ,r-bayesplot)
        ("r-bridgesampling" ,r-bridgesampling)
        ("r-coda" ,r-coda)
        ("r-future" ,r-future)
        ("r-ggplot2" ,r-ggplot2)
        ("r-glue" ,r-glue)
        ("r-loo" ,r-loo)
        ("r-matrix" ,r-matrix)
        ("r-matrixstats" ,r-matrixstats)
        ("r-mgcv" ,r-mgcv)
        ("r-nleqslv" ,r-nleqslv)
        ("r-nlme" ,r-nlme)
        ("r-rcpp" ,r-rcpp)
        ("r-rstan" ,r-rstan-mee1)
        ("r-rstantools" ,r-rstantools)
        ("r-shinystan" ,r-shinystan-mee1)))
    (native-inputs `(("r-knitr" ,r-knitr)))
    (home-page
      "https://github.com/paul-buerkner/brms")
    (synopsis
      "Bayesian Regression Models using 'Stan'")
    (description
      "Fit Bayesian generalized (non-)linear multivariate multilevel models using 'Stan' for full Bayesian inference.  A wide range of distributions and link functions are supported, allowing users to fit -- among others -- linear, robust linear, count data, survival, response times, ordinal, zero-inflated, hurdle, and even self-defined mixture models all in a multilevel context.  Further modeling options include non-linear and smooth terms, auto-correlation structures, censored data, meta-analytic standard errors, and quite a few more.  In addition, all parameters of the response distribution can be predicted in order to perform distributional regression.  Prior specifications are flexible and explicitly encourage users to apply prior distributions that actually reflect their beliefs.  Model fit can easily be assessed and compared with posterior predictive checks and leave-one-out cross-validation.  References: B??rkner (2017) <doi:10.18637/jss.v080.i01>; B??rkner (2018) <doi:10.32614/RJ-2018-017>; Carpenter et al. (2017) <doi:10.18637/jss.v076.i01>.")
    (license gpl2)))


;; =============================================================================
;; LANGUAGESERVER

(define-public r-xptr
  (package
    (name "r-xptr")
    (version "1.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "xptr" version))
        (sha256
          (base32
            "19zmzawp4mprwbcq2ajwwmmnf8vgnrkn86m246gz5f7rxzqz0vk3"))))
    (properties `((upstream-name . "xptr")))
    (build-system r-build-system)
    (home-page "https://randy3k.github.io/xptr/")
    (synopsis "Manipulating External Pointer")
    (description
      "There is limited native support for external pointers in the R interface.  This package provides some basic tools to verify, create and modify 'externalptr' objects.")
    (license expat)))

(define-public r-collections
  (package
    (name "r-collections")
    (version "0.2.4")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "collections" version))
        (sha256
          (base32
            "1ar7jmlf42fsfgiqiyxhackrkf7mc0l24fbhsan0wv85dlv0qngw"))))
    (properties `((upstream-name . "collections")))
    (build-system r-build-system)
    (propagated-inputs `(("r-xptr" ,r-xptr)))
    (home-page
      "https://randy3k.github.io/collections")
    (synopsis
      "High Performance Container Data Types")
    (description
      "This package provides high performance container data types such as Queue, Stack, Deque, Dict and OrderedDict.  Benchmarks <https://randy3k.github.io/collections/articles/benchmark.html> have shown that these containers are asymptotically more efficient than those offered by other packages.")
    (license expat)))

(define-public r-languageserver
  (package
    (name "r-languageserver")
    (version "0.3.4")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "languageserver" version))
        (sha256
          (base32
            "1l9vwlc19kj4zavhxsh04iidndjh28fj8qc1dm6187hb755dfjpw"))))
    (properties
      `((upstream-name . "languageserver")))
    (build-system r-build-system)
    (propagated-inputs
      `(("r-callr" ,r-callr)
        ("r-collections" ,r-collections)
        ("r-desc" ,r-desc)
        ("r-fs" ,r-fs)
        ("r-jsonlite" ,r-jsonlite)
        ("r-lintr" ,r-lintr)
        ("r-r6" ,r-r6)
        ("r-readr" ,r-readr)
        ("r-repr" ,r-repr)
        ("r-stringr" ,r-stringr)
        ("r-styler" ,r-styler)
        ("r-xml2" ,r-xml2)
        ("r-xmlparsedata" ,r-xmlparsedata)))
    (home-page
      "https://github.com/REditorSupport/languageserver")
    (synopsis "Language Server Protocol")
    (description
      "An implementation of the Language Server Protocol for R.  The Language Server protocol is used by an editor client to integrate features like auto completion.  See <https://microsoft.github.io/language-server-protocol> for details.")
    (license expat)))



;; =============================================================================
;; TIDYBAYES

(define-public r-svunit
  (package
    (name "r-svunit")
    (version "0.7-12")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "svUnit" version))
        (sha256
          (base32
            "16iiryj3v34zbnk1x05g30paza7al1frqx52fkw8ka61icizsrf5"))))
    (properties `((upstream-name . "svUnit")))
    (build-system r-build-system)
    (home-page "http://www.sciviews.org/SciViews-R")
    (synopsis "SciViews GUI API - Unit testing")
    (description
      "This package provides a complete unit test system and functions to implement its GUI part")
    (license gpl2)))

(define-public r-arrayhelpers
  (package
    (name "r-arrayhelpers")
    (version "1.1-0")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "arrayhelpers" version))
        (sha256
          (base32
            "02rl530qxi1idiqpmzg5wr9vl5c7phizhpj64k5pn8xq9zfxbpaz"))))
    (properties `((upstream-name . "arrayhelpers")))
    (build-system r-build-system)
    (propagated-inputs `(("r-svunit" ,r-svunit)))
    (home-page
      "http://arrayhelpers.r-forge.r-project.org/")
    (synopsis "Convenience Functions for Arrays")
    (description
      "Some convenient functions to work with arrays.")
    (license (list gpl2+ gpl3+))))

(define-public r-hdinterval
  (package
    (name "r-hdinterval")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "HDInterval" version))
        (sha256
          (base32
            "1y51q0dwav3x7c0vp571v89vq1a5mivsxsyahqrv1la12zw0m7il"))))
    (properties `((upstream-name . "HDInterval")))
    (build-system r-build-system)
    (home-page
      "https://cran.r-project.org/web/packages/HDInterval")
    (synopsis
      "Highest (Posterior) Density Intervals")
    (description
      "This package provides a generic function and a set of methods to calculate highest density intervals for a variety of classes of objects which can specify a probability density distribution, including MCMC output, fitted density objects, and functions.")
    (license gpl3)))

(define-public r-tidybayes
  (package
    (name "r-tidybayes")
    (version "2.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "tidybayes" version))
        (sha256
          (base32
            "1hwblvis75n2dvbnp955qsb7wb4cd2q6wfwvkm2ma5piygi0zrh2"))))
    (properties `((upstream-name . "tidybayes")))
    (build-system r-build-system)
    (propagated-inputs
      `(("r-arrayhelpers" ,r-arrayhelpers)
        ("r-coda" ,r-coda)
        ("r-dplyr" ,r-dplyr)
        ("r-forcats" ,r-forcats)
        ("r-ggplot2" ,r-ggplot2)
        ("r-hdinterval" ,r-hdinterval)
        ("r-magrittr" ,r-magrittr)
        ("r-plyr" ,r-plyr)
        ("r-purrr" ,r-purrr)
        ("r-rlang" ,r-rlang)
        ("r-scales" ,r-scales)
        ("r-tibble" ,r-tibble)
        ("r-tidyr" ,r-tidyr)
        ("r-tidyselect" ,r-tidyselect)))
    (native-inputs `(("r-knitr" ,r-knitr)))
    (home-page "http://mjskay.github.io/tidybayes")
    (synopsis
      "Tidy Data and 'Geoms' for Bayesian Models")
    (description
      "Compose data for and extract, manipulate, and visualize posterior draws from Bayesian models ('JAGS', 'Stan', 'rstanarm', 'brms', 'MCMCglmm', 'coda', ...) in a tidy data format.  Functions are provided to help extract tidy data frames of draws from Bayesian models and that generate point summaries and intervals in a tidy format.  In addition, 'ggplot2' 'geoms' and 'stats' are provided for common visualization primitives like points with multiple uncertainty intervals, eye plots (intervals plus densities), and fit curves with multiple, arbitrary uncertainty bands.")
    (license gpl3+)))

;;;
;;; MODELTIME & DEPENDENCIES
;;;

(define-public r-prophet
  (package
    (name "r-prophet")
    (version "0.6.1")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "prophet" version))
        (sha256
          (base32
            "08b5h4c83143q33slsa7x46bxa98rm5dg251zsb7ixvn2v0zkhk1"))))
    (properties `((upstream-name . "prophet")))
    (build-system r-build-system)
    (propagated-inputs
      `(("r-bh" ,r-bh)
        ("r-dplyr" ,r-dplyr)
        ("r-dygraphs" ,r-dygraphs)
        ("r-extradistr" ,r-extradistr)
        ("r-ggplot2" ,r-ggplot2)
        ("r-rcpp" ,r-rcpp)
        ("r-rcppeigen" ,r-rcppeigen)
        ("r-rlang" ,r-rlang)
        ("r-rstan" ,r-rstan)
        ("r-rstantools" ,r-rstantools)
        ("r-scales" ,r-scales)
        ("r-stanheaders" ,r-stanheaders)
        ("r-tidyr" ,r-tidyr)
        ("r-xts" ,r-xts)))
    (native-inputs `(("r-knitr" ,r-knitr)))
    (home-page "https://github.com/facebook/prophet")
    (synopsis "Automatic Forecasting Procedure")
    (description
      "Implements a procedure for forecasting time series data based on an additive model where non-linear trends are fit with yearly, weekly, and daily seasonality, plus holiday effects.  It works best with time series that have strong seasonal effects and several seasons of historical data.  Prophet is robust to missing data and shifts in the trend, and typically handles outliers well.")
    (license expat)))

(define-public r-progressr
  (package
    (name "r-progressr")
    (version "0.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "progressr" version))
        (sha256
          (base32
            "1cvb1llxz7ys4535dfl6hl3j07g24mcvkg0kl46v4nnl7ka4jx67"))))
    (properties `((upstream-name . "progressr")))
    (build-system r-build-system)
    (propagated-inputs `(("r-digest" ,r-digest)))
    (home-page
      "https://github.com/HenrikBengtsson/progressr")
    (synopsis
      "A Inclusive, Unifying API for Progress Updates")
    (description
      "This package provides a minimal, unifying API for scripts and packages to report progress updates from anywhere including when using parallel processing.  The package is designed such that the developer can to focus on what progress should be reported on without having to worry about how to present it.  The end user has full control of how, where, and when to render these progress updates, e.g.  in the terminal using utils::txtProgressBar() or progress::progress_bar(), in a graphical user interface using utils::winProgressBar(), tcltk::tkProgressBar() or shiny::withProgress(), via the speakers using beep::beepr(), or on a file system via the size of a file.  Anyone can add additional, customized, progression handlers.  The 'progressr' package uses R's condition framework for signaling progress updated.  Because of this, progress can be reported from almost anywhere in R, e.g.  from classical for and while loops, from map-reduce APIs like the lapply() family of functions, 'purrr', 'plyr', and 'foreach'.  It will also work with parallel processing via the 'future' framework, e.g.  future.apply::future_lapply(), furrr::future_map(), and 'foreach' with 'doFuture'.  The package is compatible with Shiny applications.")
    (license gpl3+)))

(define-public r-janitor
  (package
    (name "r-janitor")
    (version "2.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "janitor" version))
        (sha256
          (base32
            "1dy8dlvnxg057qxpd5lk30wcxa15vw95888ccd99sqra789llm3n"))))
    (properties `((upstream-name . "janitor")))
    (build-system r-build-system)
    (propagated-inputs
      `(("r-dplyr" ,r-dplyr)
        ("r-lifecycle" ,r-lifecycle)
        ("r-lubridate" ,r-lubridate)
        ("r-magrittr" ,r-magrittr)
        ("r-purrr" ,r-purrr)
        ("r-rlang" ,r-rlang)
        ("r-snakecase" ,r-snakecase)
        ("r-stringi" ,r-stringi)
        ("r-stringr" ,r-stringr)
        ("r-tidyr" ,r-tidyr)
        ("r-tidyselect" ,r-tidyselect)))
    (native-inputs `(("r-knitr" ,r-knitr)))
    (home-page "https://github.com/sfirke/janitor")
    (synopsis
      "Simple Tools for Examining and Cleaning Dirty Data")
    (description
      "The main janitor functions can: perfectly format data.frame column names; provide quick counts of variable combinations (i.e., frequency tables and crosstabs); and isolate duplicate records.  Other janitor functions nicely format the tabulation results.  These tabulate-and-report functions approximate popular features of SPSS and Microsoft Excel.  This package follows the principles of the \"tidyverse\" and works well with the pipe function %>%.  janitor was built with beginning-to-intermediate R users in mind and is optimized for user-friendliness.  Advanced R users can already do everything covered here, but with janitor they can do it faster and save their thinking for the fun stuff.")
    (license expat)))

(define-public r-sass
  (package
    (name "r-sass")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "sass" version))
        (sha256
          (base32
            "0qzrncsnp0zd8jyp4whss92m7llqsfccmp9p9r3gdc7hlq1amp3z"))))
    (properties `((upstream-name . "sass")))
    (build-system r-build-system)
    (propagated-inputs
      `(("r-digest" ,r-digest)
        ("r-fs" ,r-fs)
        ("r-htmltools" ,r-htmltools)
        ("r-rlang" ,r-rlang)))
    (native-inputs `(("r-knitr" ,r-knitr)))
    (home-page "https://github.com/rstudio/sass")
    (synopsis
      "Syntactically Awesome Style Sheets ('Sass')")
    (description
      "An 'SCSS' compiler, powered by the 'LibSass' library.  With this, R developers can use variables, inheritance, and functions to generate dynamic style sheets.  The package uses the 'Sass CSS' extension language, which is stable, powerful, and CSS compatible.")
    (license expat)))

(define-public r-gt
  (package
    (name "r-gt")
    (version "0.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "gt" version))
        (sha256
          (base32
            "081rx851dlkpx6ial6vlbc5fmzqk56sxr66j15yxihncm05b6h8h"))))
    (properties `((upstream-name . "gt")))
    (build-system r-build-system)
    (propagated-inputs
      `(("r-checkmate" ,r-checkmate)
        ("r-commonmark" ,r-commonmark)
        ("r-dplyr" ,r-dplyr)
        ("r-fs" ,r-fs)
        ("r-ggplot2" ,r-ggplot2)
        ("r-glue" ,r-glue)
        ("r-htmltools" ,r-htmltools)
        ("r-magrittr" ,r-magrittr)
        ("r-rlang" ,r-rlang)
        ("r-sass" ,r-sass)
        ("r-scales" ,r-scales)
        ("r-stringr" ,r-stringr)
        ("r-tibble" ,r-tibble)
        ("r-tidyselect" ,r-tidyselect)))
    (home-page "https://github.com/rstudio/gt")
    (synopsis
      "Easily Create Presentation-Ready Display Tables")
    (description
      "Build display tables from tabular data with an easy-to-use set of functions.  With its progressive approach, we can construct display tables with a cohesive set of table parts.  Table values can be formatted using any of the included formatting functions.  Footnotes and cell styles can be precisely added through a location targeting system.  The way in which 'gt' handles things for you means that you don't often have to worry about the fine details.")
    (license expat)))

(define-public r-reactr
  (package
    (name "r-reactr")
    (version "0.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "reactR" version))
        (sha256
          (base32
            "11rahxskch0r5hlqs7iy285dlhrmzm4vl18kbakx4jggwjqh61f5"))))
    (properties `((upstream-name . "reactR")))
    (build-system r-build-system)
    (propagated-inputs
      `(("r-htmltools" ,r-htmltools)))
    (native-inputs `(("r-knitr" ,r-knitr)))
    (home-page "https://github.com/react-R/reactR")
    (synopsis "React Helpers")
    (description
      "Make it easy to use 'React' in R with 'htmlwidget' scaffolds, helper dependency functions, an embedded 'Babel' 'transpiler', and examples.")
    (license expat)))

(define-public r-reactable
  (package
    (name "r-reactable")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "reactable" version))
        (sha256
          (base32
            "037d3za9r9pa5isn7aqi4jzw43ki6i8aq9vir3fmhzwn6lja3z72"))))
    (properties `((upstream-name . "reactable")))
    (build-system r-build-system)
    (propagated-inputs
      `(("r-digest" ,r-digest)
        ("r-htmltools" ,r-htmltools)
        ("r-htmlwidgets" ,r-htmlwidgets)
        ("r-jsonlite" ,r-jsonlite)
        ("r-reactr" ,r-reactr)))
    (home-page "https://glin.github.io/reactable")
    (synopsis
      "Interactive Data Tables Based on 'React Table'")
    (description
      "Interactive data tables for R, based on the 'React Table' JavaScript library.  Provides an HTML widget that can be used in 'R Markdown' documents and 'Shiny' applications, or viewed from an R console.")
    (license expat)))

(define-public r-anytime
  (package
    (name "r-anytime")
    (version "0.3.7")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "anytime" version))
        (sha256
          (base32
            "0vkckxaq1ga73iszwr4lyf12c1cann1w9lhflz4p3xdgx468fzb9"))))
    (properties `((upstream-name . "anytime")))
    (build-system r-build-system)
    (propagated-inputs
      `(("r-bh" ,r-bh) ("r-rcpp" ,r-rcpp)))
    (home-page
      "http://dirk.eddelbuettel.com/code/anytime.html")
    (synopsis
      "Anything to 'POSIXct' or 'Date' Converter")
    (description
      "Convert input in any one of character, integer, numeric, factor, or ordered type into 'POSIXct' (or 'Date') objects, using one of a number of predefined formats, and relying on Boost facilities for date and time parsing.")
    (license gpl2+)))

(define-public r-warp
  (package
    (name "r-warp")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "warp" version))
        (sha256
          (base32
            "16bmym91h0sbbh4iqasqs0f4kp3jqlm3sqgc356mznhxwnsm8dm2"))))
    (properties `((upstream-name . "warp")))
    (build-system r-build-system)
    (native-inputs `(("r-knitr" ,r-knitr)))
    (home-page
      "https://github.com/DavisVaughan/warp")
    (synopsis "Group Dates")
    (description
      "Tooling to group dates by a variety of periods including: yearly, monthly, by second, by week of the month, and more.  The groups are defined in such a way that they also represent the distance between dates in terms of the period.  This extracts valuable information that can be used in further calculations that rely on a specific temporal spacing between observations.")
    (license expat)))

(define-public r-slider
  (package
    (name "r-slider")
    (version "0.1.4")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "slider" version))
        (sha256
          (base32
            "0waa3s6fbr0h7gpap1akvdh3sg5ib4c67rkg91b6y6fpqjrr70xc"))))
    (properties `((upstream-name . "slider")))
    (build-system r-build-system)
    (propagated-inputs
      `(("r-glue" ,r-glue)
        ("r-rlang" ,r-rlang)
        ("r-vctrs" ,r-vctrs)
        ("r-warp" ,r-warp)))
    (native-inputs `(("r-knitr" ,r-knitr)))
    (home-page
      "https://github.com/DavisVaughan/slider")
    (synopsis "Sliding Window Functions")
    (description
      "This package provides type-stable rolling window functions over any R data type.  Cumulative and expanding windows are also supported.  For more advanced usage, an index can be used as a secondary vector that defines how sliding windows are to be created.")
    (license expat)))

(define-public r-padr
  (package
    (name "r-padr")
    (version "0.5.2")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "padr" version))
        (sha256
          (base32
            "0cnsycwd9zpz4apk7rizgyjrg072kqyk4p4q5grdlfzv73ivr7ab"))))
    (properties `((upstream-name . "padr")))
    (build-system r-build-system)
    (propagated-inputs
      `(("r-dplyr" ,r-dplyr)
        ("r-lubridate" ,r-lubridate)
        ("r-rcpp" ,r-rcpp)
        ("r-rlang" ,r-rlang)))
    (native-inputs `(("r-knitr" ,r-knitr)))
    (home-page "https://github.com/EdwinTh/padr")
    (synopsis
      "Quickly Get Datetime Data Ready for Analysis")
    (description
      "Transforms datetime data into a format ready for analysis.  It offers two core functionalities; aggregating data to a higher level interval (thicken) and imputing records where observations were absent (pad).")
    (license expat)))

(define-public r-timetk
  (package
    (name "r-timetk")
    (version "2.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "timetk" version))
        (sha256
          (base32
            "0k1vyv2z48l4ql8isnrzlp4gz3h9j6cscccvbszbayy911bq285m"))))
    (properties `((upstream-name . "timetk")))
    (build-system r-build-system)
    (propagated-inputs
      `(("r-anytime" ,r-anytime)
        ("r-assertthat" ,r-assertthat)
        ("r-dplyr" ,r-dplyr)
        ("r-forcats" ,r-forcats)
        ("r-forecast" ,r-forecast)
        ("r-ggplot2" ,r-ggplot2)
        ("r-hms" ,r-hms)
        ("r-lazyeval" ,r-lazyeval)
        ("r-lubridate" ,r-lubridate)
        ("r-padr" ,r-padr)
        ("r-plotly" ,r-plotly)
        ("r-purrr" ,r-purrr)
        ("r-readr" ,r-readr)
        ("r-recipes" ,r-recipes)
        ("r-rlang" ,r-rlang)
        ("r-rsample" ,r-rsample)
        ("r-slider" ,r-slider)
        ("r-stringi" ,r-stringi)
        ("r-stringr" ,r-stringr)
        ("r-tibble" ,r-tibble)
        ("r-tidyr" ,r-tidyr)
        ("r-tidyselect" ,r-tidyselect)
        ("r-timedate" ,r-timedate)
        ("r-xts" ,r-xts)
        ("r-zoo" ,r-zoo)))
    (native-inputs `(("r-knitr" ,r-knitr)))
    (home-page
      "https://github.com/business-science/timetk")
    (synopsis
      "A Tool Kit for Working with Time Series in R")
    (description
      " Easy visualization, wrangling, and preprocessing of time series data for forecasting and machine learning prediction.  Methods discussed herein are commonplace in machine learning, and have been cited in various literature.  Refer to \"Calendar Effects\" in papers such as Taieb, Souhaib Ben. \"Machine learning strategies for multi-step-ahead time series forecasting.\" Universit Libre de Bruxelles, Belgium (2014): 75-86. <http://souhaib-bentaieb.com/pdf/2014_phd.pdf>.")
    (license gpl3+)))

(define-public r-modeltime
  (package
    (name "r-modeltime")
    (version "0.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "modeltime" version))
        (sha256
          (base32
            "0y52v5n0cv9x3ck25kgdf8sad02v2gida982ahrcm0vjxqxqm5ma"))))
    (properties `((upstream-name . "modeltime")))
    (build-system r-build-system)
    (propagated-inputs
      `(("r-dials" ,r-dials)
        ("r-dplyr" ,r-dplyr)
        ("r-forcats" ,r-forcats)
        ("r-forecast" ,r-forecast)
        ("r-ggplot2" ,r-ggplot2)
        ("r-glue" ,r-glue)
        ("r-gt" ,r-gt)
        ("r-hardhat" ,r-hardhat)
        ("r-janitor" ,r-janitor)
        ("r-magrittr" ,r-magrittr)
        ("r-parsnip" ,r-parsnip)
        ("r-plotly" ,r-plotly)
        ("r-progressr" ,r-progressr)
        ("r-prophet" ,r-prophet)
        ("r-purrr" ,r-purrr)
        ("r-reactable" ,r-reactable)
        ("r-rlang" ,r-rlang)
        ("r-scales" ,r-scales)
        ("r-stringr" ,r-stringr)
        ("r-tibble" ,r-tibble)
        ("r-tidyr" ,r-tidyr)
        ("r-timetk" ,r-timetk)
        ("r-workflows" ,r-workflows)
        ("r-xgboost" ,r-xgboost)
        ("r-yardstick" ,r-yardstick)))
    (native-inputs `(("r-knitr" ,r-knitr)))
    (home-page
      "https://github.com/business-science/modeltime")
    (synopsis
      "The Tidymodels Extension for Time Series Modeling")
    (description
      " The time series forecasting framework for use with the 'tidymodels' ecosystem.  Models include ARIMA, Exponential Smoothing, and additional time series models from the 'forecast' and 'prophet' packages.  Refer to \"Forecasting Principles & Practice, Second edition\" (<https://otexts.com/fpp2/>).  Refer to \"Prophet: forecasting at scale\" (<https://research.fb.com/blog/2017/02/prophet-forecasting-at-scale/>.).")
    (license expat)))
