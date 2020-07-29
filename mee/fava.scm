(define-module (mee fava)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system python)
  #:use-module (gnu packages base)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages finance)
  #:use-module (gnu packages time)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (srfi srfi-1)
  )

(define-public python-pyreadline
  (package
    (name "python-pyreadline")
    (version "2.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pyreadline" version ".zip"))
        (sha256
          (base32
            "1lg255r2qfmaln7dgr3hlb0kg4m07m1n95ppm6qjanz8q8pmjc25"))))
    (build-system python-build-system)
    (native-inputs `(("unzip" ,unzip)))
    ;; A test fails because it tries to import ctypes.wintypes, which are only available on Windows.
    (arguments '(#:tests? #f))
    (home-page "http://ipython.org/pyreadline.html")
    (synopsis
      "A python implmementation of GNU readline.")
    (description
      "A python implmementation of GNU readline.")
    (license license:bsd-3)))

(define-public python-markdown2
  (package
    (name "python-markdown2")
    (version "2.3.9")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "markdown2" version))
        (sha256
          (base32
            "0sns78klc8yf0vihjqwqq40fb7n26m5l6g3qdpkfrrbsj2860ll9"))))
    (build-system python-build-system)
    (home-page
      "https://github.com/trentm/python-markdown2")
    (synopsis
      "A fast and complete Python implementation of Markdown")
    (description
      "A fast and complete Python implementation of Markdown")
    (license license:expat)))

(define-public python-googleapis-common-protos
  (package
    (name "python-googleapis-common-protos")
    (version "1.52.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "googleapis-common-protos" version))
        (sha256
          (base32
            "0lakcsd35qm5x4visvw6z5f1niasv9a0mjyf2bd98wqi0z41c1sn"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-protobuf" ,python-protobuf)))
    (home-page
      "https://github.com/googleapis/python-api-common-protos")
    (synopsis "Common protobufs used in Google APIs")
    (description
      "Common protobufs used in Google APIs")
    (license #f)))

(define-public python-google-api-core
  (package
    (name "python-google-api-core")
    (version "1.22.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "google-api-core" version))
        (sha256
          (base32
            "17gj5xgqj8lxhczck80wz127d31wcwmh3q0d1xqw5nvpx45c9vda"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-futures" ,python-futures)
        ("python-google-auth" ,python-google-auth)
        ("python-googleapis-common-protos"
         ,python-googleapis-common-protos)
        ("python-protobuf" ,python-protobuf)
        ("python-pytz" ,python-pytz)
        ("python-requests" ,python-requests)
        ("python-setuptools" ,python-setuptools)
        ("python-six" ,python-six)))
    (home-page
      "https://github.com/googleapis/python-api-core")
    (synopsis "Google API client core library")
    (description "Google API client core library")
    (license license:asl2.0)))

(define-public python-google-auth-httplib2
  (package
    (name "python-google-auth-httplib2")
    (version "0.0.4")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "google-auth-httplib2" version))
        (sha256
          (base32
            "0fdwnx2yd65f5vhnmn39f4xnxac5j6x0pv2p42qifrdi1z32q2cd"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-google-auth" ,python-google-auth)
        ("python-httplib2" ,python-httplib2)
        ("python-six" ,python-six)))
    (home-page
      "https://github.com/GoogleCloudPlatform/google-auth-library-python-httplib2")
    (synopsis
      "Google Authentication Library: httplib2 transport")
    (description
      "Google Authentication Library: httplib2 transport")
    (license license:asl2.0)))

(define-public python-cachetools
  (package
    (name "python-cachetools")
    (version "4.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "cachetools" version))
        (sha256
          (base32
            "086vkqy084rd0xlfnyyylwr2vp8qrz806aywy9fif06yvv1kkamv"))))
    (build-system python-build-system)
    (home-page "https://github.com/tkem/cachetools/")
    (synopsis
      "Extensible memoizing collections and decorators")
    (description
      "Extensible memoizing collections and decorators")
    (license license:expat)))

(define-public python-google-auth
  (package
    (name "python-google-auth")
    (version "1.19.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "google-auth" version))
        (sha256
          (base32
            "0wrjw1gmrygbppqsrnz9k1c4d3y4l3a2fx4h3m5r949w7n7l817l"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-cachetools" ,python-cachetools)
        ("python-pyasn1-modules" ,python-pyasn1-modules)
        ("python-rsa" ,python-rsa)
        ("python-setuptools" ,python-setuptools)
        ("python-six" ,python-six)))
    (home-page
      "https://github.com/googleapis/google-auth-library-python")
    (synopsis "Google Authentication Library")
    (description "Google Authentication Library")
    (license license:asl2.0)))

(define-public python-google-api-python-client
  (package
    (name "python-google-api-python-client")
    (version "1.10.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "google-api-python-client" version))
        (sha256
          (base32
            "01zzlr21rgl1skl7ayppp0qwn6s883i50xcvxs8jxzr4c5zz097s"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-google-api-core"
         ,python-google-api-core)
        ("python-google-auth" ,python-google-auth)
        ("python-google-auth-httplib2"
         ,python-google-auth-httplib2)
        ("python-httplib2" ,python-httplib2)
        ("python-six" ,python-six)
        ("python-uritemplate" ,python-uritemplate)))
    (home-page
      "https://github.com/googleapis/google-api-python-client/")
    (synopsis "Google API Client Library for Python")
    (description
      "Google API Client Library for Python")
    (license license:asl2.0)))

(define-public python-beancount
  (package
    (name "python-beancount")
    (version "2.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "beancount" version))
        (sha256
          (base32
            "04caa5f40lys93g0h8bizxb41yp3p2jiqiyvb99f735klvhbyap1"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-beautifulsoup4" ,python-beautifulsoup4)
        ("python-bottle" ,python-bottle)
        ("python-chardet" ,python-chardet)
        ("python-dateutil" ,python-dateutil)
        ("python-google-api-python-client"
         ,python-google-api-python-client)
        ("python-lxml" ,python-lxml)
        ("python-magic" ,python-magic)
        ("python-ply" ,python-ply)
        ("python-pytest" ,python-pytest)
        ("python-requests" ,python-requests)))
    (home-page "http://furius.ca/beancount")
    (synopsis "Command-line Double-Entry Accounting")
    (description
      "Command-line Double-Entry Accounting")
    (license #f)))

(define-public python-futures
  (package
    (name "python-futures")
    (version "3.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "futures" version))
        (sha256
          (base32
            "154pvaybk9ncyb1wpcnzgd7ayvvhhzk92ynsas7gadaydbvkl0vy"))))
    (build-system python-build-system)
    (home-page
      "https://github.com/agronholm/pythonfutures")
    (synopsis
      "Backport of the concurrent.futures package from Python 3")
    (description
      "Backport of the concurrent.futures package from Python 3")
    (license #f)))

(define-public python-zope.testing
  (package
    (name "python-zope.testing")
    (version "4.7")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "zope.testing" version))
        (sha256
          (base32
            "1sh3c3i0m8n8fnhqiry0bk3rr356i56ry7calmn57s1pvv8yhsyn"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-setuptools" ,python-setuptools)))
    (home-page
      "https://github.com/zopefoundation/zope.testing")
    (synopsis "Zope testing helpers")
    (description "Zope testing helpers")
    (license #f)))

(define-public python-zc.lockfile
  (package
    (name "python-zc.lockfile")
    (version "2.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "zc.lockfile" version))
        (sha256
          (base32
            "0fr0hdx57dcaa96kxicf5pifmizdhs4fr5j8wrhf52z44y1dfyih"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-setuptools" ,python-setuptools)))
    (native-inputs
      `(("python-zope.testing" ,python-zope.testing)))
    (home-page
      "https://github.com/zopefoundation/zc.lockfile")
    (synopsis "Basic inter-process locks")
    (description "Basic inter-process locks")
    (license #f)))

(define-public python-yg.lockfile
  (package
    (name "python-yg.lockfile")
    (version "2.3")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "yg.lockfile" version))
        (sha256
          (base32
            "0cmp1l80v0fqnm3jnp75hq08x2626z908c5wh9va9aldjn1a9j5q"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-contextlib2" ,python-contextlib2)
        ("python-jaraco.functools"
         ,python-jaraco.functools)
        ("python-tempora" ,python-tempora)
        ("python-zc.lockfile" ,python-zc.lockfile)))
    (native-inputs
      `(("python-pytest" ,python-pytest)
        ("python-pytest-sugar" ,python-pytest-sugar)))
    (home-page
      "https://github.com/yougov/yg.lockfile")
    (synopsis
      "Lockfile object with timeouts and context manager")
    (description
      "Lockfile object with timeouts and context manager")
    (license #f)))

(define-public python-jaraco.apt
  (package
    (name "python-jaraco.apt")
    (version "2.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "jaraco.apt" version))
        (sha256
          (base32
            "19xk6mnldxd7980p91knzsk923lzii9ysqiy1ipgdcl2fh73ddhq"))))
    (build-system python-build-system)
    (propagated-inputs `(("python-six" ,python-six)
                         ("python-setuptools-scm" ,python-setuptools-scm)))
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-pytest-checkdocs"
         ,python-pytest-checkdocs)
        ("python-pytest-flake8" ,python-pytest-flake8)))
    (home-page
      "https://github.com/jaraco/jaraco.apt")
    (synopsis
      "Tools for Debian's APT package manager")
    (description
      "Tools for Debian's APT package manager")
    (license #f)))

(define-public python-jaraco.context
  (package
    (name "python-jaraco.context")
    (version "3.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "jaraco.context" version))
        (sha256
          (base32
            "1agq4ww3l7mx5i2a8n1fyp4inp16nza5ndkp3xfvy0zdpgwxlz8b"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-contextlib2" ,python-contextlib2)
        ("python-jaraco.apt" ,python-jaraco.apt)
        ("python-yg.lockfile" ,python-yg.lockfile)))
    (native-inputs
      `(("python-pytest" ,python-pytest)
        ("python-pytest-black-multipy"
         ,python-pytest-black-multipy)
        ("python-pytest-checkdocs"
         ,python-pytest-checkdocs)
        ("python-pytest-cov" ,python-pytest-cov)
        ("python-pytest-flake8" ,python-pytest-flake8)))
    (home-page
      "https://github.com/jaraco/jaraco.context")
    (synopsis "Context managers by jaraco")
    (description "Context managers by jaraco")
    (license #f)))

(define-public python-pytest-freezegun
  (package
    (name "python-pytest-freezegun")
    (version "0.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pytest-freezegun" version ".zip"))
        (sha256
          (base32
            "0jb92x165z9nckgz11flgdwc3apzbkxq396ajbng66vm6db2vj0r"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-freezegun" ,python-freezegun)
        ("python-pytest" ,python-pytest)))
    (native-inputs `(("unzip" ,unzip)))
    (home-page
      "https://github.com/ktosiek/pytest-freezegun")
    (synopsis
      "Wrap tests with fixtures in freeze_time")
    (description
      "Wrap tests with fixtures in freeze_time")
    (license license:expat)))

(define-public python-backports.unittest-mock
  (package
    (name "python-backports.unittest-mock")
    (version "1.5")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "backports.unittest-mock" version))
        (sha256
          (base32
            "19yd3vrnjbhs9cs8akl5qcdmvlcigdzdbac73ixb5plgvr9qxxgg"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-mock" ,python-mock)))
    (native-inputs
      `(("python-pytest" ,python-pytest)
        ("python-pytest-black-multipy"
         ,python-pytest-black-multipy)
        ("python-pytest-checkdocs"
         ,python-pytest-checkdocs)
        ("python-pytest-flake8" ,python-pytest-flake8)))
    (home-page
      "https://github.com/jaraco/backports.unittest_mock")
    (synopsis
      "Make unittest.mock available on older Pythons")
    (description
      "Make unittest.mock available on older Pythons")
    (license #f)))

(define-public python-mypy-extensions
  (package
    (name "python-mypy-extensions")
    (version "0.4.3")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "mypy-extensions" version))
        (sha256
          (base32
            "1a04qsk8hd1lqns8w1j7cr0vmvbhg450di5k1i16kqxkbf7q30id"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-typing" ,python-typing)))
    (home-page
      "https://github.com/python/mypy_extensions")
    (synopsis
      "Experimental type system extensions for programs checked with the mypy typechecker.")
    (description
      "Experimental type system extensions for programs checked with the mypy typechecker.")
    (license license:expat)))

(define-public python-mypy
  (package
    (name "python-mypy")
    (version "0.782")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "mypy" version))
        (sha256
          (base32
            "030kn709515452n6gy2i1d9fg6fyrkmdz228lfpmbslybsld9xzg"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-mypy-extensions"
         ,python-mypy-extensions)
        ("python-typed-ast" ,python-typed-ast)
        ("python-typing-extensions"
         ,python-typing-extensions)))
    (home-page "http://www.mypy-lang.org/")
    (synopsis "Optional static typing for Python")
    (description "Optional static typing for Python")
    (license license:expat)))

(define-public python-pytest-mypy
  (package
    (name "python-pytest-mypy")
    (version "0.6.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pytest-mypy" version))
        (sha256
          (base32
            "09qg4hkvpg0l506sff85wh0f3j67vw139v0j1n0igfsrgnrajq15"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-filelock" ,python-filelock)
        ("python-mypy" ,python-mypy)
        ("python-pytest" ,python-pytest)))
    (home-page
      "https://github.com/dbader/pytest-mypy")
    (synopsis
      "Mypy static type checker plugin for Pytest")
    (description
      "Mypy static type checker plugin for Pytest")
    (license license:expat)))

(define-public python-tempora
  (package
    (name "python-tempora")
    (version "4.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "tempora" version))
        (sha256
          (base32
            "0vkik48fvadiy8vlp6bw06dh9jzlxj11a8kv9ia2nzrp1f8km6jr"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-jaraco.functools"
         ,python-jaraco.functools)
        ("python-pytz" ,python-pytz)))
    (native-inputs
      `(("python-backports.unittest-mock"
         ,python-backports.unittest-mock)
        ("python-freezegun" ,python-freezegun)
        ("python-pytest" ,python-pytest)
        ("python-pytest-black" ,python-pytest-black)
        ("python-pytest-checkdocs"
         ,python-pytest-checkdocs)
        ("python-pytest-cov" ,python-pytest-cov)
        ("python-pytest-flake8" ,python-pytest-flake8)
        ("python-pytest-freezegun"
         ,python-pytest-freezegun)
        ("python-pytest-mypy" ,python-pytest-mypy)))
    (home-page "https://github.com/jaraco/tempora")
    (synopsis
      "Objects and routines pertaining to date and time (tempora)")
    (description
      "Objects and routines pertaining to date and time (tempora)")
    (license #f)))

(define-public python-portend
  (package
    (name "python-portend")
    (version "2.6")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "portend" version))
        (sha256
          (base32
            "1xdgrnkipv756kq4vg7zlm7vzx5wm1x23m7kwm3r6zp1fm0xa3b0"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-tempora" ,python-tempora)))
    (native-inputs
      `(("python-pytest" ,python-pytest)
        ("python-pytest-black-multipy"
         ,python-pytest-black-multipy)
        ("python-pytest-checkdocs"
         ,python-pytest-checkdocs)
        ("python-pytest-cov" ,python-pytest-cov)
        ("python-pytest-flake8" ,python-pytest-flake8)))
    (home-page "https://github.com/jaraco/portend")
    (synopsis "TCP port monitoring and discovery")
    (description "TCP port monitoring and discovery")
    (license #f)))

(define-public python-requests-unixsocket
  (package
    (name "python-requests-unixsocket")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "requests-unixsocket" version))
        (sha256
          (base32
            "1sn12y4fw1qki5gxy9wg45gmdrxhrndwfndfjxhpiky3mwh1lp4y"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-requests" ,python-requests)
       ("python-pbr" ,python-pbr)
        ("python-urllib3" ,python-urllib3)))
    (home-page
      "https://github.com/msabramo/requests-unixsocket")
    (synopsis
      "Use requests to talk HTTP via a UNIX domain socket")
    (description
      "Use requests to talk HTTP via a UNIX domain socket")
    (license #f)))

(define-public python-trustme
  (package
    (name "python-trustme")
    (version "0.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "trustme" version))
        (sha256
          (base32
            "0v3vr5z6apnfmklf07m45kv5kaqvm6hxrkaqywch57bjd2siiywx"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-cryptography" ,python-cryptography)
        ("python-idna" ,python-idna)
        ("python-ipaddress" ,python-ipaddress)))
    (home-page
      "https://github.com/python-trio/trustme")
    (synopsis
      "#1 quality TLS certs while you wait, for the discerning tester")
    (description
      "#1 quality TLS certs while you wait, for the discerning tester")
    (license #f)))

(define-public python-jaraco.text
  (package
    (name "python-jaraco.text")
    (version "3.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "jaraco.text" version))
        (sha256
          (base32
            "1v0hz3h74m31jlbc5bxwkvrx1h2n7887bajrg1n1c3yc4q8qn1z5"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-importlib-resources"
         ,python-importlib-resources)
        ("python-jaraco.functools"
         ,python-jaraco.functools)
        ("python-six" ,python-six)))
    (native-inputs
      `(("python-pytest" ,python-pytest)
        ("python-pytest-black-multipy"
         ,python-pytest-black-multipy)
        ("python-pytest-checkdocs"
         ,python-pytest-checkdocs)
        ("python-pytest-cov" ,python-pytest-cov)
        ("python-pytest-flake8" ,python-pytest-flake8)))
    (home-page
      "https://github.com/jaraco/jaraco.text")
    (synopsis "Module for text manipulation")
    (description "Module for text manipulation")
    (license #f)))

(define-public python-pytest-watch
  (package
    (name "python-pytest-watch")
    (version "4.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pytest-watch" version))
        (sha256
          (base32
            "1fflnd3varpqy8yzcs451n8h7wmjyx1408qdin5p2qdksl1ny4q6"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-colorama" ,python-colorama)
        ("python-docopt" ,python-docopt)
        ("python-pytest" ,python-pytest)
        ("python-watchdog" ,python-watchdog)))
    (home-page
      "http://github.com/joeyespo/pytest-watch")
    (synopsis
      "Local continuous test runner with pytest and watchdog.")
    (description
      "Local continuous test runner with pytest and watchdog.")
    (license license:expat)))

(define-public python-pytest-testmon
  (package
    (name "python-pytest-testmon")
    (version "1.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pytest-testmon" version))
        (sha256
          (base32
            "1iasz23zrzjgbak8jiq12i4zmkk8f6dmcdhfxz8m2q03agcidc7x"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-coverage" ,python-coverage)
       ("python-tox" ,python-tox)
       ("python-unittest-mixins" ,python-unittest-mixins)
       ("python-pytest" ,python-pytest)))
    (home-page "https://testmon.org")
    (synopsis
      "selects tests affected by changed files and methods")
    (description
      "selects tests affected by changed files and methods")
    (license #f)))

(define-public python-selectors2
  (package
    (name "python-selectors2")
    (version "2.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "selectors2" version))
        (sha256
          (base32
            "0ayqs59yz7mqnlz8g9ggik69h1n5j9p5l6ywa74gn8rs42nbl6qz"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-psutil" ,python-psutil)
       ("python-mock" ,python-mock)
       ))
    (home-page
      "https://www.github.com/sethmlarson/selectors2")
    (synopsis
      "Back-ported, durable, and portable selectors")
    (description
      "Back-ported, durable, and portable selectors")
    (license license:expat)))

(define-public python-backports.functools-lru-cache
  (package
    (name "python-backports.functools-lru-cache")
    (version "1.6.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri
               "backports.functools-lru-cache"
               version))
        (sha256
          (base32
            "0jidrkk2w6bhjm197plxiaxrav64mgcrign0bfyr7md2ilc5zplg"))))
    (build-system python-build-system)
    (native-inputs
      `(("python-pytest" ,python-pytest)
        ("python-pytest-black-multipy"
         ,python-pytest-black-multipy)
        ("python-pytest-checkdocs"
         ,python-pytest-checkdocs)
        ("python-pytest-cov" ,python-pytest-cov)
        ("python-pytest-flake8" ,python-pytest-flake8)))
    (home-page
      "https://github.com/jaraco/backports.functools_lru_cache")
    (synopsis "Backport of functools.lru_cache")
    (description "Backport of functools.lru_cache")
    (license #f)))

(define-public python-pytest-black-multipy
  (package
    (name "python-pytest-black-multipy")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pytest-black-multipy" version))
        (sha256
          (base32
            "1ciwa99fnz3ngbsvcjvxqz4k1vwfmvpxaj7qf5vxkx0awvczhsyd"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-pytest-black" ,python-pytest-black)
       ("python-setuptools" ,python-setuptools)
       ("python-setuptools-scm" ,python-setuptools-scm)))
    (native-inputs
      `(("python-pytest" ,python-pytest)
        ("python-pytest-checkdocs"
         ,python-pytest-checkdocs)
        ("python-pytest-flake8" ,python-pytest-flake8)))
    (home-page "https://github.com/jaraco/skeleton")
    (synopsis "Allow '--black' on older Pythons")
    (description "Allow '--black' on older Pythons")
    (license #f)))

(define-public python-jaraco.classes
  (package
    (name "python-jaraco.classes")
    (version "3.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "jaraco.classes" version))
        (sha256
          (base32
            "1avsxzm5mwylmy2zbxq3xvn48z5djb0qy3hwv4ryncprivzri1n3"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-more-itertools" ,python-more-itertools)))
    (native-inputs
      `(("python-pytest" ,python-pytest)
        ("python-pytest-black-multipy"
         ,python-pytest-black-multipy)
        ("python-pytest-checkdocs"
         ,python-pytest-checkdocs)
        ("python-pytest-cov" ,python-pytest-cov)
        ("python-pytest-flake8" ,python-pytest-flake8)))
    (home-page
      "https://github.com/jaraco/jaraco.classes")
    (synopsis
      "Utility functions for Python class constructs")
    (description
      "Utility functions for Python class constructs")
    (license #f)))

(define-public python-jaraco.functools
  (package
    (name "python-jaraco.functools")
    (version "3.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "jaraco.functools" version))
        (sha256
          (base32
            "1x4l7d4mvr94nfzh4zgvkdcglvvagbx2y6ryw2ijql8p66zc9vcz"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-setuptools-scm" ,python-setuptools-scm)
       ("python-more-itertools" ,python-more-itertools)))
    (native-inputs
      `(("python-jaraco.classes" ,python-jaraco.classes)
        ("python-pytest" ,python-pytest)
        ("python-pytest-black" ,python-pytest-black)
        ("python-pytest-checkdocs"
         ,python-pytest-checkdocs)
        ("python-pytest-cov" ,python-pytest-cov)
        ("python-pytest-flake8" ,python-pytest-flake8)))
    (home-page
      "https://github.com/jaraco/jaraco.functools")
    (synopsis "Functools like those found in stdlib")
    (description
      "Functools like those found in stdlib")
    (license #f)))

(define-public python-cheroot
  (package
    (name "python-cheroot")
    (version "8.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "cheroot" version))
        (sha256
          (base32
            "139arji72rjywwp5nky2iwwqnhjjlvdmkss5c9s41qspdy1m3arc"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-backports.functools-lru-cache"
         ,python-backports.functools-lru-cache)
        ("python-jaraco.functools"
         ,python-jaraco.functools)
        ("python-more-itertools" ,python-more-itertools)
        ("python-selectors2" ,python-selectors2)
        ("python-six" ,python-six)))
    (native-inputs
      `(("python-codecov" ,python-codecov)
        ("python-colorama" ,python-colorama)
        ("python-coverage" ,python-coverage)
        ("python2-futures" ,python2-futures)
        ("python-jaraco.context" ,python-jaraco.context)
        ("python-jaraco.text" ,python-jaraco.text)
        ("python-portend" ,python-portend)
        ("python-pyopenssl" ,python-pyopenssl)
        ("python-pytest" ,python-pytest)
        ("python-pytest-cov" ,python-pytest-cov)
        ("python-pytest-forked" ,python-pytest-forked)
        ("python-pytest-mock" ,python-pytest-mock)
        ("python-pytest-sugar" ,python-pytest-sugar)
        ("python-pytest-testmon" ,python-pytest-testmon)
        ("python-pytest-watch" ,python-pytest-watch)
        ("python-pytest-xdist" ,python-pytest-xdist)
        ("python-requests-toolbelt"
         ,python-requests-toolbelt)
        ("python-requests-unixsocket"
         ,python-requests-unixsocket)
        ("python-trustme" ,python-trustme)
        ("python-urllib3" ,python-urllib3)))
    (home-page "https://cheroot.cherrypy.org")
    (synopsis
      "Highly-optimized, pure-python HTTP server")
    (description
      "Highly-optimized, pure-python HTTP server")
    (license #f)))

(define-public python-fava
  (package
    (name "python-fava")
    (version "1.15")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "fava" version))
        (sha256
          (base32
            "037fhimfjiqay28wg7hd5sm7r0935ynw9d905iszn965ihr1qsgz"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-babel" ,python-babel)
        ("python-beancount" ,beancount)
        ("python-cheroot" ,python-cheroot)
        ("python-click" ,python-click)
        ("python-flask" ,python-flask)
        ("python-flask-babel" ,python-flask-babel)
        ("python-jinja2" ,python-jinja2)
        ("python-markdown2" ,python-markdown2)
        ("python-ply" ,python-ply)
        ("python-pyreadline" ,python-pyreadline)
        ("python-simplejson" ,python-simplejson)
        ("python-werkzeug" ,python-werkzeug)))
    (home-page "https://beancount.github.io/fava/")
    (synopsis
      "Web interface for the accounting tool Beancount.")
    (description
      "Web interface for the accounting tool Beancount.")
    (license license:expat)))

(define-public python-unittest-mixins
  (package
    (name "python-unittest-mixins")
    (version "1.6")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "unittest-mixins" version))
        (sha256
          (base32
            "01r5qn9nbjpkplg3y2xiij38q1s6237d0wbdpfvj81wz16z3ldh5"))))
    (build-system python-build-system)
    (propagated-inputs `(("python-six" ,python-six)))
    (home-page
      "https://github.com/nedbat/unittest-mixins")
    (synopsis "Helpful mixins for unittest classes")
    (description
      "Helpful mixins for unittest classes")
    (license license:asl2.0)))
