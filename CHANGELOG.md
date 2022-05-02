# Version 1.0.33

* Updating p1_utils to version 1.0.25.
* Fix order in which dependencies are started
* Fix document generation in hex packages

# Version 1.0.32

* Updating p1_utils to version 1.0.23.
* Switch from using Travis to Github Actions as CI
* Fix compatiblity with OTP24

# Version 1.0.31

* Updating p1_utils to version 1.0.22.
* Encode maps

# Version 1.0.30

* Fix problems reported by dialyzer

# Version 1.0.29

* Updating p1_utils to version 1.0.21.

# Version 1.0.28

* Fix hex to support compiling ejabberd with rebar3

# Version 1.0.27

* Updating p1_utils to version 1.0.20.

# Version 1.0.26

* Fix compilation with Erlang/OTP 23.0

# Version 1.0.25

* Updating p1_utils to version 1.0.19.

# Version 1.0.24

* Use github actions for build/test

# Version 1.0.23

* Updating p1_utils to version 1.0.18.
* Add flag that decodes yaml maps to erlang maps (Thanks to Arjan Scherpenisse)
* Add flag that converts special values to erlang equivalents (Thanks to Arjan Scherpenisse)
* Add p1_utils as dependant app

# Version 1.0.22

* Updating p1_utils to version 1.0.17.

# Version 1.0.21

* Fix handling of strings with ' inside them

# Version 1.0.20

* Updating p1_utils to version 1.0.16.
* Handle erlang escape sequences

# Version 1.0.19

* Updating p1_utils to version 1.0.15.
* Don't crash when atom lenght is > 255

# Version 1.0.18

* Updating p1_utils to version 1.0.14.
* Add contribution guide

# Version 1.0.17

* Updating p1_utils to version 1.0.13.

# Version 1.0.16

* Updating p1_utils to version 6ff85e8.
* Add handling of unicode string

# Version 1.0.15

* Updating p1_utils to version 1.0.12.

# Version 1.0.14

* Use http link to p1\_utils dependency

# Version 1.0.13

* Updating p1_utils to version 1.0.11.
* Fix compilation with rebar3

# Version 1.0.12

* Fix using invalid size in allocations

# Version 1.0.11

* Updating p1_utils to version 1.0.10.
* Improved error formatting

# Version 1.0.10

* depends on p1_utils-1.0.9

# Version 1.0.9

* Use p1_utils 1.0.7 (Christophe Romain)

# Version 1.0.8

* Load local .so instead from system package when running tests (Paweł Chmielowski)

# Version 1.0.7

* Use p1_utils 1.0.6 (Christophe Romain)
* Make sure fast_yaml isn't compiled to native code (Holger Weiss)

# Version 1.0.6

* Update to p1_utils 1.0.5 (Mickaël Rémond)

# Version 1.0.5

* Fix compilation on rebar3 (Paweł Chmielowski)

# Version 1.0.4

* Update to p1_utils 1.0.4 (Mickaël Rémond)

# Version 1.0.3

* Improve error message on syntax error (Evgeny Khramtsov)
  We previously had error {unknown_macro, ''}

# Version 1.0.2

* Release with proper dependency on hex.pm (Mickaël Rémond)

# Version 1.0.1

* Update to depend on p1_utils 1.0.3 (Mickaël Rémond)

# Version 1.0.0

* Initial release on Hex.pm (Mickaël Rémond)
* Project is renamed fast_yaml.
* Continuous integration with Travis CI and Coveralls (Mickaël Rémond)
* License changed to Apache v2, so that it can be used in a wide range
  of software.
