## v1.3.0 / Unreleased

* **Add** - Configurable panel credit with mean (default), maximum, top-two mean, weighted sorted scores, and custom scalar callbacks
* **Add** - Credit policy metadata, finite-value validation, and explicit short-panel weight normalization
* **Update** - Apply assigned credit to reproduction, fitness history, and fitness-based panel selection while preserving raw average/maximum encounter statistics
* **Add** - Credit API documentation and Clojure/ClojureScript regression tests

## v1.2.0 / 2026 Sep 21

* **Add** - Panel collaboration mode with composable selectors, frozen shared panels, focal mean credit, and per-generation pair caching
* **Add** - Best fitness, random, specialist, generalist, diverse strong, and three historical champion selectors, plus generic ranking/all-member helpers
* **Add** - Bounded historical snapshots, panel statistics/provenance, and an opt-in panel member monitor
* **Add** - Panel validation, specs, Clojure/ClojureScript tests, and API examples; existing balanced and Cartesian behavior remains unchanged

## v1.1.0 / 2026 Aug 22

* **Add** - Two-species cooperative coevolution with balanced and Cartesian collaboration scheduling
* **Add** - Deferred contextual fitness, species-local reproduction, final Cartesian evaluation, monitors, specs, and documentation
* **Add** - Parallel JVM evaluation of scheduled collaboration fitness calls
* **Fix** - Use platform-appropriate logging and population mapping in ClojureScript

## v1.0.0 / 2019 Oct 22

> This release migrates nature to a .cljc library

* **Update** - POTENTIALLY BREAKING CHANGE - nature is now a .cljc library

## v0.3.1 / 2019 June 23

> This release adds argument assertions to `evolve`

* **Add** - Pre-conditions for evolve

## v0.3.0 / 2019 June 11

> This release adds common performance monitors

* **Add** - Monitors for individual/population performance reporting

## v0.2.2 / 2019 February 17

> This release adds common fitness-function helpers

* **Add** - Gray Code Binary genome to integer decoder, snt to decimal meshes

## v0.2.1 / 2019 February 16

> This release...

* **Fix** - Include :aot directive in project.clj

## v0.2.0 / 2019 January 15

> This release adds binary genome decoding and several bugfixes

* **Add** - Binary genome to integer decoder
* **Fix** - Crossover point determination

## v0.1.1 / 2019 January 14

> This release refactors the application to make the core function, evolve, stand out more distinctly.

* **Add** - Usage documents
* **Fix** - crossover no longer wipes out some genetic sequences, :carry-over no longer balloons population

## v0.0.1 / 2019 January 12

> This release builds out all initial functionality
