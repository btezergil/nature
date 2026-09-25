# nature - A simple genetic algorithms library for Clojure(Script)

<a href="https://icons8.com/icon/20873/organic-food"><img src="resources/icons8-nature.png"></a>
[![Clojars Project](https://img.shields.io/clojars/v/nature.svg)](https://clojars.org/nature)
[![Dependencies Status](https://versions.deps.co/nnichols/nature/status.svg)](https://versions.deps.co/nnichols/nature)
[![cljdoc badge](https://cljdoc.org/badge/nature/nature)](https://cljdoc.org/d/nature/nature/CURRENT)
![Github Runner](https://github.com/nnichols/nature/workflows/Clojure%20and%20ClojureScript%20CI/badge.svg)

> Don't you see the danger, John, inherent in what you're doing here?
> Genetic power is the most awesome force the planet's ever seen, but you wield it like a kid that's found his dad's gun.
>
> - Dr. Ian Malcolm from [*Jurassic Park*](https://www.imdb.com/title/tt0107290/)

## Installation

A deployed copy of the most recent version of [nature can be found on clojars.](https://clojars.org/nature)
To use it, add the following as a dependency in your project.clj file:

[![Clojars Project](http://clojars.org/nature/latest-version.svg)](http://clojars.org/nature)

The next time you build your application, [Leiningen](https://leiningen.org/) should pull it automatically.
Alternatively, you may clone or fork the repository to work with it directly.

## Usage

To see how the application works, try executing it:

```bash
~/nature (master)
$ lein run
```

After a while, you should see something like the following response:

```clojure
({:genetic-sequence [1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1],
  :guid 61369be9-7039-4abe-adea-2471b6471ee8,
  :parents [33f23983-c105-409c-999e-41c5ca14e6ae 569dc99e-b7d1-4a47-89c9-7cf88b52f114],
  :age 0,
  :fitness-score 25})
```

Fantastic!
Now, what does all of this mean?

- **genetic-sequence** - A collection of data elements that represents a solution candidate to the problem modeled by your fitness function.
- **guid** - A string representing a [v1 UUID](https://en.wikipedia.org/wiki/Universally_unique_identifier) uniquely identifying this solution candidate.
- **parents** - A collection of one or more guids representing the individuals used to create the individual. If the individual is created during the initialization phase, it is assigned to a vector containing the string ["Initializer"](https://github.com/nnichols/nature/blob/master/src/nature/population_presets.clj) instead.
- **age** - The number of generations an individual has been a member of.
- **fitness-score** - A number representing how well the **genetic-sequence** solves the problem at hand.

These five pieces of information are how nature models an individual.
In this case, the individual we see above is the best solution nature found to our given problem after executing all of the generations we specified.
Now, how do we solve problems of our own?

The core of nature's functionality lies within the `evolve` function.

```clojure
(evolve allele-set genome-length population-size generations fitness-function binary-operators unary-operators)
```

- `allele-set` is a collection of legal genome values.
- `genome-length` is the enforced size of each genetic sequence
- `population-size` is the enforced number of individuals that will be created
- `generations` is the number of iterations the algorithm will cycle through
- `fitness-function` is a partial function accepting generated sequences to evaluate solution qualities
- `binary-operators` is a collection of partial functions accepting and returning 1 or more individuals
- `unary-operators` is a collection of partial functions accepting and returning exactly 1 individual
- `options` an optional map of pre-specified keywords to values that further tune the behavior of nature.

Additionally, and 8th argument may be supplied: `options` is a map that currently checks for two keys:

- `:carry-over` an integer representing the top n individuals to be carried over between each generation. Default is 1.
- `:solutions` an integer representing the top n individuals to return after evolution completes. Default is 1.

So, how do we go from this function to the solution above?
Let's look at a simple example.
Say we commonly work with lists of twenty five binary digits:

```txt
(1 0 0 0 0 0 1 0 1 0 0 0 0 1 0 1 1 0 0 0 0 0 0 0 0)
(0 0 0 0 0 1 0 0 1 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0)
(1 1 0 1 1 0 0 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 0 1)
...
```

As we see each list, we're asked to evaluate each list by counting up the number of times we see a 1 present.

```txt
(1 0 0 0 0 0 1 0 1 0 0 0 0 1 0 1 1 0 0 0 0 0 0 0 0) = 6
(0 0 0 0 0 1 0 0 1 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0) = 4
(1 1 0 1 1 0 0 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 0 1) = 20
...
```

Now we've been given the task to find the list or lists with the highest possible value we can identify before lunch.
Being selectively skilled at mathematics and basic reasoning, we know that 2^25 = 33,554,432 possible lists exist.
It's way too many to check by hand, a close answer is acceptable, and we can easily compare the relative qualities of solutions.
Given these factors, we decide Genetic Algorithms are perfect for the task, and we clone nature to write the following:

```clojure
(evolve pp/binary-genome
        pp/default-sequence-length
        pp/default-population-size
        pp/default-generation-count
        pp/sum-alleles
        [(go/crossover pp/sum-alleles)]
        [(partial go/mutation-operator pp/sum-alleles pp/binary-genome 1)]
        {:solutions 1, :carry-over 5})
```

What luck!
The repository contains code to solve this very problem out of the box!
Now, to document why these settings can help solve each problem, we'll step through each one.

- `pp/binary-genome` is the vector `[0 1]`. Our lists may only contain those two values, so we'll tell nature to build individuals with that domain.
- `pp/default-sequence-length` is the integer 25. Since we're examining lists on 25 elements, our individuals need to match that restriction.
- `pp/default-population-size` is the integer 50. Any positive integer could be used, but there are tradeoffs. Smaller values will execute quicker, and large values will perform a wider search.
- `pp/default-generation-count` is also the integer 50. Any positive integer could be used, but there are tradeoffs just like the population size.
- `pp/sum-alleles` is `(partial apply +)`. We want to add each value in our sequence together, since that is the same as counting the occurrences of 1.
- `[(go/crossover pp/sum-alleles)]` is the binary genetic operator we want to use, including the fitness function it will use to evaluate the solutions it creates. We could have exchanged this with fitness-based scanning, or used both. The choice of operator can make a difference, but we're in a rush so we'll pick arbitrarily.
- `[(partial go/mutation-operator pp/sum-alleles pp/binary-genome 1)]` is the unary genetic operator we want to use, including the fitness function it will use to evaluate the solutions it creates, the set of alleles we can mutate to, and a probability out of 100 that we'll actually make any changes. This, like the binary genetic operator(s), can be tuned based on preference and the problem at hand.
- `{:solutions 1, :carry-over 5}` are the options we've selected. We only want the best solution we find, so we'll pick the top 1. We're also afraid that with such a large search space, we might loose good solutions between generations. We'll decide to keep the top 5 solutions between each round. This are parameters that can be tuned based on use case, but, for now, a few quick picks are fine.

From there, nature will run its course. 50 individuals with random genetic sequences containing a combination of 25 1s and 0s will be created. From there, they'll under go 50 cycles of reproduction and mutation, making sure to preserve the top 5 individuals. Once we've completed, we'll return the cream-of-the crop, and take off for lunch.

## Cooperative coevolution

`evolve-cooperatively` evolves two species independently and derives each
individual's contextual fitness from its collaborations with the other species:

```clojure
(nature/evolve-cooperatively
 {:species-id :rules
  :population-size 20
  :genome-generator generate-rule-genome
  :binary-operators [cross-rule-genomes]
  :unary-operators [mutate-rule-genome]
  :carry-over 2
  :insert-new 1}
 {:species-id :parameters
  :population-size 20
  :genome-generator generate-parameter-genome
  :binary-operators [cross-parameter-genomes]
  :unary-operators [mutate-parameter-genome]}
 50
 score-collaboration
 {:collaboration-mode :balanced
  :opponents 5
  :final-ratio 0.1
  :final-evaluation-fn evaluate-rich-result
  :monitors [record-state]})
```

The exact callback signatures are:

```clojure
(genome-generator)                     ; => genome
(binary-operator parent-genome-a
                 parent-genome-b)      ; => [child-genome ...]
(unary-operator genome)                 ; => genome
(collaboration-fitness-fn genome-a
                          genome-b)     ; => number
(final-evaluation-fn genome-a genome-b) ; => any value
(monitor complete-state)                ; side effects; return value ignored
```

Nature creates individual GUIDs and lineage, applies unary operators to the
children returned by a randomly chosen binary operator, and does not ask genetic
operators to calculate fitness. A species configuration's `:carry-over` and
`:insert-new` default to 1 and 0. The two values must fit within that species'
population size.

Balanced scheduling gives every member exactly `:opponents` distinct opponents,
evaluates each pair once, and therefore requires equal population sizes and a K
between 1 and the population size. `:cartesian` scheduling supports different
population sizes and evaluates every cross-species pair. Contextual fitness defaults to
the arithmetic mean of all collaboration scores credited to an individual and
is cleared and recomputed in every generation, including for elites. On the JVM,
scheduled collaboration fitness calls are evaluated in parallel with `pmap`;
ClojureScript evaluates them sequentially.

### Shared panel collaboration

Version 1.2.0 adds `:panel` to the existing `:collaboration-mode` option:

```clojure
(require '[nature.panel-selectors :as panels]
         '[nature.monitors :as monitors])

(nature/evolve-cooperatively
 species-a species-b 50 score-collaboration
 {:collaboration-mode :panel
  :panel-selection-fns
  [panels/best-fitness
   (panels/random-members 1)
   panels/specialist
   panels/generalist
   (panels/diverse-strong structural-distance)
   panels/historical-best-fitness
   panels/historical-best-average
   panels/historical-best-maximum]
  :monitors [monitors/print-panel-members]})
```

Every live individual faces the same frozen opposite-species panel for one
generation. Each unique pair is evaluated once per generation, even if scheduled
in both directions. Only focal encounters contribute fitness: being a panel
reference does not award extra credit. Fitness defaults to the mean focal encounter score.
Panel scores must be finite numbers. Unequal population sizes are supported.

Omitting `:panel-selection-fns` defaults to one random member per species.
An explicitly empty list, nil, or invalid function list is an error. `:opponents`
only configures balanced mode. Other modes do not invoke panel selectors.

Generation zero uses one randomly sampled bootstrap reference per species.
After each nonterminal generation, selectors use its scored population to build
the next generation's panels, before reproduction. References may therefore be
absent from the next live population. There is no extra bootstrap evaluation
pass. A run with zero generations evaluates the bootstrap panels and does not
invoke the configured selectors.

| Selector | Rule |
| --- | --- |
| `best-fitness` | Highest assigned fitness |
| `(random-members n)` | Up to n random members without replacement |
| `specialist` | Highest single focal encounter score |
| `generalist` | Highest mean focal encounter score |
| `(diverse-strong distance-fn)` | Structurally distinct member of the top fitness quartile |
| `historical-best-fitness` | Best observed assigned fitness across generations |
| `historical-best-average` | Best observed mean across generations |
| `historical-best-maximum` | Best observed single encounter across generations |
| `all-members` | Entire completed population |
| `(ranked-members n score-fn)` | Highest custom scores; score-fn takes individual and context |

Best fitness and generalist coincide under default mean credit. Other credit
policies can separate them. Selections are deduplicated by GUID, keeping the first snapshot and all
selector origins. Panel sizes can shrink through overlap; no automatic backfill
occurs. Counts must be positive integers and are capped at population size.
Current-score ties preserve population order.

For diversity, provide `(distance-fn genome-a genome-b context)` returning a finite
non-negative structural distance; zero means equivalent. Nature considers exactly
`ceil(population-size / 4)` top-fitness candidates, excludes already-selected
GUIDs, and maximizes minimum distance from earlier selections. If there are no
earlier selections, the best-fitness individual is the reference. Candidates
must have strictly positive minimum distance; otherwise the selector returns
no member. Distance ties prefer higher fitness, then population order. Place
this selector after the members it should complement. Nature does not impose a
genome-specific distance algorithm.

Custom selectors receive:

```clojure
(fn [{:keys [generation source-generation species-id species population
             statistics history previous-panel selected-panel]}]
  ;; Return a finite sequence of candidate individual snapshots.
  [(first population)])
```

`generation` is the target generation and `source-generation` the completed one.
`population` is the scored population of this species; `statistics` maps its
GUIDs to `:fitness-score`, `:average-score`, `:maximum-score`, and
`:encounter-count`. `species` is its normalized configuration. `history`
contains `:fitness`, `:average`, and `:maximum` champion records.
`previous-panel` is the completed generation's panel; `selected-panel` holds
earlier selectors' deduplicated choices. The same selector list runs sequentially
for both species; branch on `species-id` for different policies.

Return unchanged snapshots from the scored population, history records'
`:individual`, or previous panel. Unknown/modified snapshots and conflicting
genomes for the same species/GUID are rejected. An individual selector may return
`[]`, but the combined panel must be non-empty.

History retains only three champion records per species. Each includes
`:individual`, `:metric-value`, `:source-generation`, `:statistics`, and
`:opposite-panel-guids`. Strict improvements replace champions; ties keep the
earlier observation. History updates on the terminal generation too. Historical
scores reflect different opponent panels and are not automatically normalized
or re-evaluated. Archived references receive fresh collaboration evaluations
without changing their stored historical scores.

Panel monitor/result state adds `:collaboration-mode`, `:panels`,
`:panel-provenance`, `:panel-statistics`, `:panel-history`, `:next-panels`,
`:next-panel-provenance`, `:directional-collaboration-count`, and
`:unique-collaboration-evaluation-count`. Next-panel fields are nil when terminal.
Directional records add focal/collaborator species IDs and GUIDs to the existing
participants, genomes, and score fields. Panel snapshots carry their observation
fitness; current live fitness is in `:populations`.

`print-panel-members` is an opt-in one-argument cooperative monitor. It logs
current generation, species, member counts, GUIDs, and selector origins, including
bootstrap and historical selections. It ignores non-panel state.
`print-panel-members*` returns the same summary without logging.



### Collaboration credit policies

All collaboration modes accept `:credit-policy` (default `:mean`). Higher assigned credit is
better. Built-ins are `:mean`, `:maximum`, `:top-two-mean`, and `:weighted`:

```clojure
(nature/evolve-cooperatively species-a species-b generations pair-fitness
  {:collaboration-mode :panel
   :panel-selection-fns [(panels/random-members 5)]
   :credit-policy :weighted
   :credit-weights [0.4 0.3 0.15 0.1 0.05]})
```

Top-two mean averages the two highest focal scores, using the sole score during
singleton bootstrap. Weighted credit sorts scores descending (not partner IDs),
then weights them. Weights must be a non-empty finite, non-negative sequence
whose first weight is positive, so singleton encounters are defined. A shorter
encounter list truncates the weight vector and renormalizes its retained weights;
more encounters than weights is an error. Retained weights must have a finite positive
sum. Non-finite scores or resulting credit are errors. Negative scores are valid.
`:credit-weights` is required for `:weighted` and rejected for other policies.
Scheduling and credit are independent. For example, balanced pairing can use
`{:collaboration-mode :balanced :opponents 5 :credit-policy :top-two-mean}`;
Cartesian mode can use `{:collaboration-mode :cartesian :credit-policy :maximum}`.
Balanced scheduling requires at least five individuals per species in that example.
Omitting `:credit-policy` selects mean in every mode. The earlier unreleased
`:panel-credit` / `:panel-credit-weights` option names are rejected; use the general
names above.

A custom `:credit-policy` function receives:

```clojure
{:generation 0
 :collaboration-mode :balanced
 :species-id :a
 :individual focal-individual
 :collaborator-species-id :b
 :encounters focal-directional-records}
```

Encounter records contain participant GUIDs, genomes, focal/collaborator identities,
and `:score`, in scheduler order. Balanced and Cartesian pairs contribute once to
each participant's context. Panel mode supplies only the scheduled focal encounters:
being another individual's reference never adds credit. Panel callbacks additionally
receive `:panel`, aligned with encounter order. Balanced partner sets can differ
between individuals, while panel partners are shared within a species/generation;
a credit policy does not change these scheduling rules.

Return one finite number. The callback runs once per individual,
after pair evaluation and before history, selectors, monitors, and reproduction.
Keep it pure and deterministic: invocation order across species is unspecified.
Failures include generation, species ID, and focal GUID in exception data.

Public `nature.credit/mean`, `maximum`, and `top-two-mean` consume that context;
`(nature.credit/weighted weights)` returns a compatible callback. For example:

```clojure
(require '[nature.credit :as credit])
;; Consumer-defined blend, without extra collaboration evaluations:
{:collaboration-mode :panel
 :credit-policy (fn [context]
                 (+ (* 0.5 (credit/mean context))
                    (* 0.5 (credit/maximum context))))}
```

Assigned `:fitness-score` drives parent sampling, elite carry-over, fitness-based
panel selectors (including diverse-strong's candidate pool), and fitness history.
In panel mode, `:average-score` and `:maximum-score` remain raw encounter
statistics, so generalist,
specialist, and their corresponding historical selectors retain their meanings.
Existing positive-shifted fitness-weighted parent sampling is unchanged. Final
`:final-ratio` shortlisting also uses assigned credit; the subsequent Cartesian
`:final-evaluation-fn` is unchanged. Use ratio 1.0 to include all terminal individuals.

Monitor/result state includes `:credit-policy` and, for weighted credit,
`:credit-weights`. Custom callbacks report `:custom`; the consumer must record
its implementation/version/configuration for reproducibility. Omitting the policy
preserves mean scoring and introduces no random draws. Population-level ranking,
Pareto selection, and even-distributed sorting are not scalar-credit policies and
are not implemented by this interface.


### Final evaluation and results

`:final-ratio` defaults to `1.0` and must be in `(0, 1]`. Nature ranks each final
population by contextual fitness, keeps `ceil(population-size * final-ratio)`,
and enumerates their Cartesian product. If supplied, the final evaluator is
called once per final pair; otherwise the collaboration fitness function is
used. Evaluator return values are stored unchanged under `:result`.

The return value is:

```clojure
{:generation 50
 :populations {:rules [evaluated-individual ...]
               :parameters [evaluated-individual ...]}
 :collaborations [{:participants {:rules guid :parameters guid}
                   :genomes {:rules genome :parameters genome}
                   :score number} ...]
 :solutions {:rules [top-individual ...]
             :parameters [top-individual ...]}
 :final-collaborations
 [{:participants {:rules individual :parameters individual}
   :result caller-value} ...]}
```

Monitors receive the complete generation state (`:generation`, both
`:populations`, and that generation's `:collaborations`) after contextual
fitness has been assigned.

## Documentation Site

For more information on nature, please visit the full-length documentation [here.](https://nnichols.github.io/code/nature/intro)

## Automated Build And Repository Information

API documentation hosted on [cljdoc.](https://cljdoc.org/d/nature/nature/CURRENT)

Code Coverage reports via [Cloverage.](https://nnichols.github.io/nature/coverage/index.html)

## Licensing

Copyright © 2018-2022 [Nick Nichols](https://nnichols.github.io/)

Distributed under the [Eclipse Public License Version 1.0](https://www.eclipse.org/legal/epl-v10.html)

[Organic Food Icon by Icons8](https://icons8.com/icon/20873/organic-food)
