# cardano
Models random variables and bayesian nets using monads in Scala. Inference is performed by sampling.

[![Build Status](https://travis-ci.org/jrj-d/cardano.svg)](https://travis-ci.org/jrj-d/cardano) [![codecov](https://codecov.io/gh/jrj-d/cardano/branch/master/graph/badge.svg)](https://codecov.io/gh/jrj-d/cardano)

## Installation

Include this dependency in your `sbt` configuration:

```
"org.jrj-d" %% "cardano" % "0.2.3"
```

## Usage

The scaladoc is available [here](https://oss.sonatype.org/service/local/repositories/releases/archive/org/jrj-d/cardano_2.12/0.2.3/cardano_2.12-0.2.3-javadoc.jar/!/index.html).

### Random variables as monads

`cardano` models random variable as [monads](https://medium.com/@sinisalouc/demystifying-the-monad-in-scala-cc716bb6f534).
This should feel familiar to most Scala programmers since the `collection` library is built this way.

The base trait is `Stochastic[A]` which defines a random variable of concrete type `A`.

```scala
scala> def die: Stochastic[Int] = Stochastic.discreteUniform(6).map(_ + 1)
die: cardano.Stochastic[Int] = ...
```

The above line creates a uniform discrete random variable from 0 to 5 (`Stochastic.discreteUniform(6)`).
Applying `map(_ + 1)` on it effectively creates a fair die with values from 1 to 6.

Let's test our die by computing its expectation and the probability of yielding a 1.

```scala
scala> die.expectation
res0: Double = 3.554

scala> die.map(_ == 1).expectation
res1: Double = 0.156

scala> die.map(_ == 1).expectation(10000)
res2: Double = 0.1667
```

The number of samples defaults to 1000 when no argument is given.

The `filter` operation allows us to compute conditional probabilities.
The expectation of a die given that the value is less than 3:

```scala
scala> die.filter(_ < 3).expectation
res16: Double = 1.504
```

To model the sum of two dice, we will combine two dice using `flatMap`:

```scala
scala> val sumDice = die.flatMap(v => die.map(_ + v))
sumDice: cardano.Stochastic[Int] = ...

scala> sumDice.expectation
res21: Double = 7.062

scala> sumDice.std
res23: Double = 2.40265041152474
```

That's it! Along with `Stochastic.constant(...)` that allows us to build constant random variables, all the standard monadic Scala operations are here.

Let's combine all of this in a solution to the [Monty Hall problem](https://en.wikipedia.org/wiki/Monty_Hall_problem).
Thanks to Scala syntactic sugar, we can define random variables in for loops:

```scala
scala> :paste
// Entering paste mode (ctrl-D to finish)

val keepStrategy = for(
    winningDoor <- Stochastic.discreteUniform(3);
    firstChosenDoor <- Stochastic.discreteUniform(3);
    revealedDoor <- Stochastic.discreteUniform(3)
      if revealedDoor != firstChosenDoor && revealedDoor != winningDoor
) yield winningDoor == firstChosenDoor

val changeStrategy = for(
    winningDoor <- Stochastic.discreteUniform(3);
    firstChosenDoor <- Stochastic.discreteUniform(3);
    revealedDoor <- Stochastic.discreteUniform(3)
      if revealedDoor != firstChosenDoor && revealedDoor != winningDoor
) yield (winningDoor != firstChosenDoor && winningDoor != revealedDoor)

// Exiting paste mode, now interpreting.

keepStrategy: cardano.Stochastic[Boolean] = ...
changeStrategy: cardano.Stochastic[Boolean] = ...

scala> keepStrategy.expectation
res65: Double = 0.311

scala> changeStrategy.expectation
res66: Double = 0.652
```

Indeed, if the player changes door after the host reveals a loosing door, they have a probability of winning of 2/3, 1/3 otherwise.

### Duplicating random variables

What if we want to study the rolls of 5 dice?
Chaining `flatMap` would be tedious.

Method `repeat` of `Stochastic[A]` allows us to duplicate a random variable.
The duplicates are identically distributed and independent.

`repeat` takes as input a function of signature `(=> A) => F[A]` for some higher-kinded type `F`.
Let's create a random variable of a sequence of 5 dice:

```scala
scala> val fiveDice = die.repeat(Seq.fill[Int](5))
fiveDice: cardano.Stochastic[Seq[Int]] = ...

scala> fiveDice.sample
res24: Seq[Int] = List(6, 2, 6, 4, 5)

scala> fiveDice.sample
res25: Seq[Int] = List(1, 2, 2, 6, 5)

scala> fiveDice.map(s => s.distinct.size == s.size).expectation
res40: Double = 0.0933
```

The last line gives an estimate of the probability that all dice have a different value.

`repeat` allows us more exotic constructs like an infinite sequence of die rolls:

```scala
scala> val dice = die.repeat(Stream.continually[Int])
dice: cardano.Stochastic[scala.collection.immutable.Stream[Int]] = ...

scala> dice.sample.take(10).toList
res46: List[Int] = List(5, 2, 5, 6, 1, 5, 2, 6, 3, 3)

scala> dice.sample.take(10).toList
res47: List[Int] = List(4, 4, 2, 4, 2, 6, 4, 5, 4, 5)
```

This allows us to estimate the number of dice required to reach a given sum, say 30:

```scala
scala> val runningSum = dice.map(_.scan(0)(_ + _))
runningSum: cardano.Stochastic[scala.collection.immutable.Stream[Int]] = ...

scala> runningSum.sample.take(10).toList
res48: List[Int] = List(0, 6, 10, 16, 21, 22, 27, 32, 37, 42)

scala> val nbDice = runningSum.map(_.takeWhile(_ < 30).length)
nbDice: cardano.Stochastic[Int] = ...

scala> nbDice.expectation
res49: Double = 9.014

scala> nbDice.std
res51: Double = 1.4758045263516417
```

### Markov chain

Method `markov` of `Stochastic` creates a Markov chain of the first order.
It takes as input a function indicating the relationship between two consecutive terms.

Here is a model of the Californian weather (90% chance of a sunny day after a sunny day, 50% after a rainy day):

```scala
scala> :paste
// Entering paste mode (ctrl-D to finish)

sealed trait Weather
case object Sunny extends Weather
case object Rainy extends Weather

def boolToWeather(b: Boolean): Weather = if(b) Sunny else Rainy

def transition(weather: Weather): Stochastic[Weather] = weather match {
  case Sunny => Stochastic.coin(0.9).map(boolToWeather)
  case Rainy => Stochastic.coin(0.5).map(boolToWeather)
}

val weatherChain: Stochastic[Stream[Weather]] = Stochastic.constant[Weather](Sunny).markov(transition)

// Exiting paste mode, now interpreting.

defined trait Weather
defined object Sunny
defined object Rainy
boolToWeather: (b: Boolean)Weather
transition: (weather: Weather)cardano.Stochastic[Weather]
weatherChain: cardano.Stochastic[Stream[Weather]] = ...

scala> weatherChain.sample.take(10).toList
res54: List[Weather] = List(Sunny, Sunny, Rainy, Sunny, Sunny, Sunny, Sunny, Sunny, Sunny, Rainy)
```

### Bayesian inference

Sampling is often used to compute posteriors in intractable cases.
For the sake of brevity, we will work here with a toy example that is in fact tractable.

We want to determine the bias of a coin by repeatedly flipping it.
The prior is a Beta distribution with a rather flat belief in dealing with a fair coin (2, 2).

```scala
scala> :paste
// Entering paste mode (ctrl-D to finish)

val unknownCoin = Stochastic.coin(0.6)

val prior = Stochastic.beta(2, 2)

val observations = Seq.fill(100)(unknownCoin.sample)

val posterior = Stochastic.posterior(prior, observations) { (bias, observation) =>
    if(observation) bias else 1 - bias
}

// Exiting paste mode, now interpreting.

unknownCoin: cardano.Stochastic[Boolean] = ...
prior: cardano.Stochastic[Double] = ...
observations: Seq[Boolean] = ...

scala> posterior.expectation
res55: Double = 0.6051810933353833

scala> posterior.std
res56: Double = 0.0464702905875049
```

See also `posteriorByLog` where the log-likelihood is provided by the user.

### MCMC sampling

`cardano` includes the Metropolis-Hastings and Metropolis sampling procedures, along with a facility to build maximum entropy distributions.
This can be especially useful when you only have access to the unnormalized density of the distribution.

Suppose we don't have `Stochastic.beta` but we want to create a Beta random variable.
Metropolis-Hastings is a sampling procedure that builds a Markov chain whose equilibrium distribution is the target distribution.
It takes as input as a transition function and an unnormalized target density.

The transition function consists in adding a small uniform noise to the current value and capping it to [0,1].
The unnormalized target distribution can be found on [Wikipedia](https://en.wikipedia.org/wiki/Beta_distribution).

```scala
scala> :paste
// Entering paste mode (ctrl-D to finish)

def logDensity(a: Double, b: Double, value: Double) = (a - 1) * math.log(value) + (b - 1) * math.log(1 - value)

def cap(value: Double) = math.min(1.0, math.max(0.0, value))

val metropolisBeta = Stochastic.metropolis(Stochastic.constant(0.5))(logDensity(20, 10, _)) { lastValue =>
    Stochastic.continuousUniform.map(noise => cap(lastValue + 0.1 * noise - 0.05))
}

val exactBeta = Stochastic.beta(20, 10)

// Exiting paste mode, now interpreting.

logDensity: (a: Double, b: Double, value: Double)Double
cap: (value: Double)Double
metropolisBeta: cardano.Stochastic[Double] = cardano.metropolis.MetropolisDistributions$$anon$1@1aca31a1
exactBeta: cardano.Stochastic[Double] = cardano.continuous.ContinuousDistributions$$anon$2@9be85d1

scala> metropolis
metropolis   metropolisBeta

scala> metropolisBeta.expectation
res61: Double = 0.6743298870740473

scala> exactBeta.expectation
res62: Double = 0.6646562222468744

scala> metropolisBeta.std
res63: Double = 0.08492853676864531

scala> exactBeta.std
res64: Double = 0.0850264034989098
```

Refer to the scaladoc to set the burn-in period and the interval between two consecutive samples.

Note that the Bayesian inference capabilities of `cardano` exposed through methods `posterior` and `posteriorByLog` builds upon those MCMC sampling techniques.
