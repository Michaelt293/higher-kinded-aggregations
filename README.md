## Aggregations using higher-kinded data

For the purpose of discussion, the case class, `WeatherDataHK` parameterised by `F[_]]`, will be used.

```scala
case class WeatherDataHK[F[_]](
  temp: F[Double],
  dewPoint: F[Double],
  windSpeed: F[Int]
)
```

With this data representation, the type of `F` communicates important information -

* `WeatherDataHK[Id]` - each field contains exactly one element
* `WeatherDataHK[Option]` - each field contains zero or one element
* `WeatherDataHK[List]` - each field zero or more elements
* `WeatherDataHK[Set]` - each field zero or more unique elements

With this in mind, a `List` of `WeatherDataHK[Id]` could be folded into a single `WeatherDataHK[List]` value where each field contains a list of elements. Alternatively, if we fold a `List` of `WeatherDataHK[Id]` into a value of type `WeatherDataHK[Set]`, we would collect only distinct elements.

To perform these aggregations, three capabilities are required: 

1) A way to transform `Id[A]` to `List[A]` or `Set[A]`. This can be achieved using a given instance of the `FunctionK` trait.
                                                        
```scala
trait FunctionK[F[_], G[_]]
  def apply[A](fa: F[A]): G[A]

given FunctionK[Id, Set]
  def apply[A](id: Id[A]): Set[A] =
    Set(id)
```

2) A way to transform `WeatherDataHK[Id]` to `WeatherDataHK[List]` or `WeatherDataHK[Set]`. This can be achieved by defining a `FunctorK` instance for `WeatherDataHK`.

```scala
trait FunctorK[HK[_[_]]]
  def [F[_], G[_]](fa: HK[F]) mapK (f: FunctionK[F, G]): HK[G]
  
given FunctorK[WeatherDataHK]
  def [F[_], G[_]](weatherData: WeatherDataHK[F]) mapK
     (f: FunctionK[F, G]): WeatherDataHK[G] =
      WeatherDataHK[G](
        f(weatherData.temp), 
        f(weatherData.dewPoint), 
        f(weatherData.windSpeed)
      )
```

3) A `Monoid` instance for `WeatherDataHK[List]` or `WeatherDataHK[Set]`.

With this in place, it is possible to define extension methods on sequences containing higher-kinded data. For example, `mconcat` has a type parameter `G` which selects the aggregating monoid. In addition, with a given instance for `IsoK` (representing a higher-kinded isomorphism), it is possible to define `mconcatVia` which performs the aggregation via the selected monoid.
 
 
```scala
implicit class SeqHKOps[HK[_[_]], F[_]](xs: Seq[HK[F]]) extends AnyVal
  def foldMapK[G[_]](f: FunctionK[F, G])(
    given M: Monoid[HK[G]], F: FunctorK[HK]): HK[G] =
      xs.foldLeft(M.unit)((acc, x) => M.combine(acc, (x mapK f)))

  def mconcat[G[_]](
    given 
    M: Monoid[HK[G]], 
    F: FunctorK[HK],
    FK: FunctionK[F, G]): HK[G] =
      foldMapK(FK)
      
  def mconcatVia[G[_]](
    given 
    M: Monoid[HK[G]], 
    F: FunctorK[HK],
    I: IsoK[F, G]): HK[F] =
      foldMapK(I) mapK I.from
```

### Examples

For the sake of this example, let's say we have some weather data, `completeWeatherData` -

```scala
val completeWeatherData: List[WeatherData] = List(
  WeatherDataHK[Id](21.4, 19.9, 4),
  WeatherDataHK[Id](23.1, 19.8, 11),
  WeatherDataHK[Id](26.7, 21.4, 8),
  WeatherDataHK[Id](27.2, 22.0, 8),
  WeatherDataHK[Id](27.7, 21.1, 14)
)
```

It is possible perform aggregations using the `Max`, `Min` and `Mean` data types (`Monoid` instances are provided in their companion objects) -

```scala
scala> completeWeatherData.mconcat[Min]
val res0: WeatherDataHK[Min] = WeatherDataHK(Min(Some(21.4)),Min(Some(19.8)),Min(Some(4)))

scala> completeWeatherData.mconcat[Max]
val res1: WeatherDataHK[Max] = WeatherDataHK(Max(Some(27.7)),Max(Some(22.0)),Max(Some(14)))

scala> completeWeatherData.mconcat[Mean]
val res2: WeatherDataHK[Mean] = WeatherDataHK(Mean(Some(126.10000000000001),5),Mean(Some(104.19999999999999),5),Mean(Some(45),5))
```

It is also possible to perform multiple aggregations in a single pass over the data by using the `Monoid` and `FunctionK` instances for tuples -

```scala
scala> completeWeatherData.mconcat[[x] =>> (Min[x], Max[x], Mean[x])]
val res3: WeatherDataHK[[x] => (Min[x], Max[x], Mean[x])] = WeatherDataHK((Min(Some(21.4)),Max(Some(27.7)),Mean(Some(126.10000000000001),5)),(Min(Some(19.8)),Max(Some(22.0)),Mean(Some(104.19999999999999),5)),(Min(Some(4)),Max(Some(14)),Mean(Some(45),5)))
```

Since given instances for `IsoK[Id, Sum]` and `IsoK[Id, Product]` have been defined, it is also possible to perform aggregations via `Sum` and `Product` (for example) -

```scala
scala> completeWeatherData.mconcatVia[Sum]                                                                     
val res12: WeatherDataHK[[A] => A] = WeatherDataHK(126.10000000000001,104.19999999999999,45)

scala> completeWeatherData.mconcatVia[Product]                                                                 
val res13: WeatherDataHK[[A] => A] = WeatherDataHK(9944562.640319997,3914147.3976,39424)
```
Many other aggregating monoids can be defined allowing a wide variety of useful aggregations to be performed using this approach.

## Possible improvements

It should be possible to derive `Monoid` and `FunctorK` instances for higher-kinded data. This would reduce boilerplate code and therefore improve ergonomics. It should also be possible to use opaque types for monoids such as `Max` and `Sum` etc. to improve performance. Unexpected behaviour was encountered, however, when attempting to use opaque types.
