case class WeatherDataHK[F[_]](
  temp: F[Double],
  dewPoint: F[Double],
  windSpeed: F[Int]
)

object WeatherDataHK
  import FunctionK.{given _}
  import Monoid.{given _}

  type WeatherData = WeatherDataHK[Id]
  type OptionWeatherData = WeatherDataHK[Option]

  given FunctorK[WeatherDataHK]
    def [F[_], G[_]](weatherData: WeatherDataHK[F]) mapK
       (f: FunctionK[F, G]): WeatherDataHK[G] =
        WeatherDataHK[G](
          f(weatherData.temp), 
          f(weatherData.dewPoint), 
          f(weatherData.windSpeed)
        )

  given weatherDataHKMonoid[F[_]](
    using 
    MD: Monoid[F[Double]],
    MI: Monoid[F[Int]]) as Monoid[WeatherDataHK[F]]
    def combine(x: WeatherDataHK[F], y: WeatherDataHK[F]): WeatherDataHK[F] =
      WeatherDataHK[F](
        MD.combine(x.temp, y.temp), 
        MD.combine(x.dewPoint, y.dewPoint), 
        MI.combine(x.windSpeed, y.windSpeed)
      )

    def unit: WeatherDataHK[F] =
      WeatherDataHK[F](
        MD.unit, 
        MD.unit, 
        MI.unit
      )

  val completeWeatherData: List[WeatherData] = List(
    WeatherDataHK[Id](21.4, 19.9, 4),
    WeatherDataHK[Id](23.1, 19.8, 11),
    WeatherDataHK[Id](26.7, 21.4, 8),
    WeatherDataHK[Id](27.2, 22.0, 8),
    WeatherDataHK[Id](27.7, 21.1, 14)
  )

  val partialWeatherData: List[OptionWeatherData] = List(
    WeatherDataHK[Option](Some(28.6), Some(19.9), None),
    WeatherDataHK[Option](Some(27.1), Some(19.8), Some(66)),
    WeatherDataHK[Option](Some(26.5), Some(21.4), Some(91)),
    WeatherDataHK[Option](Some(26.5), Some(21.4), Some(91)),
    WeatherDataHK[Option](Some(26.5), Some(21.5), Some(91)),
    WeatherDataHK[Option](Some(22.2), None, Some(154)),
    WeatherDataHK[Option](Some(21.8), None, None)
  )
