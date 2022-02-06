defmodule SpaceAge do
  @type planet ::
          :mercury
          | :venus
          | :earth
          | :mars
          | :jupiter
          | :saturn
          | :uranus
          | :neptune

  @seconds_per_earth_year 31_557_600

  @orbital_period %{
    mercury:   0.2408467,
    venus:     0.61519726,
    earth:     1.0,
    mars:      1.8808158,
    jupiter:  11.862615,
    saturn:   29.447498,
    uranus:   84.016846,
    neptune: 164.79132,
  }

  @doc """
  Return the number of years a person that has lived for 'seconds' seconds is
  aged on 'planet', or an error if 'planet' is not a planet.
  """
  @spec age_on(planet, pos_integer) :: {:ok, float} | {:error, String.t()}
  def age_on(planet, seconds) do
    case @orbital_period[planet] do
      nil -> {:error, "not a planet"}
      p   -> {:ok, seconds / @seconds_per_earth_year / p}
    end
  end
end
