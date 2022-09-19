class NotMovieClubMemberError < RuntimeError
end

class Moviegoer
  attr_reader :age, :member

  MOVIE_PRICE = 15
  MOVIE_PRICE_SENIOR = 10
  AGE_ADULT = 18
  AGE_SENIOR = 60

  def initialize(age, member: false)
    @age = age
    @member = member
  end

  def senior?
    age >= AGE_SENIOR
  end

  def adult?
    age >= AGE_ADULT
  end

  def ticket_price
    senior? ? MOVIE_PRICE_SENIOR : MOVIE_PRICE
  end

  def watch_scary_movie?
    adult?
  end

  # Popcorn is 🍿
  def claim_free_popcorn!
    raise NotMovieClubMemberError unless member
    "🍿"
  end
end
