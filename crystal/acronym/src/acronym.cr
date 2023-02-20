module Acronym
  def self.abbreviate(phrase : String) : String
    phrase.scan(/(^ | [^'[:alpha:]]) \K [[:alpha:]]/x)
          .map(&.[0])
          .join
          .upcase
  end
end
