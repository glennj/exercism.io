module Bob
  def self.hey(string : String) : String
    silent = string.strip.empty?
    asking = string.rstrip.ends_with? "?"
    yelling = string =~ /[[:upper:]]/ && string !~ /[[:lower:]]/

    case
      when asking && yelling then "Calm down, I know what I'm doing!"
      when asking            then "Sure."
      when yelling           then "Whoa, chill out!"
      when silent            then "Fine. Be that way!"
      else                        "Whatever."
    end
  end
end
