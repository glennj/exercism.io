module Port
  IDENTIFIER = :PALE
  
  def self.get_identifier(city)
    city[0,4].to_sym.upcase
  end

  def self.get_terminal(ship_identifier)
    %w[OIL GAS].include?(ship_identifier.to_s[0,3]) ? :A : :B
  end
end
