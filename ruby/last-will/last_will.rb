# Enter your code below the lines of the families' information

# Secret knowledge of the Zhang family:
class Zhang
  def self.bank_number_part(secret_modifier)
    zhang_part = 8_541
    zhang_part * secret_modifier % 10_000
  end

  class Red
    def self.code_fragment
      512
    end
  end

  class Blue
    def self.code_fragment
      677
    end
  end
end

# Secret knowledge of the Khan family:
class Khan
  def self.bank_number_part(secret_modifier)
    khan_part = 4_142
    khan_part * secret_modifier % 10_000
  end

  class Red
    def self.code_fragment
      148
    end
  end

  class Blue
    def self.code_fragment
      875
    end
  end
end

# Secret knowledge of the Garcia family:
class Garcia
  def self.bank_number_part(secret_modifier)
    garcia_part = 4_023
    garcia_part * secret_modifier % 10_000
  end

  class Red
    def self.code_fragment
      118
    end
  end

  class Blue
    def self.code_fragment
      923
    end
  end
end

# Enter your code below
class EstateExecutor
  @@namespaces = [::Garcia, ::Khan, ::Zhang]
  
  def self.assemble_account_number(secret)
    @@namespaces.map {|ns| ns::bank_number_part(secret)}.sum
  end

  def self.assemble_code
    red = @@namespaces.map { |ns| ns::Red::code_fragment }.sum
    blue = @@namespaces.map { |ns| ns::Blue::code_fragment }.sum
    red * blue
  end
end
