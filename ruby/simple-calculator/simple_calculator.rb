class SimpleCalculator
  ALLOWED_OPERATIONS = ['+', '/', '*'].freeze
  
  class UnsupportedOperation < StandardError
  end

  def self.calculate(first_operand, second_operand, operation)
    assertNumeric first_operand
    assertNumeric second_operand
    assertOperation operation
    
    begin
      result = first_operand.send(operation.to_sym, second_operand)
      "#{first_operand} #{operation} #{second_operand} = #{result}"
    rescue ZeroDivisionError
      "Division by zero is not allowed."
    end
  end

  private
 
  def self.assertNumeric(operand)
    raise ArgumentError, "operand #{operand} is not Numeric" unless operand.kind_of? Numeric
  end

  def self.assertOperation(operation)
    raise UnsupportedOperation unless ALLOWED_OPERATIONS.include? operation
  end
end
