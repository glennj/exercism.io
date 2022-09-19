class Attendee
  def initialize(height)
    @height = height
    revoke_pass!
  end

  attr_reader :height, :pass_id

  def issue_pass!(pass_id)
    @pass_id = pass_id
  end

  def revoke_pass!
    issue_pass! nil
  end
end
