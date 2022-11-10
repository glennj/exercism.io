def assert(cond; errmsg):
  if (cond | not) then errmsg | halt_error
  else .
  end
;
