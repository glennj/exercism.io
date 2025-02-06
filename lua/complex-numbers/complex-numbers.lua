function math.hypot(x, y)  return math.sqrt(x^2 + y^2) end
function math.eq_ish(x, y) return math.abs(x - y) < 1e-6 end

local Complex, ComplexFunctions

Complex = function(r, i)
  local obj = {
    r = r,
    i = i or 0,
  }
  setmetatable(obj, ComplexFunctions)

  obj.abs  = function () return math.hypot(obj.r, obj.i) end
  obj.conj = function () return Complex(obj.r, -obj.i) end
  obj.exp  = function () 
    local ar = math.exp(obj.r)
    local br = math.cos(obj.i)
    local bi = math.sin(obj.i)
    return Complex(ar) * Complex(br, bi)
  end

  return obj
end

ComplexFunctions = {
  __eq = function (a, b)
    return math.eq_ish(a.r, b.r) and math.eq_ish(a.i, b.i)
  end,
  __add = function (a, b)
    local r = a.r + b.r
    local i = a.i + b.i
    return Complex(r, i)
  end,
  __sub = function (a, b)
    local r = a.r - b.r
    local i = a.i - b.i
    return Complex(r, i)
  end,
  __mul = function (a, b)
    local r = a.r * b.r - a.i * b.i
    local i = a.i * b.r + a.r * b.i
    return Complex(r, i)
  end,
  __div = function (a, b)
    local divisor = (b.r ^ 2 + b.i ^2)
    local r = (a.r * b.r + a.i * b.i) / divisor
    local i = (a.i * b.r - a.r * b.i) / divisor
    return Complex(r, i)
  end
}

return Complex
