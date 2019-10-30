local function gcd(a, b)
  return b == 0 and a or gcd(b, a % b)
end

local function obj_form(a)
  return { num = a[1], den = a[2] }
end

local function reduce(a)
  local r = obj_form(a)
  if r.num == 0 then
    return {0, 1}
  end
  if r.den < 0 then
    r.num = -1 * r.num
    r.den = -1 * r.den
  end
  local g = gcd(r.num, r.den)
  return {r.num / g, r.den / g}
end

local function add(a, b)
  local x = obj_form(a)
  local y = obj_form(b)
  return reduce({
    x.num * y.den + y.num * x.den,
    x.den * y.den
  })
end

local function negate(a)
  local x = obj_form(a)
  return { -1 * x.num, x.den }
end

local function subtract(a, b)
  return add(a, negate(b)) 
end

local function multiply(a, b)
  local x = obj_form(a)
  local y = obj_form(b)
  return reduce({ x.num * y.num, x.den * y.den })
end

local function invert(a)
  local x = obj_form(a)
  return { x.den, x.num }
end

local function divide(a, b)
  return multiply(a, invert(b))
end

local function abs(a)
  local x = obj_form(a)
  return {math.abs(x.num), math.abs(x.den) }
end

local function exp_rational(a, p)
  local x = obj_form(a)
  local b
  if p >= 0 then
    b = { x.num ^ p, x.den ^ p }
  else
    b = { x.den ^ (-1 * p), x.num ^ (-1 * p) }
  end
  return reduce(b)
end

local function exp_real(p, a)
  local x = obj_form(a)
  return p ^ (x.num / x.den)
end

return {
  add = add,
  subtract = subtract,
  multiply = multiply,
  divide = divide,
  abs = abs,
  exp_rational = exp_rational,
  exp_real = exp_real,
  reduce = reduce
}
