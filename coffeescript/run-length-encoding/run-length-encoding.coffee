encode = (s) -> s.replace /(.)\1+/g,   (run, c) -> "#{run.length}#{c}"
decode = (s) -> s.replace /(\d+)(.)/g, (_, n, c) -> c.repeat(n)

module.exports = {encode, decode}
