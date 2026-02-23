map = {
  a: 'z', b: 'y', c: 'x', d: 'w', e: 'v', f: 'u', g: 't',
  h: 's', i: 'r', j: 'q', k: 'p', l: 'o', m: 'n',
  n: 'm', o: 'l', p: 'k', q: 'j', r: 'i', s: 'h',
  t: 'g', u: 'f', v: 'e', w: 'd', x: 'c', y: 'b', z: 'a',
  ['0']: '0', ['1']: '1', ['2']: '2', ['3']: '3', ['4']: '4',
  ['5']: '5', ['6']: '6', ['7']: '7', ['8']: '8', ['9']: '9',
}

decode = (phrase) -> phrase\lower!\gsub(".", (char) -> map[char] or '')

encode = (phrase) -> (decode phrase)\gsub(".....", "%0 ")\gsub(" $", "")


{ :encode, :decode }
