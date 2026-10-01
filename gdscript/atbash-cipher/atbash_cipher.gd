const translate = {
	'a':'z', 'b':'y', 'c':'x', 'd':'w', 'e':'v', 'f':'u', 'g':'t',
	'h':'s', 'i':'r', 'j':'q', 'k':'p', 'l':'o', 'm':'n',
	'n':'m', 'o':'l', 'p':'k', 'q':'j', 'r':'i', 's':'h',
	't':'g', 'u':'f', 'v':'e', 'w':'d', 'x':'c', 'y':'b', 'z':'a',
	'0':'0', '1':'1', '2':'2', '3':'3', '4':'4', 
	'5':'5', '6':'6', '7':'7', '8':'8', '9':'9',
}


func encode(plain_text):
	return spaced(decode(plain_text))


func decode(ciphered_text):
	var chars = Array(ciphered_text.to_lower().split())
	var decoded = chars.map(func (c): return translate.get(c, ''))
	return "".join(decoded)


func spaced(str, size := 5):
	var chunks = []

	while not str.is_empty():
		chunks.append(str.substr(0, size))
		str = str.substr(size)

	return " ".join(chunks)
