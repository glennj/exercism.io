class SqueakyClean {
    static String clean(String identifier) {
		StringBuilder b = new StringBuilder();
		boolean uppercaseNext = false;
		for (char c : identifier.toCharArray()) {
			if (c == ' ')
				b.append('_');
			else if (c == '-')
				uppercaseNext = true; // but don't append
			else if (Character.isISOControl(c))
				b.append("CTRL");
			else if (!Character.isLetter(c))
				continue;
			else if (Character.isLowerCase(c) && Character.UnicodeScript.of(c) == Character.UnicodeScript.GREEK)
				continue;
			else if (uppercaseNext) {
				b.append(Character.toUpperCase(c));
				uppercaseNext = false;
			}
			else
				b.append(c);
		}
		return b.toString();
    }
}
