const REGEX = {
	question: /[a-z0-9]+\?/,
	yell: /[A-Z]{3,}/,
	questionYell: /[A-Z]{3,}\?/,
	nothing: /^[\s]/m, 
};

export const hey = (message) => {
	
	let bobSaid = '';

	switch (true) {

		case REGEX.question.test(message):
			bobSaid = 'Sure.';
			break;

		case REGEX.yell.test(message):
			bobSaid = 'Whoa, chill out!';
			break;

		case REGEX.questionYell.test(message):
			bobSaid = "Calm down, I know what I'm doing!";
			break;

		case REGEX.nothing.test(message):
			bobSaid = 'Fine. Be that way!';
			break;

		default:
			bobSaid = 'Whatever.';
	}

	return bobSaid;
};
