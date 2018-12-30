class ArmstrongNumbers {
	boolean isArmstrongNumber(int numberToCheck) {
		// if (numberToCheck == 0)
		// 	return true;
		//
		// int n = numberToCheck;
		// int len = (int)Math.ceil(Math.log10(numberToCheck));
		// int sum = 0;
		// while (n > 0) {
		// 	sum += Math.pow(n % 10, len);
		// 	n /= 10;
		// }
		// return sum == numberToCheck;

		String digits = String.valueOf(numberToCheck);
		int len = digits.length();
		int sum = digits
			.chars()
			.parallel()
			.map(c -> Character.getNumericValue(c))
			.map(d -> (int) Math.pow(d, len))
			.sum();
		return sum == numberToCheck;
	}
}
