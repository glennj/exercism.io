public class SalaryCalculator {
    private static final int DAYS_SKIPPED_THRESHOLD_DAYS = 5;
    private static final double DAYS_SKIPPED_MULTIPLIER_PENALTY = 0.15;
    private static final int PRODUCT_SOLD_MULTIPLIER = 10;
    private static final int PRODUCT_SOLD_BONUS_THRESHOLD = 20;
    private static final int PRODUCT_SOLD_BONUS_MULTIPLIER = 13;
    private static final double BASE_SALARY = 1000.0;
    private static final double MAX_SALARY = 2000.0;

    public double multiplierPerDaysSkipped(int daysSkipped) {
        return 1.0 - (daysSkipped > DAYS_SKIPPED_THRESHOLD_DAYS
                            ? DAYS_SKIPPED_MULTIPLIER_PENALTY
                            : 0);
    }

    public int multiplierPerProductsSold(int productsSold) {
        return productsSold > PRODUCT_SOLD_BONUS_THRESHOLD
                ? PRODUCT_SOLD_BONUS_MULTIPLIER
                : PRODUCT_SOLD_MULTIPLIER;
    }

    public double bonusForProductSold(int productsSold) {
        return productsSold * multiplierPerProductsSold(productsSold);
    }

    public double finalSalary(int daysSkipped, int productsSold) {
        double earned = BASE_SALARY * multiplierPerDaysSkipped(daysSkipped)
                        + bonusForProductSold(productsSold);
        return Math.min(MAX_SALARY, earned);
    } 
}
