class ComplexNumber {
    private double real;
    private double imag;

    ComplexNumber(double real, double imag) {
        this.real = real;
        this.imag = imag;
    }

    double getReal() { return real; }
    double getImaginary() { return imag; }

    ComplexNumber add(ComplexNumber other) {
        return new ComplexNumber(
                real + other.getReal(),
                imag + other.getImaginary()
        );
    }

    ComplexNumber subtract(ComplexNumber other) {
        return new ComplexNumber(
                real - other.getReal(),
                imag - other.getImaginary()
        );
    }

    ComplexNumber multiply(ComplexNumber other) {
        return new ComplexNumber(
                real * other.getReal() - imag * other.getImaginary(),
                imag * other.getReal() + real * other.getImaginary()
        );
    }

    ComplexNumber divide(ComplexNumber other) {
        double denom = Math.pow(other.getReal(), 2) + Math.pow(other.getImaginary(), 2);
        return new ComplexNumber(
                (real * other.getReal() + imag * other.getImaginary()) / denom,
                (imag * other.getReal() - real * other.getImaginary()) / denom
        );
    }

    double abs() {
        return Math.sqrt(Math.pow(real, 2) + Math.pow(imag, 2));
    }

    ComplexNumber conjugate() {
        return new ComplexNumber(real, -1 * imag);
    }

    ComplexNumber exponentialOf() {
        ComplexNumber a = new ComplexNumber(Math.pow(Math.E, real), 0);
        ComplexNumber b = new ComplexNumber(Math.cos(imag), Math.sin(imag));
        return a.multiply(b);
    }
}
