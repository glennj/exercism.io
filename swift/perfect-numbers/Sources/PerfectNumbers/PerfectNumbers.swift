enum NumberClassifier: Int {
    case deficient, perfect, abundant

    init(number n: Int) {
        var sum = 1
        let limit = Int( Double(n).squareRoot() )
        for i in 2...limit {
            if n % i == 0 {
                sum += i
                if i * i != n { sum += n / i }
            }
        }

        switch (sum - n).signum() {
            case -1: self = .deficient
            case  1: self = .abundant
            default: self = .perfect
        }
    }

    var classification: NumberClassifier {
        return self
    }
}
