import 'dart:math';

extension on double {
  double roundTo(int decimalPlaces) {
    var x = pow(10, decimalPlaces);
    return (this * x).round() / x;
  }
}

class SpaceAge {
  final secondsPerEarthYear = 31557600;
  final earthYearsPerOrbit = {
    'Mercury': 0.2408467,
    'Venus': 0.61519726,
    'Earth': 1.0,
    'Mars': 1.8808158,
    'Jupiter': 11.862615,
    'Saturn': 29.447498,
    'Uranus': 84.016846,
    'Neptune': 164.79132
  };

  double age({required String planet, required int seconds}) {
    return (seconds / secondsPerEarthYear / (earthYearsPerOrbit[planet] ?? 1)).roundTo(2);
  }
}
