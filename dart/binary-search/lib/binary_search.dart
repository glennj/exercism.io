import 'package:binary_search/value_not_found_exception.dart';

class BinarySearch {
  List<int> data;
  BinarySearch(this.data);

  int find(int target) {
    int i = 0;
    int j = data.length - 1;

    while (i <= j) {
      int mid = (i + j) ~/ 2;
      if (target < data[mid]) {
        j = mid - 1;
      } else if (target > data[mid]) {
        i = mid + 1;
      } else {
        return mid;
      }
    }

    throw ValueNotFoundException("Can't find $target in data");
  }
}
