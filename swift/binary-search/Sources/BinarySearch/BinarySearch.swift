enum BinarySearchError: Error {
    case unsorted
}

struct BinarySearch {
    let list: [Int]

    init(_ list: [Int]) throws {
        self.list = list
        // verify the list is sorted
        for i in 1..<list.count {
            if list[i-1] > list[i] {
                throw BinarySearchError.unsorted
            }
        }
    }

    var middle: Int { get { return list.count / 2 } }

    func searchFor(_ elem: Int, plus offset: Int = 0) -> Int? {
        let mid = self.middle
        if elem == list[mid] {
            return mid + offset
        }

        let startIdx: Int
        let endIdx: Int
        if elem < list[mid] {
            startIdx = 0
            endIdx = mid
        } else {
            startIdx = mid + 1
            endIdx = list.count
        }
        if startIdx >= endIdx { return nil }

        let sublist = Array(list[ startIdx ..< endIdx ])
        let bs = try! BinarySearch(sublist)
        return bs.searchFor(elem, plus: offset + startIdx)
    }
}