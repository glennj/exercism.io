// Implementing without using builtin higher-order methods.

class ListOps {
    static foldl(list, fn, initial) {
        for (def elem in list) {
            initial = fn(initial, elem)
        }
        return initial
    }

    static length(list) {
        foldl(list, { acc, elem -> acc + 1 }, 0)
    }

    static append(list1, list2) {
        foldl(list2, { acc, elem -> acc + elem }, list1)
    }

    static concatenate(lists) {
        foldl(lists, { acc, elem -> append(acc, [elem]) }, [])
    }

    static filter(list, fn) { 
        foldl(list, { acc, elem -> if (fn(elem)) acc = append(acc, [elem]); acc }, [])
    }

    static map(list, fn) {
        foldl(list, {acc, elem -> append(acc, fn(elem)) }, []) 
    }

    static reverse(list) {
        foldl(list, { acc, elem -> concatenate([[elem], acc]) }, [])
    }

    static foldr(list, fn, initial) {
        foldl(reverse(list), { acc, elem -> fn(acc, elem) }, initial)
    }
}
