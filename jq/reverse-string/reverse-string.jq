.value
| (. / "") as $chars             # `/` on a string is like `split/1`
| [ $chars[range(length - 1; -1; -1)] ]
                                # the array index expression is a stream
                                # which results in a stream of array elements
                                # that are captured in the array constructor
| add + ""
