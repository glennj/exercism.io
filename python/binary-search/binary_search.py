def find(list_of_numbers, number):
    i = 0
    j = len(list_of_numbers) - 1
    while i <= j:
        mid = (i + j) // 2
        if number == list_of_numbers[mid]:
            return mid
        elif number < list_of_numbers[mid]:
            j = mid - 1
        else:
            i = mid + 1
    raise ValueError('value not in array')
