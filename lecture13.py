
# All the functions we have built till now can be translated to python. 

# let is_prime (n: int) : bool =
#   if n < 2 then false
#   else
#     let i = ref 2 in
#     let has_divisor = ref false in
#     while !i * !i <= n && not !has_divisor do
#       if n mod !i = 0 then has_divisor := true;
#       i := !i + 1
#     done;
#     not !has_divisor
# ;;

from math import sqrt

def is_prime(n: int) -> bool:
    if n < 2:
        return False
    else:
        for i in range(2, int(sqrt(n))+1): 
            if n % i == 0:
                return False
        return True

# Let us observe if Python can lead to some unexpected behaviour:



a = [1, 2, 3]
b = a
a += [4]
# print(b)

a = "1, 2, 3"
b = a
a += ", 4"
# print(b)

# Here, we see a difference. It is because the two objects are of different types.

from typing import List

a: List[int] = [1, 2, 3]
b: List[int] = a
a += [4]

# After a += [4], both a and b will be updated to [1, 2, 3, 4]
# print("a:", a)
# print("b:", b)

# Explanation: In this case, both 'a' and 'b' are referencing the same list in memory.
# So, when we modify 'a' (by using '+=' to append [4]), 'b' also reflects the change.

# Example 2: String Mutation
s: str = "1, 2, 3"
t: str = s  # t is a reference to s
s += ", 4"  # Modifying s by concatenating ", 4"

# After s += ", 4", s will become "1, 2, 3, 4", but t will remain "1, 2, 3"
# print("s:", s)  # Output: "1, 2, 3, 4"
# print("t:", t)  # Output: "1, 2, 3"

# Explanation: Strings in Python are immutable, so 's' and 't' do not refer to the same object.
# When 's' is modified, a new string is created, and 't' remains unchanged.

# Mutable and Immutable Types in Python:

x: int = 5
assert type(x) == int
assert isinstance(x, int), f"Expected int, got {type(x)}"

# Is int mutable or immutable? Let us find out!

# Digression: 

assert isinstance(True, int)
# assert (type(True) is int)
# print(True + True + True)



# Here is a function that prints the memory address of a given object.
def show_memory(obj: object, name: str) -> None:
    print(f"\n{name}:")
    print(f"  Value: {obj}")
    print(f"  Type: {type(obj)}")
    print(f"  Memory address (id): {id(obj)}")

x: int = 42

# show_memory(x, "Original Integer")

x += 2
# let y = a new int, set y to x + 2
# rename y to x, and forget the old x

# show_memory(x, "Updated Integer")

z: int = 42
# show_memory(z, "My New Z")

big_str: str = "A" * 100
# show_memory(big_str, "Large string")

big_str: str = "A" * 101
# show_memory(big_str, "Large string")

l : List[int] = [1, 2, 3]
# show_memory(l, "Original List")

l.append(2)
# show_memory(l, "Updated List")

# show_memory(l[1], "Updated List")
# show_memory(l[3], "Updated List")

# Type Mismatch

a: int = 5
b: str = "hello"
result = a * b

# print (result)
# Lists in Python

# A Python list is a dynamic array that can store multiple data types.
my_list = [10, "hello", [3.14, True]]

# print(my_list)
# What is its type? Technically, this can be defined using Union types. But it is easy to see that such lists would not be useful when going for map, filter, or fold. Let us look at some list operations.


# In Python, one can even do some really dangerous typing business:
self_ref_list = [1]
self_ref_list.append(self_ref_list)
# print(self_ref_list)  
# print (self_ref_list in self_ref_list)

# 1. Indexing, Slicing, and Membership
numbers : List[int] = [1, 2, 3, 4, 5]
assert numbers[3] == 4
assert numbers[1:3] == [2, 3]
assert (4 in numbers)

# 2. Appending and Extending
numbers.append(6)  # Adds a single element
numbers.extend([7, 8, 9])  # Adds multiple elements
numbers + [783434, 4343]

# 3. Inserting and Removing Elements
numbers.insert(2, 99)  # Insert 99 at index 2
numbers.remove(3)  # Removes first occurrence of 3

# Loops in Python

# For loops allow us to iterate over iterable objects such as lists, strings, sets, dictionaries, etc. 

word: str = "Introduction to Computer Science"
for char in word:
    pass # print(char)

from typing import Tuple

coordinates: List[Tuple[int, int]] = [(0, 0), (1, 2), (3, 4)]
for (x, y) in coordinates:
    pass # print(f"X: {x}, Y: {y}")

from typing import Dict

# A dictionary maps keys to values
menu: Dict[str, int] = {"Aalu": 30, "Bhature": 25, "Chhole": 40}

for item, price in menu.items():
    pass # print(f"{name} scored {score}")

# ranges are also very popular

n : int = 100
for i in range(2, int(sqrt(n)) + 1): 
    if n % i == 0: pass

# More Funny Business!

l = [1, 2, 3, 4, 5, 6]
for x in l:
    l.remove(x)
print(l)





# The above code tells you that you should not modify the list while you iterate over it. A better way is to create a copy of a list (deep copy, if it is a list of lists) and then iterate over the copy. But in general, it might just be better to not use for loops. While loops prevent some of these unexpected behaviours. 

# While Loops

# correct_pin = "1234"
# attempts = 0
# while attempts < 3:
#     pin = input("Enter your PIN: ")
#     if pin == correct_pin:
#         print("Access Granted!")
#         break
#     attempts += 1
# else:
#     print("Too many incorrect attempts. Card blocked.")


# correct_pin = "1234"
# attempts = 0

# while True:
#     pin = input("Enter your PIN: ")

#     if pin == correct_pin:
#         print("Access Granted!")
#         break  # Exit the loop on correct PIN
    
#     attempts += 1
#     print(f"Incorrect PIN. Attempts left: {3 - attempts}")

#     if attempts >= 3:
#         print("Too many incorrect attempts. Card blocked.")
#         break  # Exit after 3 failed attempts

# Now we come to the interesting part of this class. Sorting! In Assignment 7 we saw a selection sort algorithm. It can be written in Python as:

def selection_sort(l: List[int]) -> List[int]:
    n: int = len(l)
    for i in range(n):
        
        min_index: int = i
        for j in range(i + 1, n):
            if l[j] < l[min_index]:
                min_index = j
                
        l[i], l[min_index] = l[min_index], l[i]

    return l

l = [64, 34, 25, 12, 22, 11, 90]



# What does it mean to sort a list? It means that it passes the following test:

def is_sorted (l: List[int]) -> bool:
    for i in range(len(l)-1):
        if l[i] < l[i+1]:
            return False
    return True

# Then we want to assert the following:

def selection_sort(l: List[int]) -> List[int]:
    n : int = len(l)
    
    for i in range(n):
        # First i elements are sorted
        assert (is_sorted(l[0:i]))
        # That is:
        assert all(l[k] <= l[k + 1] for k in range(i)) 
        
        min_index = i
        for j in range(i + 1, n):
            assert all(l[k] >= l[min_index] for k in range(i, j))  # All elements in l[i...j-1] are >= l[min_index]
            if l[j] < l[min_index]:
                min_index = j
        
        assert l[min_index] <= l[i]  # min_index element is <= current element
        
        l[i], l[min_index] = l[min_index], l[i]
        
        assert all(l[k] <= l[k + 1] for k in range(i + 1))

    assert (is_sorted(l))
    # That is:
    assert all(l[k] <= l[k + 1] for k in range(len(l)))
    return l