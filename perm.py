def generate_combinations(digits, combination_length):
    def backtrack(start, path):
        # If the current combination is of the desired length, add it to the results
        if len(path) == combination_length:
            results.append(path[:])
            return
        
        # Iterate through the digits to build the combinations
        for i in range(start, len(digits)):
            # Skip used digits
            if digits[i] in path:
                continue
            
            # Include the digit in the current path
            path.append(digits[i])
            
            # Recurse with the next digit
            backtrack(0, path)
            
            # Backtrack: remove the last digit from the path
            path.pop()

    results = []
    backtrack(0, [])
    return results

# Define the digits and the length of the combination
digits = [1, 2, 3, 4, 5, 6, 7, 8]
combination_length = 4

# Generate the combinations
combinations = generate_combinations(digits, combination_length)

# Convert each combination to a string and print it
for comb in combinations:
    print(''.join(map(str, comb)))

