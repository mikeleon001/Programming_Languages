# course: cmps3500
# Activity 7
# date: 12/09/24 
# username: mchitorog
# name: Mihail Chitorog
# 
# file_name: stack.py
# 
# description: A stack simulator with error handling and size management.
# The program implements basic stack operations with bounds checking
# and proper error handling for invalid inputs.
#
# Stack is implemented as a class to encapsulate all operations and data,
# making it easier to maintain and modify stack behavior without affecting
# the rest of the code

class Stack:
    def __init__(self, max_size=10):
        # Using a list instead of creating our own array implementation
        # because Python lists already handle memory management efficiently
        self.items = []
        # Max size is configurable but defaults to 10 to prevent unbounded growth
        self.max_size = max_size
    
    def push(self, item):
        # We check fullness before pushing to prevent silent failures
        # and give users immediate feedback
        if self.isFull():
            print("The stack is full, please pop an element to continue")
            return False
        self.items.append(item)
        # Return status helps calling code know if push succeeded
        return True
    
    def pop(self):
        # Empty check prevents the IndexError that would crash the program
        # and gives users a more helpful message
        if self.isEmpty():
            print("Cannot pop from an empty stack, please push some elements")
            return None
        return self.items.pop()
    
    # These three methods are separated for clarity even though they're simple
    # This makes the code more maintainable if we need to change size handling later
    def isEmpty(self):
        return len(self.items) == 0
    
    def isFull(self):
        return len(self.items) >= self.max_size
    
    def size(self):
        return len(self.items)
    
    def print(self):
        # Direct print of the list works because Python's representation
        # of lists matches our desired output format
        print(self.items)

def print_menu():
    # Menu is separated into its own function to make it easier to modify
    # and to keep the main loop clean and focused on logic
    print("***********************************")
    print("          Stack Simulator          ")
    print("***********************************")
    print("Please only use digits from 0 to 9 ")
    print("***********************************")
    print("Please enter 'pop' for popping")
    print("Please enter 'push' for pushing")
    print("Please enter 'print' to print")
    print("Please enter 'IsEmpty' to check if the stack is empty")
    print("Please enter 'IsFull' to check if the stack is full")
    print("Please enter 'size' to print the current size of the stack")
    print("Please enter 'end' to terminate the program")

def validate_number(num_str):
    # Validation is separated to make it easy to modify rules
    # and to keep the main loop clean
    if not num_str.isdigit() or len(num_str) > 1:
        print("please enter only a 1 digit positive numbers")
        return False
    return True

def main():
    # Stack is created once at startup to maintain state throughout the session
    stack = Stack()
    print_menu()
    
    while True:
        val = input("...")
        # Convert to lowercase to make commands case-insensitive
        # This provides a better user experience
        command = val.lower()
        
        if command == 'push':
            num = input("Which number to push?... ")
            # Validate before pushing to prevent invalid stack states
            if validate_number(num):
                stack.push(num)
                
        elif command == 'pop':
            result = stack.pop()
            # Only print result if pop was successful
            if result is not None:
                print(result)
                
        elif command == 'print':
            stack.print()
            
        elif command == 'isempty':
            # Using ternary operator for concise status messages
            print("Stack is empty" if stack.isEmpty() else "Stack is not empty")
            
        elif command == 'isfull':
            print("Stack is full" if stack.isFull() else "Stack is not full")
            
        elif command == 'size':
            print(f"The current size of the stack is {stack.size()}")
            
        elif command == 'end':
            print("Thank you")
            break
            
        else:
            print("Unknown command")

if __name__ == "__main__":
    main()
