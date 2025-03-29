########################################################################
# course: cmps3500
# Activity 7
# date: 12/08/24 
# username: mchitorog
# name: Mihail Chitorog
#
# file_name: GoodFileWords.py
#
# description: Command line utility for creating a file that contains 
# multiple copies of a single word. The program handles different error 
# cases gracefully by providing informative error messages.
########################################################################

import sys

# Constants are defined at module level for easy maintenance and consistency
# for all error messages
USAGE_MSG = "Usage: word_writer.py FILE_NAME WORD COUNT"
RETRY_MSG = "To try again, please relaunch the program with correct arguments."

def print_words(file_name, word, count):
    """
    Prints a given word to a newly created file some number of times.

    Arguments:
        file_name -- A string containing the file name.
        word      -- A string containing the word to write.
        count     -- An integer indicating how many times to write the word.
    """
    try:
        # 'x' mode is used instead of 'w' to prevent accidentally overwriting
        # existing files - it's safer for users
        file_obj = open(file_name, 'x')
        for _ in range(count):
            file_obj.write(word + "\n")
        file_obj.close()
    except FileExistsError:
        # Specific error message for file existence helps users understand
        # why their command failed and how to fix it
        print(f"The file {file_name} already exists in this folder.")
        print(USAGE_MSG)
        print(RETRY_MSG)
    except PermissionError:
        # Permission errors need their own message since they require
        # different user action to resolve
        print(f"Permission denied: Unable to create file {file_name}")
        print(USAGE_MSG)
        print(RETRY_MSG)

def main():
    """
    The main function obtains command line arguments and makes an appropriate
    call to the print_words function.
    """
    try:
        # Argument count is checked first because no other validation
        # makes sense if we don't have the right number of arguments
        if len(sys.argv) != 4:
            print("Incorrect number of arguments.")
            print(USAGE_MSG)
            print(RETRY_MSG)
            return

        file_name = sys.argv[1]
        word = sys.argv[2]

        try:
            # Count validation is separated to provide specific error
            # messages for different types of count-related errors
            count = int(sys.argv[3])
            if count < 0:
                # Negative numbers are technically integers but don't
                # make sense for this application
                print("Count must be a positive integer.")
                print(USAGE_MSG)
                print(RETRY_MSG)
                return
        except ValueError:
            # This catches non-integer inputs for count, helping users
            # understand exactly what went wrong
            print(f"'{sys.argv[3]}' cannot be converted to an integer.")
            print(USAGE_MSG)
            print(RETRY_MSG)
            return

        print_words(file_name, word, count)

    except Exception as e:
        # Catch-all for unexpected errors ensures the program always
        # exits gracefully with helpful information
        print(f"An unexpected error occurred: {str(e)}")
        print(USAGE_MSG)
        print(RETRY_MSG)

if __name__ == "__main__":
    main()
