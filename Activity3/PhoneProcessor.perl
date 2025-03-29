#!/usr/bin/perl
#C********************************************************************
#C NAME: Mihail Chitorog
#C ASGT: Activity 3
#C ORGN: CSUB - CMPS 3500
#C FILE: PhoneProcessor.perl
#C DATE: 10/10/2024
#C********************************************************************

use strict;
use warnings;

# Declare input and output file names
my $inputFile = "PhoneBook.txt";
my $outputFile = "CleanPhoneBook.txt";

# Open input and output files
open(my $INFILE, '<', $inputFile) or die "Could not open file '$inputFile' $!";
open(my $OUTFILE, '>', $outputFile) or die "Could not open file '$outputFile' $!";

# Initialize counters for dropped, total, and usable phone numbers
my $dropped_Count = 0;
my $total_Count = 0;
my $usable_Count = 0;
my $ten_Digit_Count = 0;
my $phone_Length = 0;

# Print header for the output file
print $OUTFILE "Last                           First                   Clean             String   Usable         Formatted\n";
print $OUTFILE "Name                           Name                    Phone Number      Length   Phone Number   Phone Number\n";
print $OUTFILE "****************************   **********************  ****************  *******  *************  *******************\n";

# Skip the first header line in the input file
my $headerLine = <$INFILE>;

# Process each line in the input file
while (my $currentLine = <$INFILE>) {
    # Remove any carriage return (CR) or line feed (LF) characters
    chomp $currentLine;

    # Increment total count
    $total_Count++;

    # Remove unwanted control characters
    $currentLine =~ tr/\15/\00/;

    # Declare variables to store name and phone information
    my ($namePortion, $nameField, $remainingFields, $uselessField, $city, $state, $rawPhoneNumber);
    my ($lastName, $firstName, $extraWords, $cleanedPhoneNumber, $formattedPhoneNumber, $leadingDigit);

    # Extract name and phone number from lines with or without quotes
    if ($currentLine =~ /[\"]/) {
        ($namePortion, $nameField, $remainingFields) = split '"', $currentLine;
        ($uselessField, $city, $state, $rawPhoneNumber) = split ',', $remainingFields;
    } else {
        ($uselessField, $nameField, $city, $state, $rawPhoneNumber) = split ',', $currentLine;
    }

    # Clean up name by removing commas and non-alphabetic characters
    $nameField =~ s/,//g;
    $nameField =~ s/[^a-zA-Z.\s\-]//g;

    # Extract first two words for last name and first name
    # Initialize to empty string if either is missing
    ($lastName, $firstName) = split " ", $nameField, 2;
    $lastName  = defined($lastName) ? $lastName : '';
    $firstName = defined($firstName) ? $firstName : '';

    # Remove non-numeric characters from the phone number
    $rawPhoneNumber =~ s/[^0-9]//g;

    # Store the clean phone number and calculate its length
    $cleanedPhoneNumber = $rawPhoneNumber;
    $phone_Length = length($rawPhoneNumber);

    # Apply the transformation rules based on the length of the phone number
    if ($phone_Length == 10) {
        $usable_Count++;
        $ten_Digit_Count++;
    } elsif ($phone_Length == 7) {
        $rawPhoneNumber = sprintf '%s%s', "661", $rawPhoneNumber;
        $usable_Count++;
    } elsif ($phone_Length == 11) {
        $rawPhoneNumber = substr $rawPhoneNumber, 1;
        $usable_Count++;
    } elsif ($phone_Length == 12) {
        $rawPhoneNumber = substr $rawPhoneNumber, 2;
        $usable_Count++;
    } elsif ($phone_Length == 9) {
        $leadingDigit = substr($rawPhoneNumber, 0, 1);
        if ($leadingDigit == "0") {
            $rawPhoneNumber = substr $rawPhoneNumber, 1;
            $rawPhoneNumber = sprintf '%s%s', "92", $rawPhoneNumber;
            $usable_Count++;
        } else {
            $rawPhoneNumber = "Dropped";
            $dropped_Count++;
        }
    } else {
        $rawPhoneNumber = "Dropped";
        $dropped_Count++;
    }

    # If the phone number is usable, format it for output
    if ($rawPhoneNumber eq "Dropped") {
        $formattedPhoneNumber = "";
    } else {
        my $areaCode = substr($rawPhoneNumber, 0, 3);
        my $centralOfficeCode = substr($rawPhoneNumber, 3, 3);
        my $lineNumber = substr($rawPhoneNumber, 6);

        $formattedPhoneNumber = sprintf "(%s) %s - %s", $areaCode, $centralOfficeCode, $lineNumber;
    }

    # Write the processed line to the output file
    my $newLine = sprintf "%-30s %-23s %-17s %-8d %-14s %s", $lastName, $firstName, $cleanedPhoneNumber, $phone_Length, $rawPhoneNumber, $formattedPhoneNumber;
    print $OUTFILE "$newLine \n";
}

# Print summary information to the screen
print "\n";
print "Number of usable numbers:  $usable_Count \n\n";
print "Number of dropped numbers: $dropped_Count \n\n";
print "Number of total numbers:   $total_Count \n\n";
#print "Number of 10-digit phone numbers that were identified correctly:   $ten_Digit_Count \n\n";
print "... Output file has been created\n\n";
print "Goodbye!\n";

# Close the input and output files
close($INFILE);
close($OUTFILE);
