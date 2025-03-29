/******************************************************************/         
/* NAME: Mihail Chitorog (originally by Walter) */
/* ASGT: Activity 1 */
/* ORGN: CSUB - CMPS 3500*/
/* FILE: fixed.cpp */
/* DATE: 02/03/2021 */
/* DATE: 09/13/2021 Applied coding style guidelines by M. Chitorog */
/*****************************************************************/

/*This program checks if a number is within the first 100 prime numbers*/ 

#include <iostream> 
#include <string> 
#include <bits/stdc++.h>

using namespace std;

/*Search one by one algorithm*/
int linearSearch(int arr[], int size, int search_num)
{
    for (int i = 0; i < size; i++)
        if (arr[i] == search_num) return i; return -1;
}

/*Divide and conquer algorithm*/
int binarySearch(int arr[], int beg, int end, int search_num) 
{  
    if(end >= beg) 
    { 
        int mid = (beg + end ) / 2;

        /* Check if element is at the middle */
        if(arr[mid] == search_num) 
        {
            return mid;
        }  

        /* If element is greater, ignore left half */
        else if(arr[mid] < search_num) 
        {
            return binarySearch(arr, mid + 1, end, search_num);
        }  
        else 
        {
            /* If element is smaller, ignore right half */
            return binarySearch(arr, beg, mid - 1, search_num);
        }                                                                   
    }  

    /* If element not present in array */
    return -1;
} 

/*Jump around algorithm*/
int jumpSearch(int arr[], int size, int search_num)
{
    int step = sqrt(size);
    int prev = 0;

    /* Jump through array in steps of sqrt(size) */   
    while (arr[min(step, size) - 1] < search_num) 
    {
        prev = step; 
        step += sqrt(size);
        
        if (prev >= search_num) 
            return -1;
    }
    /* Use linear search within the block */
    while (arr[prev] < search_num) 
    {
        prev++;
        
        if (prev == min(step, size)) 
            return -1;
    }
    
    /* If element is found */
    if (arr[prev] == search_num) 
        return prev; 

    /* If element not present in array */
    return -1;
}

int main () 
{  
    int arr[100] = { 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 
        53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 
        127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 
        193, 197, 199, 211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 
        269, 271, 277, 281, 283, 293, 307, 311, 313, 317, 331, 337, 347, 
        349, 353, 359, 367, 373, 379, 383, 389, 397, 401, 409, 419, 421, 
        431, 433, 439, 443, 449, 457, 461, 463, 467, 479, 487, 491, 499,
        503, 509, 521, 523, 541};
    
    int search_num; 
    int result_l = -1, result_b = -1, result_j = -1;
    
    /* Prompt user to input the number to search */
    cout<<"Enter the number that is to be searched: "; 
    cin >> search_num;

    /* Perform searches using 3 different algorithms */
    result_l = linearSearch(arr, 100, search_num); 
    result_b = binarySearch(arr, 0, 100, search_num); 
    result_j = jumpSearch(arr, 100, search_num);
    
    /* Display results */
    if((result_l + result_b + result_j) != -3)  
    {
        cout << "Key found at location using <your answer>: "
            << result_l + 1 << "\n"; 
        cout << "Key found at location using <your answer>: "
            << result_b + 1 << "\n"; 
        cout << "Key found at location using <your answer>: "
            << result_j + 1 << "\n\n"; 
    }  
    
    else 
    {
        /* If searched number not found in the array */
        cout << "Requested key not found\n\n";
    }

    return 0;
}
