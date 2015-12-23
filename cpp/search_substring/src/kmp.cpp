/*
 * Implementation of the Knuth-Morris-Pratt algorithm
 *
 * "Introduction to Algorithms" Thomas H. Corman and other
 *
 * */
#include <string>
#include <vector>
#include <iostream>

using namespace std;

void compute_prefix(vector<size_t> &prefix, const string &needle){
    const size_t m = needle.length();
    size_t k = string::pos;
    prefix.push_back(k);
    for(size_t q=1; q < m; q++){
        while(k < string::npos && needle[k + 1] != needle[q]){
            k = prefix[k]; 
        }
        if(needle[k + 1] == needle[q]){
            k++;
        }
        prefix.push_back(k);
    }
}

size_t kmp_matcher(const string &haystack, const string &needle){
    if(haystack.empty() || needle.empty()){
        return string::npos;    
    }
    const size_t n = haystack.length();
    const size_t m = needle.length();
    vector<size_t> prefix;
    prefix.reserve(m);

    compute_prefix(prefix, needle);
    
    size_t q = string::npos;
    for(size_t i=0; i < n; i++){
        while(q < string::npos && needle[q + 1] != haystack[i]){
            q = prefix[q];
        }
        if(needle[q + 1] == haystack[i]){
            q++;
        }
        if(q == m - 1){
            return i - m + 1;
        }
    }
    return string::npos;
}

int main(int argc, char* argv[]){
    if(argc < 3){
        cout << "Usage: <haystack> <needle>" << endl;
        return 0;
    }
    size_t offset = string::npos;
    string haystack = argv[1];
    string needle = argv[2];
    
    offset = kmp_matcher(haystack, needle);

    cout << offset << endl;
    return 0;
}
