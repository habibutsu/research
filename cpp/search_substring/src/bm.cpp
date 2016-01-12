/*
 * Boyer - Moore string search algorithm
 *
 * http://www.cs.utexas.edu/~moore/publications/fstrpos.pdf
 * https://en.wikipedia.org/wiki/Boyer–Moore_string_search_algorithm
 * http://www.boost.org/doc/libs/1_60_0/libs/algorithm/doc/html/algorithm/Searching.html#the_boost_algorithm_library.Searching.BoyerMoore
 * */

#include <string>

# define ALPHABET_SIZE 256

size_t bm_search(const string &haystack, const string &needle){
    // TODO: need to implement
}

int main(int argc, char* argv[]){
    if(argc < 3){
        cout << "Usage: <haystack> <needle>" << endl;
        return 0;
    }
    size_t offset = string::npos;
    string haystack = argv[1];
    string needle = argv[2];
    
    offset = bm_search(haystack, needle);

    cout << offset << endl;
    return 0;
}
