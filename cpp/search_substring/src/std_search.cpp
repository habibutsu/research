#include <string>
#include <iostream>
#include <algorithm>

using namespace std;

int main(int argc, char* argv[]){
    if(argc < 3){
        cout << "Usage: <haystack> <needle>" << endl;
        return 0;
    }
    size_t offset = string::npos;
    string::iterator it;
    string haystack = argv[1];
    string needle = argv[2];
    
    it = search(haystack.begin(), haystack.end(),
                needle.begin(), needle.end());

    if(it != haystack.end()){
        offset = it - haystack.begin();
    }

    cout << offset << endl;
    return 0;
}
