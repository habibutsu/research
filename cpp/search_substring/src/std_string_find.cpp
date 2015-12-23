#include <string>
#include <iostream>

using namespace std;

int main(int argc, char* argv[]){
    if(argc < 3){
        cout << "Usage: <haystack> <needle>" << endl;
        return 0;
    }
    size_t offset = string::npos;
    string haystack = argv[1];
    string needle = argv[2];
    
    offset = haystack.find(needle);

    cout << offset << endl;
    return 0;
}
