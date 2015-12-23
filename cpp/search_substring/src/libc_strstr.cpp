#include <iostream>
#include <string.h>

int main(int argc, char* argv[]){
    if(argc < 3){
        std::cout << "Usage: <haystack> <needle>" << std::endl;
        return 0;
    }
    int offset = -1;
    char* c;
    c = strstr(argv[1], argv[2]);

    if(c != NULL){
        offset = c - argv[1];
    }

    std::cout << offset << std::endl;
    return 0;
}
