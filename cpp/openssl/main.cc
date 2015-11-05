#include <openssl/aes.h>
#include <openssl/evp.h>
#include <iostream>
#include <string.h>

using namespace std;

int main(int argc, char* argv[])
{
    /* ckey and ivec are the two 128-bits keys necessary to
       en- and recrypt your data.  Note that ckey can be
       192 or 256 bits as well
     */

    unsigned char ckey[] = "1234567890123456";

    unsigned char ivec[] = "1234567890123456";

    // encrypted/decrypted operations is destructive for ivec
    unsigned char ivec_clone1[AES_BLOCK_SIZE];
    unsigned char ivec_clone2[AES_BLOCK_SIZE];

    memcpy(ivec_clone1 , ivec, AES_BLOCK_SIZE);
    memcpy(ivec_clone2 , ivec, AES_BLOCK_SIZE);


    // data structure that contains the key itself
    AES_KEY aes_key;

    // set the encryption key
    AES_set_encrypt_key(ckey, AES_BLOCK_SIZE*8, &aes_key);


    unsigned char indata[] = "Hello";
    int text_size = sizeof(indata)-1;

    unsigned char outdata[AES_BLOCK_SIZE];
    unsigned char decryptdata[AES_BLOCK_SIZE];

    int num = 0;
    AES_cfb128_encrypt(indata, outdata, AES_BLOCK_SIZE, &aes_key, ivec_clone1, &num, AES_ENCRYPT);

    AES_cfb128_encrypt(outdata, decryptdata, AES_BLOCK_SIZE, &aes_key, ivec_clone2, &num, AES_DECRYPT);

    cout << "key:                    " << ckey << endl;
    cout << "iv:                     " << ivec << endl;
    cout << "original data:          " << indata << endl;
    cout << "size:                   " << text_size << endl;
    cout << "encrypted data:         ";
    for (int i = 0; i < text_size; i++)
        cout << std::hex << (int)  outdata[i];

    cout << endl;
    cout << "decrypted data:         " << decryptdata << endl;
    return 0;
}