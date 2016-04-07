Alg = aes_cfb128,
Key = "1234567890123456",
Iv = list_to_binary(Key),
EncData = crypto:block_encrypt(Alg, Key, Iv, <<"Hello123">>),
io:format("~p~n", [EncData]),
io:format("~p~n", [crypto:block_decrypt(Alg, Key, Iv, EncData)]).