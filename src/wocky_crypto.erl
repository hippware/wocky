-module(wocky_crypto).

-define(CIPHER, aes_cbc).
-define(IV_LENGTH, 16).

-export([encrypt/2,
         encrypt/3,
         decrypt/2,
         decrypt/3
        ]).

encrypt(Key, Data) -> encrypt(Key, Data, 0).
encrypt(Key, Data, PadTo) ->
    PaddedData = pad_to(Data, PadTo),
    IV = crypto:strong_rand_bytes(?IV_LENGTH),
    CipherText = crypto:block_encrypt(?CIPHER, Key, IV, PaddedData),
    <<IV/binary, CipherText/binary>>.

decrypt(Key, Data) -> decrypt(Key, Data, false).
decrypt(Key, <<IV:?IV_LENGTH/binary, CipherText/binary>>, UnPad) ->
    Text = crypto:block_decrypt(?CIPHER, Key, IV, CipherText),
    case UnPad of
        false -> Text;
        true -> unpad(Text)
    end.

pad_to(Bin, Size) when Size =< byte_size(Bin) -> Bin;
pad_to(Bin, Size) ->
    PadLen = Size - byte_size(Bin),
    <<Bin/binary, 0:(PadLen*8)>>.

unpad(Bin) ->
    hd(binary:split(Bin, <<0>>)).

