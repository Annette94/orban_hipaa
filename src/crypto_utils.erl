-module(crypto_utils).
%% TODO :  Add spec

-export[hash_password/1,
        verify_password/2,
        encrypt_aes/2,
        decrypt_aes/3].

%% ================
%% Hashing
%% ================
hash_password(PlainTextPassword) ->
    crypto:hash(sha256, PlainTextPassword).

verify_password(PlainTextPassword, StoredHashedPassword) ->
    % Verify the password against the stored hash using bcrypt
    HashedPassword = crypto:hash(sha256, PlainTextPassword),
    StoredHashedPassword =:= HashedPassword.

%% ================
%% Encyption
%% ================
% AES encryption function
encrypt_aes(Key, PlainText) ->
    IV = <<0:128>>,
    crypto:crypto_one_time_aead(aes_256_gcm, Key, IV, PlainText, <<>>, 2, true).

% AES decryption function
decrypt_aes(Key, CipherText, Tag) ->
    IV = <<0:128>>,
    crypto:crypto_one_time_aead(aes_256_gcm, Key, IV, CipherText, <<>>, Tag, false).
