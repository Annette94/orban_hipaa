**How this is designed:**

This backend implementation follows the gen_server behavior and implements functionalities like user login, user management, and PHI data export. It appears to use Erlang's ETS (Erlang Term Storage) for user data in memory and AWS DynamoDB for persistent storage of user details and encrypted PHI data.

Inorder to comply with HIPAA standards, 

    - When a user registers, the actual password is not stored plain in DB instead the password is hashed using a secure cryptographic hash function providing an extra layer of security in case of a data breach. During login attempts, the provided password is hashed again using the same algorithm, and the resulting hash is compared to the stored hash in the database. If the hashes match, the login attempt is successful, indicating that the correct password was provided.
    - PHI (Protected Health Information) data is highly sensitive and subject to strict security requirements. To protect PHI data, encryption is used. Encryption is a process of converting data into a form that can only be read with the correct decryption key. Here's how PHI data encryption is done in the code:
        • Before storing PHI data in the database, it is first Base64 encoded (to handle binary data as text) and then encrypted using a strong encryption algorithm (e.g., AES).
        • The encrypted data and the tag are stored in the database under the "phidata" and "tag" fields, respectively.
        • When retrieving PHI data, the encrypted data and tag are retrieved from the database. The "crypto_utils:decrypt_aes/3" function is used to decrypt the data using the same secret key and tag.
        • The decrypted data is then Base64 decoded to obtain the original PHI data.
    - Rate limitation mechanism to help protect user accounts from brute force attacks by limiting the number of login attempts that can be made within a specific time period.

**What all requirements are satisfied under this code:**
    **User Login:**__ The backend implementation handles user login attempts, password verification, and rate limiting to prevent brute force attacks.
    **User Management:**__ It allows adding new users to the system along with their role and encrypted PHI data.
    **PHI Data Export**__ The backend provides functionality to export PHI data.
    **Access Control**__ Only user with administrator role can access the PHI data.

**Limitations:**
    - This is only the backend implementation, the frontend must be designed to utilize the functionalities provided here.
    - The symmetric key used for aes encryption and decryption is currecntly hard-coded in the code but in realtime this key must either be stored in HSM or we can use AWS KMS service.
    - Even within administrators there can be various access rights (like read, write, export). The granularity in access rights is not provided.
    - Data Persistence: The code uses AWS DynamoDB for persistent data storage, but the database configuration and connection details are hardcoded, limiting flexibility for changing database providers or connection settings.


Build
-----

    $ rebar3 compile

Test
-----
    You can run these test cases by calling eunit:test/1 in your Erlang shell:
    eunit:test(phi_backend_handler_test).
    
