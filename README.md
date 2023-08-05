**Design Logic:**

This backend implementation follows the gen_server behavior and implements functionalities like user login, user management, and PHI data export. It uses Erlang's ETS (Erlang Term Storage) for user data in memory and AWS DynamoDB for persistent storage of user details and encrypted PHI data.

Inorder to comply with HIPAA standards, 

    - The actual password is not stored in DB when a user registers instead the password is hashed using a secure cryptographic hash function providing an extra layer of security in case of a data breach. During login attempts, the provided password is hashed again using the same algorithm, and the resulting hash is compared to the stored hash in the database. If the hashes match, the login attempt is successful, indicating that the correct password was provided.

    - PHI (Protected Health Information) data is highly sensitive and subject to strict security requirements. To protect PHI data, encryption is used. Encryption is a process of converting data into a form that can only be read with the correct decryption key. Here's how PHI data encryption is done in the code:
        • Before storing PHI data in the database, it is first Base64 encoded (to handle binary data as text) and then encrypted using a strong encryption algorithm (e.g., AES).
        • The encrypted data and the tag are stored in the database under the "phidata" and "tag" fields, respectively.
        • When retrieving PHI data, the encrypted data and tag are retrieved from the database. The "crypto_utils:decrypt_aes/3" function is used to decrypt the data using the same secret key and tag.
        • The decrypted data is then Base64 decoded to obtain the original PHI data.

    - Rate limitation mechanism to help protect user accounts from brute force attacks by limiting the number of login attempts that can be made within a specific time period. 

**What all requirements are satisfied under this code:**
    *User Login:*
        The backend implementation handles user login attempts, password verification, and rate limiting to prevent brute force attacks.
    *User Management:*
        It allows adding new users to the system along with their role and encrypted PHI data.
    *PHI Data Export*
        The backend provides functionality to export PHI data.
    *Access Control*
        Only user with administrator role can access the PHI data.

**Limitations:**
    *Backend-Only:* 
        The frontend must be designed to utilize the provided functionalities.
    *Symmetric Key:*
        The AES encryption key is currently hard-coded, but in a real system, it should be securely managed using HSM or AWS KMS.
    *Access Rights:*
        The granularity of access rights within administrators (like read, write, export) is not yet provided.
    *Database Configuration:*
        The code has hardcoded AWS DynamoDB settings, which limit flexibility for changing database providers or connection settings.

**Additional notes:**
    - Rebar3 tool is used to build the application.
    - The code can be cloned to your local system and 'rebar3 compile' can be used to get the dependencies and 'rebar3 shell' command to start the application in a shell.
    - Local dynamodb setup was used to check the DB calls.
        (Reference Link : https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/DynamoDBLocal.html ; https://www.geeksforgeeks.org/dynamodb-local-installation/).

Build
-----

    $ rebar3 compile

Test
-----
    You can run these test cases by calling eunit:test/1 in your Erlang shell:
    eunit:test(phi_backend_handler_tests).
