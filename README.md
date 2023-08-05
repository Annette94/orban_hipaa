**Design Logic:**

This backend implementation follows the gen_server behavior and implements functionalities like user login, user management, and PHI data export. It uses Erlang's ETS (Erlang Term Storage) for user data in memory and AWS DynamoDB for persistent storage of user details and encrypted PHI data.

In order to comply with HIPAA standards, the following design features are implemented:

- **Password Handling:**
  - During user registration or password update, the actual password is not stored in the database. Instead, it is securely hashed using a cryptographic hash function. This hashing process converts the password into a fixed-size, irreversible hash value.
  - During login attempts, the provided password is hashed using the same algorithm, and the resulting hash is compared with the stored hash in the database. If the hashes match, the login attempt is successful, indicating that the correct password was provided.
  - Storing hashed passwords instead of plain text helps protect user credentials in case of a data breach or unauthorized access to the database.

- **PHI Data Encryption:**
  - PHI (Protected Health Information) data is highly sensitive and subject to strict security requirements. To protect PHI data, the backend uses encryption.
  - Before storing PHI data in the database, it is Base64 encoded to handle binary data as text and then encrypted using a strong encryption algorithm like AES (Advanced Encryption Standard).
  - The encrypted data, along with a tag, is stored in the database under the "phidata" and "tag" fields, respectively.
  - During data retrieval, the encrypted data and tag are retrieved from the database, and the data is decrypted.
  - The decrypted data is then Base64 decoded to obtain the original PHI data.
  - Encrypting PHI data ensures that even if unauthorized access to the database occurs, the data remains protected and unreadable without the correct decryption key.

- **Rate Limitation Mechanism:**
  - A rate limitation mechanism is implemented to protect user accounts from brute force attacks. Brute force attacks involve automated trial-and-error attempts to guess login credentials by trying multiple combinations rapidly.
  - For each user, the backend tracks login attempts using the ETS table "USER_DETAILS." This table stores user-specific login information, including the number of login attempts and the timestamp of the last attempt.
  - Existing users are subject to rate limiting. If the user exceeds the maximum number of allowed login attempts within a specific time window, further login attempts are blocked for a specific duration.
  - After each login attempt, the system checks if the time elapsed since the last login attempt has exceeded the time window defined by `?RATE_LIMIT_TIME`. If the time window has passed, the login attempt count is reset, allowing the user to make new attempts.

**Requirements Satisfied:**
- **User Login:** Handles user login attempts, password verification, and rate limiting to prevent brute force attacks.
- **User Management:** Allows adding new users to the system along with their roles and encrypted PHI data.
- **PHI Data Export:** Provides functionality to export PHI data.
- **Access Control:** Only users with the administrator role can access PHI data.

**Limitations:**
- **Backend-Only:** The frontend must be designed to utilize the provided functionalities.
- **Symmetric Key:** The AES encryption key is currently hard-coded, but in a real system, it should be securely managed using HSM (Hardware Security Module) or AWS KMS (Key Management Service).
- **Access Rights:** The granularity of access rights within administrators (e.g., read, write, export) is not yet provided.
- **Database Configuration:** The code has hardcoded AWS DynamoDB settings, limiting flexibility for changing database providers or connection settings.

**Additional Notes:**
- Rebar3 tool is used to build the application.
- The code can be cloned to your local system, and 'rebar3 compile' can be used to get the dependencies, while the 'rebar3 shell' command can start the application in a shell.
- Local DynamoDB setup was used to check the DB calls.
- (Reference Link : https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/DynamoDBLocal.html ; https://www.geeksforgeeks.org/dynamodb-local-installation/).

Build
-----

    $ rebar3 compile

Test
-----
    You can run these test cases by calling eunit:test/1 in your Erlang shell:
    eunit:test(phi_backend_handler_tests).
