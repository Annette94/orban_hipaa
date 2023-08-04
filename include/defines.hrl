%% AWS config
-define(ACCESS_KEY_ID, "******").
-define(SECRET_ACCCESS_KEY, "******").

-define(Key, <<192,210,98,11,206,91,247,73,68,23,101,74,13,98,21,204,174,34,66,127,55,171,104,59,107,104,9,18,58,185,65,17>>).

-define(PHI_TABLE_NAME, <<"phi_data">>).
-define(ETS_USER_DETAILS, user_details).

-define(RATE_LIMIT_TIME, 30). % Time in seconds for the rate limit window
-define(MAX_LOGIN_ATTEMPTS, 2). % Maximum allowed login attempts within the rate limit window