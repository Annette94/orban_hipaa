-record(user_rec, {
    user_name :: binary(),
    role :: binary(),
    login_attempts :: non_neg_integer(),
    last_attempt_time :: non_neg_integer(),
    login_success :: boolean()
}).