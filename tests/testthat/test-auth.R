
context('Auth-local')

MOCK_CREDS_LOCAL <- c(
    PHILIPS_HUE_BRIDGE_IP = '0.0.0.0',
    PHILIPS_HUE_BRIDGE_USERNAME = 'AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA'
)

test_that('errors are thrown with invalid inputs', {
    withr::with_envvar(MOCK_CREDS_LOCAL, {
        expect_error(auth_local(ip = NULL), '`ip` must be a single character value')
        expect_error(auth_local(ip = NA), '`ip` must be a single character value')
        expect_error(auth_local(ip = pi), '`ip` must be a single character value')
        expect_error(auth_local(ip = letters), '`ip` must be a single character value')
        expect_error(auth_local(ip = iris), '`ip` must be a single character value')

        expect_error(auth_local(username = NULL), '`username` must be a single character value')
        expect_error(auth_local(username = NA), '`username` must be a single character value')
        expect_error(auth_local(username = pi), '`username` must be a single character value')
        expect_error(auth_local(username = letters), '`username` must be a single character value')
        expect_error(auth_local(username = iris), '`username` must be a single character value')

        expect_error(auth_local(ip = 'invalid IP'), '[Ii]nvalid value for `ip`')
        expect_error(auth_local(username = 'invalid username'), '[Ii]nvalid value for `username`')
    })
})

test_that('TRUE is returned upon success', {
    withr::with_envvar(MOCK_CREDS_LOCAL, {
        expect_true(auth_local())
    })
})
