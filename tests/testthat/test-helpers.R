context('set/reset credentials')

test_that('`reset_bridge_credentials` sets PhilipsHue options to an empty list', {
    expect_null(getOption('PhilipsHue'))
    expect_true(reset_bridge_credentials())
    expect_equal(getOption('PhilipsHue'), list())
})

test_that('error is thrown with invalid inputs', {
    vars <- c(
        PHILIPS_HUE_BRIDGE_IP = '0.0.0.0',
        PHILIPS_HUE_BRIDGE_USERNAME = 'AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA'
    )

    withr::with_envvar(vars, expect_error(set_bridge_credentials(ip = NULL), '`ip` must be a single character value'))
    withr::with_envvar(vars, expect_error(set_bridge_credentials(ip = NA), '`ip` must be a single character value'))
    withr::with_envvar(vars, expect_error(set_bridge_credentials(ip = pi), '`ip` must be a single character value'))
    withr::with_envvar(vars, expect_error(set_bridge_credentials(ip = letters), '`ip` must be a single character value'))
    withr::with_envvar(vars, expect_error(set_bridge_credentials(ip = iris), '`ip` must be a single character value'))

    withr::with_envvar(vars, expect_error(set_bridge_credentials(username = NULL), '`username` must be a single character value'))
    withr::with_envvar(vars, expect_error(set_bridge_credentials(username = NA), '`username` must be a single character value'))
    withr::with_envvar(vars, expect_error(set_bridge_credentials(username = pi), '`username` must be a single character value'))
    withr::with_envvar(vars, expect_error(set_bridge_credentials(username = letters), '`username` must be a single character value'))
    withr::with_envvar(vars, expect_error(set_bridge_credentials(username = iris), '`username` must be a single character value'))

    withr::with_envvar(vars, expect_error(set_bridge_credentials(ip = 'invalid IP'), '[Ii]nvalid value for `ip`'))
    withr::with_envvar(vars, expect_error(set_bridge_credentials(username = 'invalid username'), '[Ii]nvalid value for `username`'))
})
