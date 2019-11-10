
# LOCAL AUTHENTICATION #########################################################

context('auth_local')

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



# REMOTE AUTHENTICATION ########################################################

context('auth_remote')

test_that('{remote_username_valid():TRUE, access_token_valid():TRUE} sets env vars and returns TRUE', {
    mockery::stub(auth_remote, 'remote_username_valid', TRUE)
    mockery::stub(auth_remote, 'access_token_valid', TRUE)

    ## passing arguments directly
    withr::with_envvar(
        c(
            PHILIPS_HUE_APP_ID = '',
            PHILIPS_HUE_CLIENT_ID = '',
            PHILIPS_HUE_CLIENT_SECRET = '',
            PHILIPS_HUE_BRIDGE_ID = '',
            PHILIPS_HUE_BRIDGE_NAME = '',
            PHILIPS_HUE_BRIDGE_REMOTE_USERNAME = '',
            PHILIPS_HUE_ACCESS_TOKEN = '',
            PHILIPS_HUE_ACCESS_TOKEN_EXP = '',
            PHILIPS_HUE_REFRESH_TOKEN = '',
            PHILIPS_HUE_REFRESH_TOKEN_EXP = ''
        ),
        {
            expect_equal(Sys.getenv('PHILIPS_HUE_BRIDGE_REMOTE_USERNAME'), '')
            expect_equal(Sys.getenv('PHILIPS_HUE_ACCESS_TOKEN'), '')
            expect_equal(Sys.getenv('PHILIPS_HUE_ACCESS_TOKEN_EXP'), '')
            expect_true(auth_remote(username = 'MOCK-USERNAME', access_token = 'MOCK_ACCESS_TOKEN', access_token_exp = '9999-12-31 23:59:59'))
            expect_equal(Sys.getenv('PHILIPS_HUE_BRIDGE_REMOTE_USERNAME'), 'MOCK-USERNAME')
            expect_equal(Sys.getenv('PHILIPS_HUE_ACCESS_TOKEN'), 'MOCK_ACCESS_TOKEN')
            expect_equal(Sys.getenv('PHILIPS_HUE_ACCESS_TOKEN_EXP'), '9999-12-31 23:59:59')
        }
    )

    ## passing arguments through env vars
    withr::with_envvar(
        c(
            PHILIPS_HUE_APP_ID = '',
            PHILIPS_HUE_CLIENT_ID = '',
            PHILIPS_HUE_CLIENT_SECRET = '',
            PHILIPS_HUE_BRIDGE_ID = '',
            PHILIPS_HUE_BRIDGE_NAME = '',
            PHILIPS_HUE_BRIDGE_REMOTE_USERNAME = 'MOCK-USERNAME',
            PHILIPS_HUE_ACCESS_TOKEN = 'MOCK_ACCESS_TOKEN',
            PHILIPS_HUE_ACCESS_TOKEN_EXP = '9999-12-31 23:59:59',
            PHILIPS_HUE_REFRESH_TOKEN = '',
            PHILIPS_HUE_REFRESH_TOKEN_EXP = ''
        ),
        {
            expect_true(auth_remote())
            expect_equal(Sys.getenv('PHILIPS_HUE_BRIDGE_REMOTE_USERNAME'), 'MOCK-USERNAME')
            expect_equal(Sys.getenv('PHILIPS_HUE_ACCESS_TOKEN'), 'MOCK_ACCESS_TOKEN')
            expect_equal(Sys.getenv('PHILIPS_HUE_ACCESS_TOKEN_EXP'), '9999-12-31 23:59:59')
        }
    )
})

test_that('{remote_username_valid():TRUE, access_token_valid():FALSE, client_valid():TRUE} sets env vars', {
    mockery::stub(auth_remote, 'remote_username_valid', TRUE)
    mockery::stub(auth_remote, 'access_token_valid', FALSE)
    mockery::stub(auth_remote, 'client_valid', TRUE)

    ## passing arguments directly
    withr::with_envvar(
        c(
            PHILIPS_HUE_APP_ID = '',
            PHILIPS_HUE_CLIENT_ID = '',
            PHILIPS_HUE_CLIENT_SECRET = '',
            PHILIPS_HUE_BRIDGE_ID = '',
            PHILIPS_HUE_BRIDGE_NAME = '',
            PHILIPS_HUE_BRIDGE_REMOTE_USERNAME = '',
            PHILIPS_HUE_ACCESS_TOKEN = '',
            PHILIPS_HUE_ACCESS_TOKEN_EXP = '',
            PHILIPS_HUE_REFRESH_TOKEN = '',
            PHILIPS_HUE_REFRESH_TOKEN_EXP = ''
        ),
        {
            expect_equal(Sys.getenv('PHILIPS_HUE_CLIENT_ID'), '')
            expect_equal(Sys.getenv('PHILIPS_HUE_CLIENT_SECRET'), '')
            suppressWarnings(auth_remote(client_id = 'MOCK_CLIENT_ID', client_secret = 'MOCK_CLIENT_SECRET'))
            expect_equal(Sys.getenv('PHILIPS_HUE_CLIENT_ID'), 'MOCK_CLIENT_ID')
            expect_equal(Sys.getenv('PHILIPS_HUE_CLIENT_SECRET'), 'MOCK_CLIENT_SECRET')
        }
    )

    ## passing arguments through env vars
    withr::with_envvar(
        c(
            PHILIPS_HUE_APP_ID = '',
            PHILIPS_HUE_CLIENT_ID = 'MOCK_CLIENT_ID',
            PHILIPS_HUE_CLIENT_SECRET = 'MOCK_CLIENT_SECRET',
            PHILIPS_HUE_BRIDGE_ID = '',
            PHILIPS_HUE_BRIDGE_NAME = '',
            PHILIPS_HUE_BRIDGE_REMOTE_USERNAME = '',
            PHILIPS_HUE_ACCESS_TOKEN = '',
            PHILIPS_HUE_ACCESS_TOKEN_EXP = '',
            PHILIPS_HUE_REFRESH_TOKEN = '',
            PHILIPS_HUE_REFRESH_TOKEN_EXP = ''
        ),
        {
            suppressWarnings(auth_remote())
            expect_equal(Sys.getenv('PHILIPS_HUE_CLIENT_ID'), 'MOCK_CLIENT_ID')
            expect_equal(Sys.getenv('PHILIPS_HUE_CLIENT_SECRET'), 'MOCK_CLIENT_SECRET')
        }
    )
})

test_that('{remote_username_valid():TRUE, access_token_valid():FALSE, client_valid():FALSE} warns to set client ID & secret', {
    mockery::stub(auth_remote, 'remote_username_valid', TRUE)
    mockery::stub(auth_remote, 'access_token_valid', FALSE)
    mockery::stub(auth_remote, 'client_valid', FALSE)

    expect_warning(auth_remote(), 'PHILIPS_HUE_CLIENT_ID')
    expect_warning(auth_remote(), 'PHILIPS_HUE_CLIENT_SECRET')
})

test_that('{remote_username_valid():TRUE, access_token_valid():FALSE, refresh_token_valid():TRUE} refreshes token, sets env vars, and returns TRUE', {
    mockery::stub(auth_remote, 'remote_username_valid', TRUE)
    mockery::stub(auth_remote, 'access_token_valid', FALSE)
    mockery::stub(auth_remote, 'refresh_token_valid', TRUE)
    mockery::stub(auth_remote, 'lubridate::now', as.POSIXct('1970-01-01 00:00:00 UTC'))
    mock_refresh_token <- mockery::mock(cycle = TRUE, list(
        access_token = 'MOCK_ACCESS_TOKEN', access_token_expires_in = '10',
        refresh_token = 'MOCK_REFRESH_TOKEN', refresh_token_expires_in = '11'
    ))

    ## passing arguments directly
    withr::with_envvar(
        c(
            PHILIPS_HUE_APP_ID = '',
            PHILIPS_HUE_CLIENT_ID = '',
            PHILIPS_HUE_CLIENT_SECRET = '',
            PHILIPS_HUE_BRIDGE_ID = '',
            PHILIPS_HUE_BRIDGE_NAME = '',
            PHILIPS_HUE_BRIDGE_REMOTE_USERNAME = '',
            PHILIPS_HUE_ACCESS_TOKEN = '',
            PHILIPS_HUE_ACCESS_TOKEN_EXP = '',
            PHILIPS_HUE_REFRESH_TOKEN = '',
            PHILIPS_HUE_REFRESH_TOKEN_EXP = ''
        ),
        {
            expect_equal(Sys.getenv('PHILIPS_HUE_ACCESS_TOKEN'), '')
            expect_equal(Sys.getenv('PHILIPS_HUE_ACCESS_TOKEN_EXP'), '')
            expect_equal(Sys.getenv('PHILIPS_HUE_REFRESH_TOKEN'), '')
            expect_equal(Sys.getenv('PHILIPS_HUE_REFRESH_TOKEN_EXP'), '')
            expect_true(suppressWarnings(with_mock(
                refresh_token = mock_refresh_token,
                {auth_remote(refresh_token = 'MOCK_REFRESH_TOKEN', client_id = 'MOCK_CLIENT_ID', client_secret = 'MOCK_CLIENT_SECRET')}
            )))
            mockery::expect_args(mock_refresh_token, 1, refresh_token = 'MOCK_REFRESH_TOKEN', client_id = 'MOCK_CLIENT_ID', client_secret = 'MOCK_CLIENT_SECRET')
            expect_equal(Sys.getenv('PHILIPS_HUE_ACCESS_TOKEN'), 'MOCK_ACCESS_TOKEN')
            expect_equal(Sys.getenv('PHILIPS_HUE_ACCESS_TOKEN_EXP'), '1970-01-01 00:00:10')
            expect_equal(Sys.getenv('PHILIPS_HUE_REFRESH_TOKEN'), 'MOCK_REFRESH_TOKEN')
            expect_equal(Sys.getenv('PHILIPS_HUE_REFRESH_TOKEN_EXP'), '1970-01-01 00:00:11')
        }
    )

    ## passing arguments through env vars
    withr::with_envvar(
        c(
            PHILIPS_HUE_APP_ID = '',
            PHILIPS_HUE_CLIENT_ID = 'MOCK_CLIENT_ID',
            PHILIPS_HUE_CLIENT_SECRET = 'MOCK_CLIENT_SECRET',
            PHILIPS_HUE_BRIDGE_ID = '',
            PHILIPS_HUE_BRIDGE_NAME = '',
            PHILIPS_HUE_BRIDGE_REMOTE_USERNAME = '',
            PHILIPS_HUE_ACCESS_TOKEN = 'MOCK_ORIG_ACCESS_TOKEN',
            PHILIPS_HUE_ACCESS_TOKEN_EXP = 'MOCK_ORIG_ACCESS_TOKEN_EXP',
            PHILIPS_HUE_REFRESH_TOKEN = 'MOCK_ORIG_REFRESH_TOKEN',
            PHILIPS_HUE_REFRESH_TOKEN_EXP = 'MOCK_ORIG_REFRESH_TOKEN_EXP'
        ),
        {
            expect_equal(Sys.getenv('PHILIPS_HUE_ACCESS_TOKEN'), 'MOCK_ORIG_ACCESS_TOKEN')
            expect_equal(Sys.getenv('PHILIPS_HUE_ACCESS_TOKEN_EXP'), 'MOCK_ORIG_ACCESS_TOKEN_EXP')
            expect_equal(Sys.getenv('PHILIPS_HUE_REFRESH_TOKEN'), 'MOCK_ORIG_REFRESH_TOKEN')
            expect_equal(Sys.getenv('PHILIPS_HUE_REFRESH_TOKEN_EXP'), 'MOCK_ORIG_REFRESH_TOKEN_EXP')
            expect_true(suppressWarnings(with_mock(
                refresh_token = mock_refresh_token,
                {auth_remote()}
            )))
            mockery::expect_args(mock_refresh_token, 2, refresh_token = 'MOCK_ORIG_REFRESH_TOKEN', client_id = 'MOCK_CLIENT_ID', client_secret = 'MOCK_CLIENT_SECRET')
            expect_equal(Sys.getenv('PHILIPS_HUE_ACCESS_TOKEN'), 'MOCK_ACCESS_TOKEN')
            expect_equal(Sys.getenv('PHILIPS_HUE_ACCESS_TOKEN_EXP'), '1970-01-01 00:00:10')
            expect_equal(Sys.getenv('PHILIPS_HUE_REFRESH_TOKEN'), 'MOCK_REFRESH_TOKEN')
            expect_equal(Sys.getenv('PHILIPS_HUE_REFRESH_TOKEN_EXP'), '1970-01-01 00:00:11')
        }
    )
})

test_that('{remote_username_valid():TRUE, access_token_valid():FALSE, refresh_token_valid():FALSE} refreshes token, sets env vars, and returns TRUE', {
    mockery::stub(auth_remote, 'remote_username_valid', TRUE)
    mockery::stub(auth_remote, 'access_token_valid', FALSE)
    mockery::stub(auth_remote, 'refresh_token_valid', FALSE)

    expect_warning(auth_remote(), 'PHILIPS_HUE_REFRESH_TOKEN')
    expect_warning(auth_remote(), 'PHILIPS_HUE_REFRESH_TOKEN_EXP')
    expect_false(suppressWarnings(auth_remote()))
})

test_that('{remote_username_valid():FALSE, interactive():TRUE, *_valid():TRUE} sets env vars', {
    mockery::stub(auth_remote, 'remote_username_valid', FALSE)
    mockery::stub(auth_remote, 'interactive', TRUE)
    mockery::stub(auth_remote, 'app_id_valid', TRUE)
    mockery::stub(auth_remote, 'client_valid', TRUE)
    mockery::stub(auth_remote, 'bridge_valid', TRUE)

    mock_authorize_at <- mockery::mock(cycle = TRUE, 'mock/auth/url')
    mockery::stub(auth_remote, 'readline', 'MOCK_AUTH_CODE')
    mock_request_token <- mockery::mock(cycle = TRUE, list(
        access_token = 'MOCK_ACCESS_TOKEN', access_token_expires_in = '10',
        refresh_token = 'MOCK_REFRESH_TOKEN', refresh_token_expires_in = '11'
    ))
    mock_remote_auth <- mockery::mock(cycle = TRUE, TRUE)
    mock_request_app_username <- mockery::mock(cycle = TRUE, 'MOCK_REMOTE_USERNAME')
    mockery::stub(auth_remote, 'lubridate::now', as.POSIXct('1970-01-01 00:00:00 UTC'))

    ## passing arguments directly
    withr::with_envvar(
        c(
            PHILIPS_HUE_APP_ID = '',
            PHILIPS_HUE_CLIENT_ID = '',
            PHILIPS_HUE_CLIENT_SECRET = '',
            PHILIPS_HUE_BRIDGE_ID = '',
            PHILIPS_HUE_BRIDGE_NAME = '',
            PHILIPS_HUE_BRIDGE_REMOTE_USERNAME = '',
            PHILIPS_HUE_ACCESS_TOKEN = '',
            PHILIPS_HUE_ACCESS_TOKEN_EXP = '',
            PHILIPS_HUE_REFRESH_TOKEN = '',
            PHILIPS_HUE_REFRESH_TOKEN_EXP = ''
        ),
        {
            expect_equal(Sys.getenv('PHILIPS_HUE_APP_ID'), '')
            expect_equal(Sys.getenv('PHILIPS_HUE_CLIENT_ID'), '')
            expect_equal(Sys.getenv('PHILIPS_HUE_CLIENT_SECRET'), '')
            expect_equal(Sys.getenv('PHILIPS_HUE_BRIDGE_ID'), '')
            expect_equal(Sys.getenv('PHILIPS_HUE_BRIDGE_NAME'), '')
            expect_equal(Sys.getenv('PHILIPS_HUE_BRIDGE_REMOTE_USERNAME'), '')
            expect_equal(Sys.getenv('PHILIPS_HUE_ACCESS_TOKEN'), '')
            expect_equal(Sys.getenv('PHILIPS_HUE_ACCESS_TOKEN_EXP'), '')
            expect_equal(Sys.getenv('PHILIPS_HUE_REFRESH_TOKEN'), '')
            expect_equal(Sys.getenv('PHILIPS_HUE_REFRESH_TOKEN_EXP'), '')
            expect_true(
                with_mock(
                    authorize_at = mock_authorize_at,
                    request_token = mock_request_token,
                    remote_auth = mock_remote_auth,
                    request_app_username = mock_request_app_username,
                    {
                        auth_remote(
                            app_id = 'MOCK_PHILIPS_HUE_APP_ID',
                            client_id = 'MOCK_PHILIPS_HUE_CLIENT_ID', client_secret = 'MOCK_PHILIPS_HUE_CLIENT_SECRET',
                            bridge_id = 'MOCK_PHILIPS_HUE_BRIDGE_ID', bridge_name = 'MOCK_PHILIPS_HUE_BRIDGE_NAME',
                            initial_setup = TRUE
                        )
                    }
                )
            )
            mockery::expect_called(mock_authorize_at, 1)
            mockery::expect_args(mock_request_token, 1, auth_code = 'MOCK_AUTH_CODE')
            mockery::expect_args(mock_remote_auth, 1, token = 'MOCK_ACCESS_TOKEN')
            mockery::expect_args(mock_request_app_username, 1, token = 'MOCK_ACCESS_TOKEN')
            expect_equal(Sys.getenv('PHILIPS_HUE_APP_ID'), 'MOCK_PHILIPS_HUE_APP_ID')
            expect_equal(Sys.getenv('PHILIPS_HUE_CLIENT_ID'), 'MOCK_PHILIPS_HUE_CLIENT_ID')
            expect_equal(Sys.getenv('PHILIPS_HUE_CLIENT_SECRET'), 'MOCK_PHILIPS_HUE_CLIENT_SECRET')
            expect_equal(Sys.getenv('PHILIPS_HUE_BRIDGE_ID'), 'MOCK_PHILIPS_HUE_BRIDGE_ID')
            expect_equal(Sys.getenv('PHILIPS_HUE_BRIDGE_NAME'), 'MOCK_PHILIPS_HUE_BRIDGE_NAME')
            expect_equal(Sys.getenv('PHILIPS_HUE_BRIDGE_REMOTE_USERNAME'), 'MOCK_REMOTE_USERNAME')
            expect_equal(Sys.getenv('PHILIPS_HUE_ACCESS_TOKEN'), 'MOCK_ACCESS_TOKEN')
            expect_equal(Sys.getenv('PHILIPS_HUE_ACCESS_TOKEN_EXP'), '1970-01-01 00:00:10')
            expect_equal(Sys.getenv('PHILIPS_HUE_REFRESH_TOKEN'), 'MOCK_REFRESH_TOKEN')
            expect_equal(Sys.getenv('PHILIPS_HUE_REFRESH_TOKEN_EXP'), '1970-01-01 00:00:11')
        }
    )

    ## passing arguments through env vars
    withr::with_envvar(
        c(
            PHILIPS_HUE_APP_ID = 'MOCK_PHILIPS_HUE_APP_ID',
            PHILIPS_HUE_CLIENT_ID = 'MOCK_PHILIPS_HUE_CLIENT_ID',
            PHILIPS_HUE_CLIENT_SECRET = 'MOCK_PHILIPS_HUE_CLIENT_SECRET',
            PHILIPS_HUE_BRIDGE_ID = 'MOCK_PHILIPS_HUE_BRIDGE_ID',
            PHILIPS_HUE_BRIDGE_NAME = 'MOCK_PHILIPS_HUE_BRIDGE_NAME',
            PHILIPS_HUE_BRIDGE_REMOTE_USERNAME = 'MOCK_ORIG_REMOTE_USERNAME',
            PHILIPS_HUE_ACCESS_TOKEN = 'MOCK_ORIG_ACCESS_TOKEN',
            PHILIPS_HUE_ACCESS_TOKEN_EXP = 'MOCK_ORIG_ACCESS_TOKEN_EXP',
            PHILIPS_HUE_REFRESH_TOKEN = 'MOCK_ORIG_REFRESH_TOKEN',
            PHILIPS_HUE_REFRESH_TOKEN_EXP = 'MOCK_ORIG_REFRESH_TOKEN_EXP'
        ),
        {
            expect_true(
                with_mock(
                    authorize_at = mock_authorize_at,
                    request_token = mock_request_token,
                    remote_auth = mock_remote_auth,
                    request_app_username = mock_request_app_username,
                    {auth_remote(initial_setup = TRUE)}
                )
            )
            mockery::expect_called(mock_authorize_at, 2)
            mockery::expect_args(mock_request_token, 2, auth_code = 'MOCK_AUTH_CODE')
            mockery::expect_args(mock_remote_auth, 2, token = 'MOCK_ACCESS_TOKEN')
            mockery::expect_args(mock_request_app_username, 2, token = 'MOCK_ACCESS_TOKEN')
            expect_equal(Sys.getenv('PHILIPS_HUE_APP_ID'), 'MOCK_PHILIPS_HUE_APP_ID')
            expect_equal(Sys.getenv('PHILIPS_HUE_CLIENT_ID'), 'MOCK_PHILIPS_HUE_CLIENT_ID')
            expect_equal(Sys.getenv('PHILIPS_HUE_CLIENT_SECRET'), 'MOCK_PHILIPS_HUE_CLIENT_SECRET')
            expect_equal(Sys.getenv('PHILIPS_HUE_BRIDGE_ID'), 'MOCK_PHILIPS_HUE_BRIDGE_ID')
            expect_equal(Sys.getenv('PHILIPS_HUE_BRIDGE_NAME'), 'MOCK_PHILIPS_HUE_BRIDGE_NAME')
            expect_equal(Sys.getenv('PHILIPS_HUE_BRIDGE_REMOTE_USERNAME'), 'MOCK_REMOTE_USERNAME')
            expect_equal(Sys.getenv('PHILIPS_HUE_ACCESS_TOKEN'), 'MOCK_ACCESS_TOKEN')
            expect_equal(Sys.getenv('PHILIPS_HUE_ACCESS_TOKEN_EXP'), '1970-01-01 00:00:10')
            expect_equal(Sys.getenv('PHILIPS_HUE_REFRESH_TOKEN'), 'MOCK_REFRESH_TOKEN')
            expect_equal(Sys.getenv('PHILIPS_HUE_REFRESH_TOKEN_EXP'), '1970-01-01 00:00:11')
        }
    )
})

test_that('{remote_username_valid():FALSE, interactive():TRUE, *_valid():FALSE} returns FALSE with warnings', {
    mockery::stub(auth_remote, 'remote_username_valid', FALSE)
    mockery::stub(auth_remote, 'interactive', TRUE)

    # app_id_valid(): FALSE
    expect_warning(
        y <- with_mock(
            app_id_valid = function(...) {FALSE},
            client_valid = function(...) {TRUE},
            bridge_valid = function(...) {TRUE},
            .env = 'PhilipsHue',
            auth_remote()
        ),
        'PHILIPS_HUE_APP_ID'
    )
    expect_false(y)
    rm(y)

    # client_valid(): FALSE
    expect_warning(
        y <- with_mock(
            app_id_valid = function(...) {TRUE},
            client_valid = function(...) {FALSE},
            bridge_valid = function(...) {TRUE},
            .env = 'PhilipsHue',
            auth_remote()
        ),
        'PHILIPS_HUE_CLIENT_ID'
    )
    expect_false(y)
    rm(y)

    expect_warning(
        y <- with_mock(
            app_id_valid = function(...) {TRUE},
            client_valid = function(...) {FALSE},
            bridge_valid = function(...) {TRUE},
            .env = 'PhilipsHue',
            auth_remote()
        ),
        'PHILIPS_HUE_CLIENT_SECRET'
    )
    expect_false(y)
    rm(y)

    # bridge_valid(): FALSE
    expect_warning(
        y <- with_mock(
            app_id_valid = function(...) {TRUE},
            client_valid = function(...) {TRUE},
            bridge_valid = function(...) {FALSE},
            .env = 'PhilipsHue',
            auth_remote()
        ),
        'PHILIPS_HUE_BRIDGE_ID'
    )
    expect_false(y)
    rm(y)

    expect_warning(
        y <- with_mock(
            app_id_valid = function(...) {TRUE},
            client_valid = function(...) {TRUE},
            bridge_valid = function(...) {FALSE},
            .env = 'PhilipsHue',
            auth_remote()
        ),
        'PHILIPS_HUE_BRIDGE_NAME'
    )
    expect_false(y)
    rm(y)

    # app_id_valid(): FALSE, client_valid(): FALSE
    expect_warning(
        y <- with_mock(
            app_id_valid = function(...) {FALSE},
            client_valid = function(...) {FALSE},
            bridge_valid = function(...) {FALSE},
            .env = 'PhilipsHue',
            auth_remote()
        ),
        'PHILIPS_HUE_APP_ID'
    )
    expect_false(y)
    rm(y)

    expect_warning(
        y <- with_mock(
            app_id_valid = function(...) {FALSE},
            client_valid = function(...) {FALSE},
            bridge_valid = function(...) {FALSE},
            .env = 'PhilipsHue',
            auth_remote()
        ),
        'PHILIPS_HUE_CLIENT_ID'
    )
    expect_false(y)
    rm(y)

    expect_warning(
        y <- with_mock(
            app_id_valid = function(...) {FALSE},
            client_valid = function(...) {FALSE},
            bridge_valid = function(...) {FALSE},
            .env = 'PhilipsHue',
            auth_remote()
        ),
        'PHILIPS_HUE_CLIENT_SECRET'
    )
    expect_false(y)
    rm(y)

    expect_warning(
        y <- with_mock(
            app_id_valid = function(...) {FALSE},
            client_valid = function(...) {FALSE},
            bridge_valid = function(...) {FALSE},
            .env = 'PhilipsHue',
            auth_remote()
        ),
        'PHILIPS_HUE_BRIDGE_ID'
    )
    expect_false(y)
    rm(y)

    expect_warning(
        y <- with_mock(
            app_id_valid = function(...) {FALSE},
            client_valid = function(...) {FALSE},
            bridge_valid = function(...) {FALSE},
            .env = 'PhilipsHue',
            auth_remote()
        ),
        'PHILIPS_HUE_BRIDGE_NAME'
    )
    expect_false(y)
    rm(y)
})

test_that('{remote_username_valid():FALSE, interactive():TRUE, *_valid():FALSE} returns FALSE with warnings', {
    mockery::stub(auth_remote, 'remote_username_valid', FALSE)
    mockery::stub(auth_remote, 'interactive', FALSE)

    expect_warning(y <- auth_remote(), 'PHILIPS_HUE_BRIDGE_REMOTE_USERNAME')
    expect_false(y)

    expect_warning(y <- auth_remote(initial_setup = FALSE), 'PHILIPS_HUE_BRIDGE_REMOTE_USERNAME')
    expect_false(y)

    expect_warning(y <- auth_remote(initial_setup = TRUE), 'PHILIPS_HUE_BRIDGE_REMOTE_USERNAME')
    expect_false(y)
})



# REMOTE AUTH SEQUENCE #########################################################

context('authorize_at')

test_that('helper creates URL to request access to bridge from client', {
    mockery::stub(authorize_at, 'digest::digest', 'mock_hash')
    expect_identical(
        authorize_at('client_id', 'app id', 'device_id', 'device name'),
        'https://api.meethue.com/oauth2/auth?clientid=client_id&appid=app%20id&deviceid=device_id&devicename=device%20name&state=mock_hash&response_type=code'
    )
})



context('request_token')

test_that('`request_token` throws error and prints parsed content if POST fails', {
    mockery::stub(request_token, 'httr::POST', list())
    mockery::stub(request_token, 'httr::status_code', 'mock status code')
    mockery::stub(request_token, 'httr::content', list(errors = c('mock error 1')))

    expect_error(request_token('mock auth code'), 'mock status code')
    expect_error(request_token('mock auth code'), 'mock error 1')
})

test_that('`request_token` throws error and prints parsed content if POST fails due to httr::content failure', {
    mockery::stub(request_token, 'httr::POST', list())
    mockery::stub(request_token, 'httr::status_code', 'mock status code')

    expect_error(request_token('mock auth code'), 'mock status code')
    expect_error(
        with_mock(
            content = function(...) {stop('httr::content mock error')},
            .env = 'httr',
            request_token('mock auth code')
        ),
        'httr::content mock error'
    )
})

test_that('`request_token` returns token if POST succeeds', {
    mockery::stub(request_token, 'httr::POST', list())
    mockery::stub(request_token, 'httr::status_code', 200)
    mockery::stub(request_token, 'httr::content', list(access_token = 'mock access token'))

    expect_identical(request_token('mock auth code'), list(access_token = 'mock access token'))
})



context('remote_auth')

test_that('`remote_auth` throws error and prints parsed content if POST fails', {
    mockery::stub(remote_auth, 'httr::PUT', list())
    mockery::stub(remote_auth, 'httr::status_code', 'mock status code')
    mockery::stub(remote_auth, 'httr::content', list(errors = c('mock error 1')))

    expect_error(remote_auth('mock access token'), 'mock status code')
    expect_error(remote_auth('mock access token'), 'mock error 1')
})

test_that('`remote_auth` throws error and prints parsed content if POST fails due to httr::content failure', {
    mockery::stub(remote_auth, 'httr::PUT', list())
    mockery::stub(remote_auth, 'httr::status_code', 'mock status code')

    expect_error(remote_auth('mock access token'), 'mock status code')
    expect_error(
        with_mock(
            content = function(...) {stop('httr::content mock error')},
            .env = 'httr',
            remote_auth('mock access token')
        ),
        'httr::content mock error'
    )
})

test_that('`remote_auth` returns TRUE if POST succeeds', {
    mockery::stub(remote_auth, 'httr::POST', list())
    mockery::stub(remote_auth, 'httr::status_code', 200)

    expect_true(remote_auth('mock access token'))
})



context('request_app_username')

test_that('`request_app_username` throws error and prints parsed content if POST fails', {
    mockery::stub(request_app_username, 'httr::POST', list())
    mockery::stub(request_app_username, 'httr::status_code', 'mock status code')
    mockery::stub(request_app_username, 'httr::content', list(errors = c('mock error 1')))

    expect_error(request_app_username('mock access token', 'mock_app_id'), 'mock status code')
    expect_error(request_app_username('mock access token', 'mock_app_id'), 'mock error 1')
})

test_that('`request_app_username` throws error and prints parsed content if POST fails due to httr::content failure', {
    mockery::stub(request_app_username, 'httr::POST', list())
    mockery::stub(request_app_username, 'httr::status_code', 'mock status code')

    expect_error(request_app_username('mock access token', 'mock_app_id'), 'mock status code')
    expect_error(
        with_mock(
            content = function(...) {stop('httr::content mock error')},
            .env = 'httr',
            request_app_username('mock access token', 'mock_app_id')
        ),
        'httr::content mock error'
    )
})

test_that('`request_app_username` returns username if POST succeeds', {
    mockery::stub(request_app_username, 'httr::POST', list())
    mockery::stub(request_app_username, 'httr::status_code', 200)
    mockery::stub(request_app_username, 'httr::content', list(list(success = list(username = 'mock username'))))

    expect_identical(request_app_username('mock access token', 'mock_app_id'), 'mock username')
})



context('refresh_token')

test_that('`refresh_token` throws error and prints parsed content if POST fails', {
    mockery::stub(refresh_token, 'httr::POST', list())
    mockery::stub(refresh_token, 'httr::status_code', 'mock status code')
    mockery::stub(refresh_token, 'httr::content', list(errors = c('mock error 1')))

    expect_error(refresh_token('mock refresh token', 'mock_app_id'), 'mock status code')
    expect_error(refresh_token('mock refresh token', 'mock_app_id'), 'mock error 1')
})

test_that('`refresh_token` throws error and prints parsed content if POST fails due to httr::content failure', {
    mockery::stub(refresh_token, 'httr::POST', list())
    mockery::stub(refresh_token, 'httr::status_code', 'mock status code')

    expect_error(refresh_token('mock refresh token', 'mock_app_id'), 'mock status code')
    expect_error(
        with_mock(
            content = function(...) {stop('httr::content mock error')},
            .env = 'httr',
            refresh_token('mock refresh token', 'mock_app_id')
        ),
        'httr::content mock error'
    )
})

test_that('`refresh_token` returns token if POST succeeds', {
    mockery::stub(refresh_token, 'httr::POST', list())
    mockery::stub(refresh_token, 'httr::status_code', 200)
    mockery::stub(refresh_token, 'httr::content', list(access_token = 'mock access token'))

    expect_identical(refresh_token('mock refresh code'), list(access_token = 'mock access token'))
})



# AUTH RESETTERS ###############################################################

context('reset_auth')

test_that('`reset_auth` resets local and/or remote auth', {
    withr::with_envvar(
        c(
            PHILIPS_HUE_BRIDGE_IP = 'MOCK_BRIDGE_IP',
            PHILIPS_HUE_BRIDGE_USERNAME = 'MOCK_BRIDGE_USERNAME',
            PHILIPS_HUE_APP_ID = 'MOCK_APP_ID',
            PHILIPS_HUE_CLIENT_ID = 'MOCK_CLIENT_ID',
            PHILIPS_HUE_CLIENT_SECRET = 'MOCK_CLIENT_SECRET',
            PHILIPS_HUE_BRIDGE_ID = 'MOCK_BRIDGE_ID',
            PHILIPS_HUE_BRIDGE_NAME = 'MOCK_BRIDGE_NAME',
            PHILIPS_HUE_BRIDGE_REMOTE_USERNAME = 'MOCK_BRIDGE_REMOTE_USERNAME',
            PHILIPS_HUE_ACCESS_TOKEN = 'MOCK_ACCESS_TOKEN',
            PHILIPS_HUE_ACCESS_TOKEN_EXP = 'MOCK_ACCESS_TOKEN_EXP',
            PHILIPS_HUE_REFRESH_TOKEN = 'MOCK_REFRESH_TOKEN',
            PHILIPS_HUE_REFRESH_TOKEN_EXP = 'MOCK_REFRESH_TOKEN_EXP'
        ),
        {
            expect_equal(Sys.getenv('PHILIPS_HUE_BRIDGE_IP'), 'MOCK_BRIDGE_IP')
            expect_equal(Sys.getenv('PHILIPS_HUE_BRIDGE_USERNAME'), 'MOCK_BRIDGE_USERNAME')
            expect_equal(Sys.getenv('PHILIPS_HUE_APP_ID'), 'MOCK_APP_ID')
            expect_equal(Sys.getenv('PHILIPS_HUE_CLIENT_ID'), 'MOCK_CLIENT_ID')
            expect_equal(Sys.getenv('PHILIPS_HUE_CLIENT_SECRET'), 'MOCK_CLIENT_SECRET')
            expect_equal(Sys.getenv('PHILIPS_HUE_BRIDGE_ID'), 'MOCK_BRIDGE_ID')
            expect_equal(Sys.getenv('PHILIPS_HUE_BRIDGE_NAME'), 'MOCK_BRIDGE_NAME')
            expect_equal(Sys.getenv('PHILIPS_HUE_BRIDGE_REMOTE_USERNAME'), 'MOCK_BRIDGE_REMOTE_USERNAME')
            expect_equal(Sys.getenv('PHILIPS_HUE_ACCESS_TOKEN'), 'MOCK_ACCESS_TOKEN')
            expect_equal(Sys.getenv('PHILIPS_HUE_ACCESS_TOKEN_EXP'), 'MOCK_ACCESS_TOKEN_EXP')
            expect_equal(Sys.getenv('PHILIPS_HUE_REFRESH_TOKEN'), 'MOCK_REFRESH_TOKEN')
            expect_equal(Sys.getenv('PHILIPS_HUE_REFRESH_TOKEN_EXP'), 'MOCK_REFRESH_TOKEN_EXP')
            expect_true(reset_auth())
            expect_equal(Sys.getenv('PHILIPS_HUE_BRIDGE_IP'), '')
            expect_equal(Sys.getenv('PHILIPS_HUE_BRIDGE_USERNAME'), '')
            expect_equal(Sys.getenv('PHILIPS_HUE_APP_ID'), '')
            expect_equal(Sys.getenv('PHILIPS_HUE_CLIENT_ID'), '')
            expect_equal(Sys.getenv('PHILIPS_HUE_CLIENT_SECRET'), '')
            expect_equal(Sys.getenv('PHILIPS_HUE_BRIDGE_ID'), '')
            expect_equal(Sys.getenv('PHILIPS_HUE_BRIDGE_NAME'), '')
            expect_equal(Sys.getenv('PHILIPS_HUE_BRIDGE_REMOTE_USERNAME'), '')
            expect_equal(Sys.getenv('PHILIPS_HUE_ACCESS_TOKEN'), '')
            expect_equal(Sys.getenv('PHILIPS_HUE_ACCESS_TOKEN_EXP'), '')
            expect_equal(Sys.getenv('PHILIPS_HUE_REFRESH_TOKEN'), '')
            expect_equal(Sys.getenv('PHILIPS_HUE_REFRESH_TOKEN_EXP'), '')
        }
    )

    withr::with_envvar(
        c(
            PHILIPS_HUE_BRIDGE_IP = 'MOCK_BRIDGE_IP',
            PHILIPS_HUE_BRIDGE_USERNAME = 'MOCK_BRIDGE_USERNAME',
            PHILIPS_HUE_APP_ID = 'MOCK_APP_ID',
            PHILIPS_HUE_CLIENT_ID = 'MOCK_CLIENT_ID',
            PHILIPS_HUE_CLIENT_SECRET = 'MOCK_CLIENT_SECRET',
            PHILIPS_HUE_BRIDGE_ID = 'MOCK_BRIDGE_ID',
            PHILIPS_HUE_BRIDGE_NAME = 'MOCK_BRIDGE_NAME',
            PHILIPS_HUE_BRIDGE_REMOTE_USERNAME = 'MOCK_BRIDGE_REMOTE_USERNAME',
            PHILIPS_HUE_ACCESS_TOKEN = 'MOCK_ACCESS_TOKEN',
            PHILIPS_HUE_ACCESS_TOKEN_EXP = 'MOCK_ACCESS_TOKEN_EXP',
            PHILIPS_HUE_REFRESH_TOKEN = 'MOCK_REFRESH_TOKEN',
            PHILIPS_HUE_REFRESH_TOKEN_EXP = 'MOCK_REFRESH_TOKEN_EXP'
        ),
        {
            expect_equal(Sys.getenv('PHILIPS_HUE_BRIDGE_IP'), 'MOCK_BRIDGE_IP')
            expect_equal(Sys.getenv('PHILIPS_HUE_BRIDGE_USERNAME'), 'MOCK_BRIDGE_USERNAME')
            expect_equal(Sys.getenv('PHILIPS_HUE_APP_ID'), 'MOCK_APP_ID')
            expect_equal(Sys.getenv('PHILIPS_HUE_CLIENT_ID'), 'MOCK_CLIENT_ID')
            expect_equal(Sys.getenv('PHILIPS_HUE_CLIENT_SECRET'), 'MOCK_CLIENT_SECRET')
            expect_equal(Sys.getenv('PHILIPS_HUE_BRIDGE_ID'), 'MOCK_BRIDGE_ID')
            expect_equal(Sys.getenv('PHILIPS_HUE_BRIDGE_NAME'), 'MOCK_BRIDGE_NAME')
            expect_equal(Sys.getenv('PHILIPS_HUE_BRIDGE_REMOTE_USERNAME'), 'MOCK_BRIDGE_REMOTE_USERNAME')
            expect_equal(Sys.getenv('PHILIPS_HUE_ACCESS_TOKEN'), 'MOCK_ACCESS_TOKEN')
            expect_equal(Sys.getenv('PHILIPS_HUE_ACCESS_TOKEN_EXP'), 'MOCK_ACCESS_TOKEN_EXP')
            expect_equal(Sys.getenv('PHILIPS_HUE_REFRESH_TOKEN'), 'MOCK_REFRESH_TOKEN')
            expect_equal(Sys.getenv('PHILIPS_HUE_REFRESH_TOKEN_EXP'), 'MOCK_REFRESH_TOKEN_EXP')
            expect_true(reset_auth(local = TRUE, remote = FALSE))
            expect_equal(Sys.getenv('PHILIPS_HUE_BRIDGE_IP'), '')
            expect_equal(Sys.getenv('PHILIPS_HUE_BRIDGE_USERNAME'), '')
            expect_equal(Sys.getenv('PHILIPS_HUE_APP_ID'), 'MOCK_APP_ID')
            expect_equal(Sys.getenv('PHILIPS_HUE_CLIENT_ID'), 'MOCK_CLIENT_ID')
            expect_equal(Sys.getenv('PHILIPS_HUE_CLIENT_SECRET'), 'MOCK_CLIENT_SECRET')
            expect_equal(Sys.getenv('PHILIPS_HUE_BRIDGE_ID'), 'MOCK_BRIDGE_ID')
            expect_equal(Sys.getenv('PHILIPS_HUE_BRIDGE_NAME'), 'MOCK_BRIDGE_NAME')
            expect_equal(Sys.getenv('PHILIPS_HUE_BRIDGE_REMOTE_USERNAME'), 'MOCK_BRIDGE_REMOTE_USERNAME')
            expect_equal(Sys.getenv('PHILIPS_HUE_ACCESS_TOKEN'), 'MOCK_ACCESS_TOKEN')
            expect_equal(Sys.getenv('PHILIPS_HUE_ACCESS_TOKEN_EXP'), 'MOCK_ACCESS_TOKEN_EXP')
            expect_equal(Sys.getenv('PHILIPS_HUE_REFRESH_TOKEN'), 'MOCK_REFRESH_TOKEN')
            expect_equal(Sys.getenv('PHILIPS_HUE_REFRESH_TOKEN_EXP'), 'MOCK_REFRESH_TOKEN_EXP')
        }
    )

    withr::with_envvar(
        c(
            PHILIPS_HUE_BRIDGE_IP = 'MOCK_BRIDGE_IP',
            PHILIPS_HUE_BRIDGE_USERNAME = 'MOCK_BRIDGE_USERNAME',
            PHILIPS_HUE_APP_ID = 'MOCK_APP_ID',
            PHILIPS_HUE_CLIENT_ID = 'MOCK_CLIENT_ID',
            PHILIPS_HUE_CLIENT_SECRET = 'MOCK_CLIENT_SECRET',
            PHILIPS_HUE_BRIDGE_ID = 'MOCK_BRIDGE_ID',
            PHILIPS_HUE_BRIDGE_NAME = 'MOCK_BRIDGE_NAME',
            PHILIPS_HUE_BRIDGE_REMOTE_USERNAME = 'MOCK_BRIDGE_REMOTE_USERNAME',
            PHILIPS_HUE_ACCESS_TOKEN = 'MOCK_ACCESS_TOKEN',
            PHILIPS_HUE_ACCESS_TOKEN_EXP = 'MOCK_ACCESS_TOKEN_EXP',
            PHILIPS_HUE_REFRESH_TOKEN = 'MOCK_REFRESH_TOKEN',
            PHILIPS_HUE_REFRESH_TOKEN_EXP = 'MOCK_REFRESH_TOKEN_EXP'
        ),
        {
            expect_equal(Sys.getenv('PHILIPS_HUE_BRIDGE_IP'), 'MOCK_BRIDGE_IP')
            expect_equal(Sys.getenv('PHILIPS_HUE_BRIDGE_USERNAME'), 'MOCK_BRIDGE_USERNAME')
            expect_equal(Sys.getenv('PHILIPS_HUE_APP_ID'), 'MOCK_APP_ID')
            expect_equal(Sys.getenv('PHILIPS_HUE_CLIENT_ID'), 'MOCK_CLIENT_ID')
            expect_equal(Sys.getenv('PHILIPS_HUE_CLIENT_SECRET'), 'MOCK_CLIENT_SECRET')
            expect_equal(Sys.getenv('PHILIPS_HUE_BRIDGE_ID'), 'MOCK_BRIDGE_ID')
            expect_equal(Sys.getenv('PHILIPS_HUE_BRIDGE_NAME'), 'MOCK_BRIDGE_NAME')
            expect_equal(Sys.getenv('PHILIPS_HUE_BRIDGE_REMOTE_USERNAME'), 'MOCK_BRIDGE_REMOTE_USERNAME')
            expect_equal(Sys.getenv('PHILIPS_HUE_ACCESS_TOKEN'), 'MOCK_ACCESS_TOKEN')
            expect_equal(Sys.getenv('PHILIPS_HUE_ACCESS_TOKEN_EXP'), 'MOCK_ACCESS_TOKEN_EXP')
            expect_equal(Sys.getenv('PHILIPS_HUE_REFRESH_TOKEN'), 'MOCK_REFRESH_TOKEN')
            expect_equal(Sys.getenv('PHILIPS_HUE_REFRESH_TOKEN_EXP'), 'MOCK_REFRESH_TOKEN_EXP')
            expect_true(reset_auth(local = FALSE, remote = TRUE))
            expect_equal(Sys.getenv('PHILIPS_HUE_BRIDGE_IP'), 'MOCK_BRIDGE_IP')
            expect_equal(Sys.getenv('PHILIPS_HUE_BRIDGE_USERNAME'), 'MOCK_BRIDGE_USERNAME')
            expect_equal(Sys.getenv('PHILIPS_HUE_APP_ID'), '')
            expect_equal(Sys.getenv('PHILIPS_HUE_CLIENT_ID'), '')
            expect_equal(Sys.getenv('PHILIPS_HUE_CLIENT_SECRET'), '')
            expect_equal(Sys.getenv('PHILIPS_HUE_BRIDGE_ID'), '')
            expect_equal(Sys.getenv('PHILIPS_HUE_BRIDGE_NAME'), '')
            expect_equal(Sys.getenv('PHILIPS_HUE_BRIDGE_REMOTE_USERNAME'), '')
            expect_equal(Sys.getenv('PHILIPS_HUE_ACCESS_TOKEN'), '')
            expect_equal(Sys.getenv('PHILIPS_HUE_ACCESS_TOKEN_EXP'), '')
            expect_equal(Sys.getenv('PHILIPS_HUE_REFRESH_TOKEN'), '')
            expect_equal(Sys.getenv('PHILIPS_HUE_REFRESH_TOKEN_EXP'), '')
        }
    )
})



# VALIDATORS ###################################################################

context('app_id_valid')

test_that('app_id_valid() returns TRUE with valid inputs', {
    expect_true(app_id_valid('MOCK APP ID'))
})

test_that('app_id_valid() returns FALSE with invalid inputs', {
    expect_false(app_id_valid(NULL))
    expect_false(app_id_valid(NA))
    expect_false(app_id_valid(''))
    expect_false(app_id_valid(pi))
    expect_false(app_id_valid(letters))
    expect_false(app_id_valid(iris))
})



context('client_valid')

test_that('client_valid() returns TRUE with valid inputs', {
    expect_true(client_valid('MOCK_CLIENT_ID', 'MOCK_CLIENT_SECRET'))
})

test_that('refresh_token_valid() returns FALSE with invalid inputs', {
    expect_false(client_valid(NULL, 'MOCK_CLIENT_SECRET'))
    expect_false(client_valid(NA, 'MOCK_CLIENT_SECRET'))
    expect_false(client_valid('', 'MOCK_CLIENT_SECRET'))
    expect_false(client_valid(pi, 'MOCK_CLIENT_SECRET'))
    expect_false(client_valid(letters, 'MOCK_CLIENT_SECRET'))
    expect_false(client_valid(iris, 'MOCK_CLIENT_SECRET'))
    expect_false(client_valid('invalid token', 'MOCK_CLIENT_SECRET'))

    expect_false(client_valid('MOCK_CLIENT_ID', NULL))
    expect_false(client_valid('MOCK_CLIENT_ID', NA))
    expect_false(client_valid('MOCK_CLIENT_ID', ''))
    expect_false(client_valid('MOCK_CLIENT_ID', pi))
    expect_false(client_valid('MOCK_CLIENT_ID', letters))
    expect_false(client_valid('MOCK_CLIENT_ID', iris))
    expect_false(client_valid('MOCK_CLIENT_ID', 'invalid token'))
})



context('remote_username_valid')

test_that('remote_username_valid() returns TRUE with valid inputs', {
    expect_true(remote_username_valid('MOCK-USERNAME'))
})

test_that('remote_username_valid() returns FALSE with invalid inputs', {
    expect_false(remote_username_valid(NULL))
    expect_false(remote_username_valid(NA))
    expect_false(remote_username_valid(''))
    expect_false(remote_username_valid(pi))
    expect_false(remote_username_valid(letters))
    expect_false(remote_username_valid(iris))
})



context('access_token_valid')

test_that('access_token_valid() returns TRUE with valid inputs', {
    expect_true(access_token_valid('MOCK_ACCESS_TOKEN', '9999-12-31 23:59:59'))
    expect_true(access_token_valid('MOCK_ACCESS_TOKEN', as.POSIXct('9999-12-31 23:59:59')))
})

test_that('access_token_valid() returns FALSE with invalid inputs', {
    expect_false(access_token_valid(NULL, '9999-12-31 23:59:59'))
    expect_false(access_token_valid(NA, '9999-12-31 23:59:59'))
    expect_false(access_token_valid('', '9999-12-31 23:59:59'))
    expect_false(access_token_valid(pi, '9999-12-31 23:59:59'))
    expect_false(access_token_valid(letters, '9999-12-31 23:59:59'))
    expect_false(access_token_valid(iris, '9999-12-31 23:59:59'))
    expect_false(access_token_valid('invalid token', '9999-12-31 23:59:59'))

    expect_false(access_token_valid('MOCK_ACCESS_TOKEN', NULL))
    expect_false(access_token_valid('MOCK_ACCESS_TOKEN', NA))
    expect_false(access_token_valid('MOCK_ACCESS_TOKEN', ''))
    expect_false(access_token_valid('MOCK_ACCESS_TOKEN', pi))
    expect_false(access_token_valid('MOCK_ACCESS_TOKEN', letters))
    expect_false(access_token_valid('MOCK_ACCESS_TOKEN', iris))
    expect_false(access_token_valid('MOCK_ACCESS_TOKEN', 'invalid exp'))
    expect_false(access_token_valid('MOCK_ACCESS_TOKEN', '1970-01-01 00:00:00'))
})



context('refresh_token_valid')

test_that('refresh_token_valid() returns TRUE with valid inputs', {
    expect_true(refresh_token_valid('MOCK_REFRESH_TOKEN', '9999-12-31 23:59:59'))
    expect_true(refresh_token_valid('MOCK_REFRESH_TOKEN', as.POSIXct('9999-12-31 23:59:59')))
})

test_that('refresh_token_valid() returns FALSE with invalid inputs', {
    expect_false(refresh_token_valid(NULL, '9999-12-31 23:59:59'))
    expect_false(refresh_token_valid(NA, '9999-12-31 23:59:59'))
    expect_false(refresh_token_valid('', '9999-12-31 23:59:59'))
    expect_false(refresh_token_valid(pi, '9999-12-31 23:59:59'))
    expect_false(refresh_token_valid(letters, '9999-12-31 23:59:59'))
    expect_false(refresh_token_valid(iris, '9999-12-31 23:59:59'))
    expect_false(refresh_token_valid('invalid token', '9999-12-31 23:59:59'))

    expect_false(refresh_token_valid('MOCK_REFRESH_TOKEN', NULL))
    expect_false(refresh_token_valid('MOCK_REFRESH_TOKEN', NA))
    expect_false(refresh_token_valid('MOCK_REFRESH_TOKEN', ''))
    expect_false(refresh_token_valid('MOCK_REFRESH_TOKEN', pi))
    expect_false(refresh_token_valid('MOCK_REFRESH_TOKEN', letters))
    expect_false(refresh_token_valid('MOCK_REFRESH_TOKEN', iris))
    expect_false(refresh_token_valid('MOCK_REFRESH_TOKEN', 'invalid exp'))
    expect_false(refresh_token_valid('MOCK_REFRESH_TOKEN', '1970-01-01 00:00:00'))
})



context('bridge_valid')

test_that('bridge_valid() returns TRUE with valid inputs', {
    expect_true(bridge_valid('MOCK_BRIDGE_ID', 'MOCK BRIDGE NAME'))
})

test_that('bridge_valid() returns FALSE with invalid inputs', {
    expect_false(bridge_valid(NULL, 'MOCK BRIDGE NAME'))
    expect_false(bridge_valid(NA, 'MOCK BRIDGE NAME'))
    expect_false(bridge_valid('', 'MOCK BRIDGE NAME'))
    expect_false(bridge_valid(pi, 'MOCK BRIDGE NAME'))
    expect_false(bridge_valid(letters, 'MOCK BRIDGE NAME'))
    expect_false(bridge_valid(iris, 'MOCK BRIDGE NAME'))
    expect_false(bridge_valid('invalid bridge ID', 'MOCK BRIDGE NAME'))

    expect_false(bridge_valid('MOCK_BRIDGE_ID', NULL))
    expect_false(bridge_valid('MOCK_BRIDGE_ID', NA))
    expect_false(bridge_valid('MOCK_BRIDGE_ID', ''))
    expect_false(bridge_valid('MOCK_BRIDGE_ID', pi))
    expect_false(bridge_valid('MOCK_BRIDGE_ID', letters))
    expect_false(bridge_valid('MOCK_BRIDGE_ID', iris))
})



context('has_local_auth')

test_that('has_local_auth() returns TRUE with proper env vars and FALSE otherwise', {
    withr::with_envvar(
        c(
            PHILIPS_HUE_BRIDGE_IP = '0.0.0.0',
            PHILIPS_HUE_BRIDGE_USERNAME = 'AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA'
        ),
        expect_true(has_local_auth())
    )

    withr::with_envvar(
        c(
            PHILIPS_HUE_BRIDGE_IP = '',
            PHILIPS_HUE_BRIDGE_USERNAME = 'AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA'
        ),
        expect_false(has_local_auth())
    )

    withr::with_envvar(
        c(
            PHILIPS_HUE_BRIDGE_IP = '0.0.0.0',
            PHILIPS_HUE_BRIDGE_USERNAME = ''
        ),
        expect_false(has_local_auth())
    )

    withr::with_envvar(
        c(
            PHILIPS_HUE_BRIDGE_IP = 'invalid IP address',
            PHILIPS_HUE_BRIDGE_USERNAME = 'AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA'
        ),
        expect_false(has_local_auth())
    )

    withr::with_envvar(
        c(
            PHILIPS_HUE_BRIDGE_IP = '0.0.0.0',
            PHILIPS_HUE_BRIDGE_USERNAME = 'invalid username'
        ),
        expect_false(has_local_auth())
    )
})



context('has_remote_auth')

test_that('has_remote_auth() returns TRUE with proper env vars and FALSE otherwise', {
    withr::with_envvar(
        c(
            PHILIPS_HUE_APP_ID = 'MOCK_APP_ID',
            PHILIPS_HUE_CLIENT_ID = 'MOCK_CLIENT_ID',
            PHILIPS_HUE_CLIENT_SECRET = 'MOCK_CLIENT_SECRET',
            PHILIPS_HUE_BRIDGE_ID = 'MOCK_BRIDGE_ID',
            PHILIPS_HUE_BRIDGE_NAME = 'MOCK BRIDGE NAME',
            PHILIPS_HUE_BRIDGE_REMOTE_USERNAME = 'MOCK_BRIDGE_REMOTE_USERNAME',
            PHILIPS_HUE_ACCESS_TOKEN = 'MOCK_ACCESS_TOKEN',
            PHILIPS_HUE_ACCESS_TOKEN_EXP = '9999-01-01 00:00:00',
            PHILIPS_HUE_REFRESH_TOKEN = 'MOCK_REFRESH_TOKEN',
            PHILIPS_HUE_REFRESH_TOKEN_EXP = '9999-12-31 23:59:59'
        ),
        expect_true(has_remote_auth())
    )

    withr::with_envvar(
        c(
            PHILIPS_HUE_APP_ID = '',
            PHILIPS_HUE_CLIENT_ID = 'MOCK_CLIENT_ID',
            PHILIPS_HUE_CLIENT_SECRET = 'MOCK_CLIENT_SECRET',
            PHILIPS_HUE_BRIDGE_ID = 'MOCK_BRIDGE_ID',
            PHILIPS_HUE_BRIDGE_NAME = 'MOCK BRIDGE NAME',
            PHILIPS_HUE_BRIDGE_REMOTE_USERNAME = 'MOCK_BRIDGE_REMOTE_USERNAME',
            PHILIPS_HUE_ACCESS_TOKEN = 'MOCK_ACCESS_TOKEN',
            PHILIPS_HUE_ACCESS_TOKEN_EXP = '9999-01-01 00:00:00',
            PHILIPS_HUE_REFRESH_TOKEN = 'MOCK_REFRESH_TOKEN',
            PHILIPS_HUE_REFRESH_TOKEN_EXP = '9999-12-31 23:59:59'
        ),
        expect_false(has_remote_auth())
    )

    withr::with_envvar(
        c(
            PHILIPS_HUE_APP_ID = 'MOCK_APP_ID',
            PHILIPS_HUE_CLIENT_ID = '',
            PHILIPS_HUE_CLIENT_SECRET = 'MOCK_CLIENT_SECRET',
            PHILIPS_HUE_BRIDGE_ID = 'MOCK_BRIDGE_ID',
            PHILIPS_HUE_BRIDGE_NAME = 'MOCK BRIDGE NAME',
            PHILIPS_HUE_BRIDGE_REMOTE_USERNAME = 'MOCK_BRIDGE_REMOTE_USERNAME',
            PHILIPS_HUE_ACCESS_TOKEN = 'MOCK_ACCESS_TOKEN',
            PHILIPS_HUE_ACCESS_TOKEN_EXP = '9999-01-01 00:00:00',
            PHILIPS_HUE_REFRESH_TOKEN = 'MOCK_REFRESH_TOKEN',
            PHILIPS_HUE_REFRESH_TOKEN_EXP = '9999-12-31 23:59:59'
        ),
        expect_false(has_remote_auth())
    )

    withr::with_envvar(
        c(
            PHILIPS_HUE_APP_ID = 'MOCK_APP_ID',
            PHILIPS_HUE_CLIENT_ID = 'invalid MOCK_CLIENT_ID',
            PHILIPS_HUE_CLIENT_SECRET = 'MOCK_CLIENT_SECRET',
            PHILIPS_HUE_BRIDGE_ID = 'MOCK_BRIDGE_ID',
            PHILIPS_HUE_BRIDGE_NAME = 'MOCK BRIDGE NAME',
            PHILIPS_HUE_BRIDGE_REMOTE_USERNAME = 'MOCK_BRIDGE_REMOTE_USERNAME',
            PHILIPS_HUE_ACCESS_TOKEN = 'MOCK_ACCESS_TOKEN',
            PHILIPS_HUE_ACCESS_TOKEN_EXP = '9999-01-01 00:00:00',
            PHILIPS_HUE_REFRESH_TOKEN = 'MOCK_REFRESH_TOKEN',
            PHILIPS_HUE_REFRESH_TOKEN_EXP = '9999-12-31 23:59:59'
        ),
        expect_false(has_remote_auth())
    )

    withr::with_envvar(
        c(
            PHILIPS_HUE_APP_ID = 'MOCK_APP_ID',
            PHILIPS_HUE_CLIENT_ID = 'MOCK_CLIENT_ID',
            PHILIPS_HUE_CLIENT_SECRET = '',
            PHILIPS_HUE_BRIDGE_ID = 'MOCK_BRIDGE_ID',
            PHILIPS_HUE_BRIDGE_NAME = 'MOCK BRIDGE NAME',
            PHILIPS_HUE_BRIDGE_REMOTE_USERNAME = 'MOCK_BRIDGE_REMOTE_USERNAME',
            PHILIPS_HUE_ACCESS_TOKEN = 'MOCK_ACCESS_TOKEN',
            PHILIPS_HUE_ACCESS_TOKEN_EXP = '9999-01-01 00:00:00',
            PHILIPS_HUE_REFRESH_TOKEN = 'MOCK_REFRESH_TOKEN',
            PHILIPS_HUE_REFRESH_TOKEN_EXP = '9999-12-31 23:59:59'
        ),
        expect_false(has_remote_auth())
    )

    withr::with_envvar(
        c(
            PHILIPS_HUE_APP_ID = 'MOCK_APP_ID',
            PHILIPS_HUE_CLIENT_ID = 'MOCK_CLIENT_ID',
            PHILIPS_HUE_CLIENT_SECRET = 'invalid MOCK_CLIENT_SECRET',
            PHILIPS_HUE_BRIDGE_ID = 'MOCK_BRIDGE_ID',
            PHILIPS_HUE_BRIDGE_NAME = 'MOCK BRIDGE NAME',
            PHILIPS_HUE_BRIDGE_REMOTE_USERNAME = 'MOCK_BRIDGE_REMOTE_USERNAME',
            PHILIPS_HUE_ACCESS_TOKEN = 'MOCK_ACCESS_TOKEN',
            PHILIPS_HUE_ACCESS_TOKEN_EXP = '9999-01-01 00:00:00',
            PHILIPS_HUE_REFRESH_TOKEN = 'MOCK_REFRESH_TOKEN',
            PHILIPS_HUE_REFRESH_TOKEN_EXP = '9999-12-31 23:59:59'
        ),
        expect_false(has_remote_auth())
    )

    withr::with_envvar(
        c(
            PHILIPS_HUE_APP_ID = 'MOCK_APP_ID',
            PHILIPS_HUE_CLIENT_ID = 'MOCK_CLIENT_ID',
            PHILIPS_HUE_CLIENT_SECRET = 'MOCK_CLIENT_SECRET',
            PHILIPS_HUE_BRIDGE_ID = '',
            PHILIPS_HUE_BRIDGE_NAME = 'MOCK BRIDGE NAME',
            PHILIPS_HUE_BRIDGE_REMOTE_USERNAME = 'MOCK_BRIDGE_REMOTE_USERNAME',
            PHILIPS_HUE_ACCESS_TOKEN = 'MOCK_ACCESS_TOKEN',
            PHILIPS_HUE_ACCESS_TOKEN_EXP = '9999-01-01 00:00:00',
            PHILIPS_HUE_REFRESH_TOKEN = 'MOCK_REFRESH_TOKEN',
            PHILIPS_HUE_REFRESH_TOKEN_EXP = '9999-12-31 23:59:59'
        ),
        expect_false(has_remote_auth())
    )

    withr::with_envvar(
        c(
            PHILIPS_HUE_APP_ID = 'MOCK_APP_ID',
            PHILIPS_HUE_CLIENT_ID = 'MOCK_CLIENT_ID',
            PHILIPS_HUE_CLIENT_SECRET = 'MOCK_CLIENT_SECRET',
            PHILIPS_HUE_BRIDGE_ID = 'invalid MOCK_BRIDGE_ID',
            PHILIPS_HUE_BRIDGE_NAME = 'MOCK BRIDGE NAME',
            PHILIPS_HUE_BRIDGE_REMOTE_USERNAME = 'MOCK_BRIDGE_REMOTE_USERNAME',
            PHILIPS_HUE_ACCESS_TOKEN = 'MOCK_ACCESS_TOKEN',
            PHILIPS_HUE_ACCESS_TOKEN_EXP = '9999-01-01 00:00:00',
            PHILIPS_HUE_REFRESH_TOKEN = 'MOCK_REFRESH_TOKEN',
            PHILIPS_HUE_REFRESH_TOKEN_EXP = '9999-12-31 23:59:59'
        ),
        expect_false(has_remote_auth())
    )

    withr::with_envvar(
        c(
            PHILIPS_HUE_APP_ID = 'MOCK_APP_ID',
            PHILIPS_HUE_CLIENT_ID = 'MOCK_CLIENT_ID',
            PHILIPS_HUE_CLIENT_SECRET = 'MOCK_CLIENT_SECRET',
            PHILIPS_HUE_BRIDGE_ID = 'MOCK_BRIDGE_ID',
            PHILIPS_HUE_BRIDGE_NAME = '',
            PHILIPS_HUE_BRIDGE_REMOTE_USERNAME = 'MOCK_BRIDGE_REMOTE_USERNAME',
            PHILIPS_HUE_ACCESS_TOKEN = 'MOCK_ACCESS_TOKEN',
            PHILIPS_HUE_ACCESS_TOKEN_EXP = '9999-01-01 00:00:00',
            PHILIPS_HUE_REFRESH_TOKEN = 'MOCK_REFRESH_TOKEN',
            PHILIPS_HUE_REFRESH_TOKEN_EXP = '9999-12-31 23:59:59'
        ),
        expect_false(has_remote_auth())
    )

    withr::with_envvar(
        c(
            PHILIPS_HUE_APP_ID = 'MOCK_APP_ID',
            PHILIPS_HUE_CLIENT_ID = 'MOCK_CLIENT_ID',
            PHILIPS_HUE_CLIENT_SECRET = 'MOCK_CLIENT_SECRET',
            PHILIPS_HUE_BRIDGE_ID = 'MOCK_BRIDGE_ID',
            PHILIPS_HUE_BRIDGE_NAME = 'MOCK BRIDGE NAME',
            PHILIPS_HUE_BRIDGE_REMOTE_USERNAME = '',
            PHILIPS_HUE_ACCESS_TOKEN = 'MOCK_ACCESS_TOKEN',
            PHILIPS_HUE_ACCESS_TOKEN_EXP = '9999-01-01 00:00:00',
            PHILIPS_HUE_REFRESH_TOKEN = 'MOCK_REFRESH_TOKEN',
            PHILIPS_HUE_REFRESH_TOKEN_EXP = '9999-12-31 23:59:59'
        ),
        expect_false(has_remote_auth())
    )

    withr::with_envvar(
        c(
            PHILIPS_HUE_APP_ID = 'MOCK_APP_ID',
            PHILIPS_HUE_CLIENT_ID = 'MOCK_CLIENT_ID',
            PHILIPS_HUE_CLIENT_SECRET = 'MOCK_CLIENT_SECRET',
            PHILIPS_HUE_BRIDGE_ID = 'MOCK_BRIDGE_ID',
            PHILIPS_HUE_BRIDGE_NAME = 'MOCK BRIDGE NAME',
            PHILIPS_HUE_BRIDGE_REMOTE_USERNAME = 'invalid MOCK_BRIDGE_REMOTE_USERNAME',
            PHILIPS_HUE_ACCESS_TOKEN = 'MOCK_ACCESS_TOKEN',
            PHILIPS_HUE_ACCESS_TOKEN_EXP = '9999-01-01 00:00:00',
            PHILIPS_HUE_REFRESH_TOKEN = 'MOCK_REFRESH_TOKEN',
            PHILIPS_HUE_REFRESH_TOKEN_EXP = '9999-12-31 23:59:59'
        ),
        expect_false(has_remote_auth())
    )

    withr::with_envvar(
        c(
            PHILIPS_HUE_APP_ID = 'MOCK_APP_ID',
            PHILIPS_HUE_CLIENT_ID = 'MOCK_CLIENT_ID',
            PHILIPS_HUE_CLIENT_SECRET = 'MOCK_CLIENT_SECRET',
            PHILIPS_HUE_BRIDGE_ID = 'MOCK_BRIDGE_ID',
            PHILIPS_HUE_BRIDGE_NAME = 'MOCK BRIDGE NAME',
            PHILIPS_HUE_BRIDGE_REMOTE_USERNAME = 'MOCK_BRIDGE_REMOTE_USERNAME',
            PHILIPS_HUE_ACCESS_TOKEN = '',
            PHILIPS_HUE_ACCESS_TOKEN_EXP = '9999-01-01 00:00:00',
            PHILIPS_HUE_REFRESH_TOKEN = 'MOCK_REFRESH_TOKEN',
            PHILIPS_HUE_REFRESH_TOKEN_EXP = '9999-12-31 23:59:59'
        ),
        expect_false(has_remote_auth())
    )

    withr::with_envvar(
        c(
            PHILIPS_HUE_APP_ID = 'MOCK_APP_ID',
            PHILIPS_HUE_CLIENT_ID = 'MOCK_CLIENT_ID',
            PHILIPS_HUE_CLIENT_SECRET = 'MOCK_CLIENT_SECRET',
            PHILIPS_HUE_BRIDGE_ID = 'MOCK_BRIDGE_ID',
            PHILIPS_HUE_BRIDGE_NAME = 'MOCK BRIDGE NAME',
            PHILIPS_HUE_BRIDGE_REMOTE_USERNAME = 'MOCK_BRIDGE_REMOTE_USERNAME',
            PHILIPS_HUE_ACCESS_TOKEN = 'invalid MOCK_ACCESS_TOKEN',
            PHILIPS_HUE_ACCESS_TOKEN_EXP = '9999-01-01 00:00:00',
            PHILIPS_HUE_REFRESH_TOKEN = 'MOCK_REFRESH_TOKEN',
            PHILIPS_HUE_REFRESH_TOKEN_EXP = '9999-12-31 23:59:59'
        ),
        expect_false(has_remote_auth())
    )

    withr::with_envvar(
        c(
            PHILIPS_HUE_APP_ID = 'MOCK_APP_ID',
            PHILIPS_HUE_CLIENT_ID = 'MOCK_CLIENT_ID',
            PHILIPS_HUE_CLIENT_SECRET = 'MOCK_CLIENT_SECRET',
            PHILIPS_HUE_BRIDGE_ID = 'MOCK_BRIDGE_ID',
            PHILIPS_HUE_BRIDGE_NAME = 'MOCK BRIDGE NAME',
            PHILIPS_HUE_BRIDGE_REMOTE_USERNAME = 'MOCK_BRIDGE_REMOTE_USERNAME',
            PHILIPS_HUE_ACCESS_TOKEN = 'MOCK_ACCESS_TOKEN',
            PHILIPS_HUE_ACCESS_TOKEN_EXP = '',
            PHILIPS_HUE_REFRESH_TOKEN = 'MOCK_REFRESH_TOKEN',
            PHILIPS_HUE_REFRESH_TOKEN_EXP = '9999-12-31 23:59:59'
        ),
        expect_false(has_remote_auth())
    )

    withr::with_envvar(
        c(
            PHILIPS_HUE_APP_ID = 'MOCK_APP_ID',
            PHILIPS_HUE_CLIENT_ID = 'MOCK_CLIENT_ID',
            PHILIPS_HUE_CLIENT_SECRET = 'MOCK_CLIENT_SECRET',
            PHILIPS_HUE_BRIDGE_ID = 'MOCK_BRIDGE_ID',
            PHILIPS_HUE_BRIDGE_NAME = 'MOCK BRIDGE NAME',
            PHILIPS_HUE_BRIDGE_REMOTE_USERNAME = 'MOCK_BRIDGE_REMOTE_USERNAME',
            PHILIPS_HUE_ACCESS_TOKEN = 'MOCK_ACCESS_TOKEN',
            PHILIPS_HUE_ACCESS_TOKEN_EXP = 'invalid 9999-01-01 00:00:00',
            PHILIPS_HUE_REFRESH_TOKEN = 'MOCK_REFRESH_TOKEN',
            PHILIPS_HUE_REFRESH_TOKEN_EXP = '9999-12-31 23:59:59'
        ),
        expect_false(has_remote_auth())
    )

    withr::with_envvar(
        c(
            PHILIPS_HUE_APP_ID = 'MOCK_APP_ID',
            PHILIPS_HUE_CLIENT_ID = 'MOCK_CLIENT_ID',
            PHILIPS_HUE_CLIENT_SECRET = 'MOCK_CLIENT_SECRET',
            PHILIPS_HUE_BRIDGE_ID = 'MOCK_BRIDGE_ID',
            PHILIPS_HUE_BRIDGE_NAME = 'MOCK BRIDGE NAME',
            PHILIPS_HUE_BRIDGE_REMOTE_USERNAME = 'MOCK_BRIDGE_REMOTE_USERNAME',
            PHILIPS_HUE_ACCESS_TOKEN = 'MOCK_ACCESS_TOKEN',
            PHILIPS_HUE_ACCESS_TOKEN_EXP = '9999-01-01 00:00:00',
            PHILIPS_HUE_REFRESH_TOKEN = '',
            PHILIPS_HUE_REFRESH_TOKEN_EXP = '9999-12-31 23:59:59'
        ),
        expect_false(has_remote_auth())
    )

    withr::with_envvar(
        c(
            PHILIPS_HUE_APP_ID = 'MOCK_APP_ID',
            PHILIPS_HUE_CLIENT_ID = 'MOCK_CLIENT_ID',
            PHILIPS_HUE_CLIENT_SECRET = 'MOCK_CLIENT_SECRET',
            PHILIPS_HUE_BRIDGE_ID = 'MOCK_BRIDGE_ID',
            PHILIPS_HUE_BRIDGE_NAME = 'MOCK BRIDGE NAME',
            PHILIPS_HUE_BRIDGE_REMOTE_USERNAME = 'MOCK_BRIDGE_REMOTE_USERNAME',
            PHILIPS_HUE_ACCESS_TOKEN = 'MOCK_ACCESS_TOKEN',
            PHILIPS_HUE_ACCESS_TOKEN_EXP = '9999-01-01 00:00:00',
            PHILIPS_HUE_REFRESH_TOKEN = 'invalid MOCK_REFRESH_TOKEN',
            PHILIPS_HUE_REFRESH_TOKEN_EXP = '9999-12-31 23:59:59'
        ),
        expect_false(has_remote_auth())
    )

    withr::with_envvar(
        c(
            PHILIPS_HUE_APP_ID = 'MOCK_APP_ID',
            PHILIPS_HUE_CLIENT_ID = 'MOCK_CLIENT_ID',
            PHILIPS_HUE_CLIENT_SECRET = 'MOCK_CLIENT_SECRET',
            PHILIPS_HUE_BRIDGE_ID = 'MOCK_BRIDGE_ID',
            PHILIPS_HUE_BRIDGE_NAME = 'MOCK BRIDGE NAME',
            PHILIPS_HUE_BRIDGE_REMOTE_USERNAME = 'MOCK_BRIDGE_REMOTE_USERNAME',
            PHILIPS_HUE_ACCESS_TOKEN = 'MOCK_ACCESS_TOKEN',
            PHILIPS_HUE_ACCESS_TOKEN_EXP = '9999-01-01 00:00:00',
            PHILIPS_HUE_REFRESH_TOKEN = 'MOCK_REFRESH_TOKEN',
            PHILIPS_HUE_REFRESH_TOKEN_EXP = ''
        ),
        expect_false(has_remote_auth())
    )

    withr::with_envvar(
        c(
            PHILIPS_HUE_APP_ID = 'MOCK_APP_ID',
            PHILIPS_HUE_CLIENT_ID = 'MOCK_CLIENT_ID',
            PHILIPS_HUE_CLIENT_SECRET = 'MOCK_CLIENT_SECRET',
            PHILIPS_HUE_BRIDGE_ID = 'MOCK_BRIDGE_ID',
            PHILIPS_HUE_BRIDGE_NAME = 'MOCK BRIDGE NAME',
            PHILIPS_HUE_BRIDGE_REMOTE_USERNAME = 'MOCK_BRIDGE_REMOTE_USERNAME',
            PHILIPS_HUE_ACCESS_TOKEN = 'MOCK_ACCESS_TOKEN',
            PHILIPS_HUE_ACCESS_TOKEN_EXP = '9999-01-01 00:00:00',
            PHILIPS_HUE_REFRESH_TOKEN = 'MOCK_REFRESH_TOKEN',
            PHILIPS_HUE_REFRESH_TOKEN_EXP = 'invalid 9999-12-31 23:59:59'
        ),
        expect_false(has_remote_auth())
    )
})
