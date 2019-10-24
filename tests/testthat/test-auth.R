
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

test_that('`refresh_token` returns token if POST succeeds', {
    mockery::stub(refresh_token, 'httr::POST', list())
    mockery::stub(refresh_token, 'httr::status_code', 200)
    mockery::stub(refresh_token, 'httr::content', list(access_token = 'mock access token'))

    expect_identical(refresh_token('mock refresh code'), list(access_token = 'mock access token'))
})
