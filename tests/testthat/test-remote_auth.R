
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



context('app_username')

test_that('`app_username` throws error and prints parsed content if POST fails', {
    mockery::stub(app_username, 'httr::POST', list())
    mockery::stub(app_username, 'httr::status_code', 'mock status code')
    mockery::stub(app_username, 'httr::content', list(errors = c('mock error 1')))

    expect_error(app_username('mock access token', 'mock_app_id'), 'mock status code')
    expect_error(app_username('mock access token', 'mock_app_id'), 'mock error 1')
})

test_that('`app_username` returns username if POST succeeds', {
    mockery::stub(app_username, 'httr::POST', list())
    mockery::stub(app_username, 'httr::status_code', 200)
    mockery::stub(app_username, 'httr::content', list(list(success = list(username = 'mock username'))))

    expect_identical(app_username('mock access token', 'mock_app_id'), 'mock username')
})
